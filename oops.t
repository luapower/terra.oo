--[[

	OOP DSL for Terra.
	Written by Cosmin Apreutesei. Public Domain.

]]

if not ... then require'oops_test'; return end

setfenv(1, require'low')

local class = {}; setmetatable(class, class)

--parser ---------------------------------------------------------------------

function class:__call()
	local self = {fields = {}, methods = {}, __index = self}
	return setmetatable(self, self)
end

function class:parse(lex)
	lex:expect'class'
	--class name
	self.name = lex:expect(lex.name).value
	--super class
	if lex:nextif':' then
		self.super_expr = lex:luaexpr()
	end
	--declarations
	while not lex:nextif'end' do
		if lex:matches'before' or lex:matches'after' then --before/after hooks
			local hook = lex:next().type
			local name = lex:expect(lex.name).value
			local m = self:parse_method_def(lex, name)
			m.hook = hook
			add(self.methods, m)
		else --field or method definition
			local name = lex:expect(lex.name).value
			if lex:nextif':' then --field definition
				local field = {name = name}
				field.type_expr = lex:luaexpr()
				if lex:nextif'=' then
					field.val_expr = lex:luaexpr()
				end
				add(self.fields, field)
			elseif lex:matches'(' then --method definition
				local m = self:parse_method_def(lex, name)
				add(self.methods, m)
				self.methods[m.name] = m
			else
				lex:error'field or method definition expected'
			end
		end
	end
end

function class:parse_method_def(lex, name, hook)
	lex:expect'('
	local m = {name = name, args = {}, ismacro = false}
	while not lex:nextif')' do --method args
		--TODO: support escapes in arg list (splice per Terra semantics)
		local arg = {}
		arg.name = lex:expect(lex.name).value
		if lex:nextif':' then
			arg.type_expr = lex:luaexpr()
		else
			m.ismacro = true
		end
		lex:nextif','
		add(m.args, arg)
	end
	if lex:nextif':' then --method return type (optional)
		m.ret_type_expr = lex:luaexpr()
	end
	m.body_stmts = m.ismacro and lex:luastats() or lex:terrastats()
	lex:expect'end'
	return m
end

--compiler -------------------------------------------------------------------

function class:compile_field(f, env)
	add(self.T.entries, {field = f.name, type = f.type_expr(env)})
end

function class:build_method(m, env, arg_syms)
	local fenv = setmetatable({}, {__index = env})
	local self_sym = symbol(&self.T, 'self')
	fenv.self = self_sym
	local arg_syms = arg_syms or {}
	add(arg_syms, self_sym)
	for i,arg in ipairs(m.args) do
		add(arg_syms, symbol(arg.type, arg.name))
	end
	for i,arg_sym in ipairs(arg_syms) do
		fenv[arg_sym.displayname] = arg_sym
	end
	local body_quote = m.body_stmts(fenv); m.body_stmts = nil
	local ret_type
	if m.ret_type_expr then
		ret_type = m.ret_type_expr(env); m.ret_type_expr = nil
	end
	local func
	if ret_type then
		func = terra([arg_syms]): ret_type
			[ body_quote ]
		end
	else --autodetect return type
		func = terra([arg_syms])
			[ body_quote ]
		end
		ret_type = func:gettype().returntype
	end
	return func, ret_type, arg_syms
end

function class:build_macro(m, env)
	local fenv = setmetatable({}, {__index = env})
	local func = macro(function(self, ...)
		fenv.self = self
		for i,arg in ipairs(m.args) do
			fenv[arg.name] = select(i, ...)
		end
		return m.body_stmts(fenv) or quote end
	end)
	return func
end

function class:compile_method(m, env)

	local func, ret_type, arg_syms = self:build_method(m, env)

	--attach "before" hooks
	if m.before then
		for i,m in ipairs(m.before) do
			local before_func = self:build_method(m, env)
			func = terra([arg_syms]): ret_type
				before_func([arg_syms])
				return func([arg_syms])
			end
		end
	end

	--attach "after" hooks
	if m.after then
		for i,m in ipairs(m.after) do
			--make `retval` available to "after" hooks
			local retval_sym = symbol(ret_type, 'retval')
			local after_func, after_ret_type = self:build_method(m, env, {retval_sym})
			if after_ret_type:isunit() then --pass-by-reference.
				func = terra([arg_syms]) : ret_type
					var [retval_sym] = func([arg_syms])
					after_func(&[retval_sym], [arg_syms])
					return [retval_sym]
				end
			else --hook returns a value: pass-by-value and return it back.
				func = terra([arg_syms]) : ret_type
					var [retval_sym] = func([arg_syms])
					return after_func([retval_sym], [arg_syms])
				end
			end
		end
	end

	--add method to vtable and wrap it.
	self.vtable[self.vt_index] = func
	self.vtable_names[self.vt_index] = m.name
	local func_type = func:gettype()
	func = terra([arg_syms]): ret_type
		var fn = [&func_type]([arg_syms[1]].__vtable[self.vt_index-1])
		return fn([arg_syms])
	end
	self.vt_index = self.vt_index + 1

	return self:add_method(m.name, func)
end

function class:add_method(name, func)
	local existing_func = self.T.methods[name]
	if terralib.type(existing_func) == 'overloadedterrafunction' then
		existing_func:adddefinition(func)
	elseif existing_func then
		func = overload(name, {existing_func, func})
	end
	self.T.methods[name] = func
	return func
end

function class:compile_macro(m, env)
	local existing_func = T.methods[m.name]
	assert(not existing_func, 'NYI: overriding with a macro')
	local func = build_macro(m, env)
	self.T.methods[m.name] = func
	return func
end

function class:compile_method_or_macro(m, env)
	if m.compiled then return end
	m.compiled = true
	if m.ismacro then
		return self:compile_macro(m, env)
	else
		return self:compile_method(m, env, index)
	end
end

--implicit cast to avoid wrapping inherited methods just to re-type self.
local function __cast(from, to, exp)
	if from:ispointer() and to:ispointer() then
		local to_ptr = to
		local from, to = from.type, to.type
		while from ~= to and from ~= nil do
			from = from.metamethods.self.super
		end
		if from ~= nil then
			return `[to_ptr](exp)
		end
	end
	error(tostring(from)..' is not a subtype of '..tostring(to))
end

function class:compile(env)

	local T = newstruct(self.name)
	self.T = T
	T.metamethods.class = self

	self.vt_index = 1
	self.vtable = {}
	self.vtable_names = {}

	if self.super_expr then
		self.super = self.super_expr(env); self.super_expr = nil
	end
	if self.super then
		--copy super's fields
		local super = self.super.metamethods.cls
		for i,entry in ipairs(super.T.entries) do
			T.entries[i] = entry
		end
		--import methods super's virtual methods
		for vt_index, name in ipairs(super.vtable_names) do
			local func = super.vtable[vt_index]
			self:add_method(name, func)
		end
	else
		add(T.entries, {field = '__vtable', type = &&opaque})
	end

	for i,f in ipairs(self.fields) do
		self:compile_field(T, f, env)
	end

	--compute a distinct method signature identity for a class.
	local signature = tuples()
	local function method_signature(m)
		local t = {m.name}
		if not m.ismacro then
			for i,arg in ipairs(m.args) do
				add(t, arg.type)
			end
		end
		return signature(unpack(t))
	end

	local methods = {} --{sig -> m; m1, ...}
	for i,m in ipairs(self.methods) do

		--eval Lua expressions for argument types and return value
		for _,arg in ipairs(m.args) do
			if arg.type_expr then
				arg.type = arg.type_expr(env); arg.type_expr = nil
			end
		end
		if arg.ret_type_expr then
			arg.ret_type = arg.ret_type_expr(env); arg.ret_type_expr = nil
		end

		--group together all before/after hooks on the same method signature.
		local sig = method_signature(m)
		local m0 = methods[sig]
		if m0 then
			assert(m.hook, 'trying to replace method '..m.name)
			add(attr(m0, m.hook), m); m.hook = nil
		else
			methods[sig] = m
			add(methods, m)
		end
	end

	T.metamethods.__methodmissing = macro(function(name, obj, ...)

		--compile dependent methods on-demand.
		--find the right method based on its signature.
		local arg_types = {}
		for i=1,select('#',...) do
			arg_types[i] = (select(i,...)):gettype()
		end
		local sig = signature(name, unpack(arg_types))
		local m = methods[sig]
		if m then
			local func = self:compile_method_or_macro(m, env)
			local args = {...}
			return `func(&obj, [args])
		end

		if select('#', ...) == 0 then --default getter
			return `obj.[name]
		elseif select('#', ...) == 1 then --default setter
			local arg = ...
			return quote obj.[name] = arg end
		end
	end)

	for _,m in ipairs(methods) do
		self:compile_method_or_macro(m, env)
	end

	local vtable = constant(`arrayof([&opaque], [self.vtable]))

	terra T:init()
		self.__vtable = vtable
		return self
	end
	T.metamethods.__cast = __cast

	return T
end

--language extension ---------------------------------------------------------

local oopslang = {
	name = 'oopslang';
	entrypoints = {'class'};
	keywords = {'before', 'after'};
}

function oopslang:statement(lex)
	local cls = class()
	cls:parse(lex)
	local ctor = function(get_env)
		return cls:compile(get_env())
	end
	return ctor, {cls.name} -- statement: name = ctor(get_env))
end

oopslang.expression = oopslang.statement
oopslang.localstatement = oopslang.statement

return oopslang

--[[

	OOP DSL for Terra.
	Written by Cosmin Apreutesei. Public Domain.

]]

if not ... then require'oops_test'; return end

setfenv(1, require'low')

--parser/compiler for `class <C>[:<S>] ... end` decls.
local class = {}; setmetatable(class, class)

--parser ---------------------------------------------------------------------

function class:__call()
	local self = {__index = self}
	return setmetatable(self, self)
end

function class:parse(lex)
	--header: `class <name>[:<super_expr>]`
	lex:expect'class'
	self.ast = {fields = {}, methods = {}}
	self.name = lex:expect(lex.name).value
	if lex:nextif':' then
		self.ast.super_expr = lex:luaexpr()
	end
	--definitions: <field_def> | <method_def> | <macro_def> | <hook_def>
	while not lex:nextif'end' do
		if lex:matches'before' or lex:matches'after' or lex:matches'over' then --<hook_def>
			--before|after|over <method_def>|<macro_def>
			local hook = lex:next().type
			local name = lex:expect(lex.name).value
			local m = self:parse_method_or_macro_def(lex, name)
			m.hook = hook
			add(self.ast.methods, m)
		else
			local name = lex:expect(lex.name).value
			if lex:nextif':' then --<field_def>
				local f = self:parse_field_def(lex, name)
				add(self.ast.fields, field)
			elseif lex:matches'(' then --<method_def> | <macro_def>
				local m = self:parse_method_or_macro_def(lex, name)
				add(self.ast.methods, m)
			else
				lex:error'field or method definition expected'
			end
		end
	end
end

--<field_name>: field_type_expr [=init_const_expr]
function class:parse_field_def(lex, name)
	local f = {name = name}
	f.type_expr = lex:luaexpr()
	if lex:nextif'=' then
		f.val_expr = lex:luaexpr()
	end
	return f
end

--method: <name>(arg_name: arg_type_expr, ...)[: ret_type_expr]
--macro:  <name>(arg_name, ...)
function class:parse_method_or_macro_def(lex, name, hook)
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
	add(self.T.entries, {
		field = f.name,
		type = f.type_expr(env),
		private = f:starts'_',
	})
end

--generate a unique id for each distinct method signature in the context
--of an entire class hierarchy. returns the same id for the same signature.
--this is needed for identifying the right method to override since all
--methods are overloadable as well.
function class:init_signature_function(super)
	if super then
		self.signature = super.signature
	else
		local signature = memoize(function(...) return {} end)
		function self:signature(...) return signature(...) end
	end
end

--terra methods are overloadable by distinct sequence of arg types.
function class:terra_signature(name, arg_types)
	return self:signature(name, unpack(arg_types))
end

--macros are overloadable by number of arguments alone.
function class:macro_signature(name, nargs)
	return self:signature(name, nargs)
end

--eval Lua expressions for argument and return types and compute
--the method signature.
function class:type_method(ast, env)
	local arg_types = {}
	ast.arg_syms = {symbol(&self.T, 'self')}
	for _,arg in ipairs(ast.args) do
		if arg.type_expr then
			local arg_type = arg.type_expr(env); arg.type_expr = nil
			add(arg_types, arg_type)
			add(ast.arg_syms, symbol(arg_type, arg.name))
		end
	end
	if ast.ret_type_expr then
		ast.ret_type = ast.ret_type_expr(env); ast.ret_type_expr = nil
	end
	if ast.ismacro then
		ast.sig = self:macro_signature(ast.name, #ast.args)
	else
		ast.sig = self:terra_signature(ast.name, arg_types)
	end
end

local nothing = quote end
local function mkterra(arg_syms, ret_type, body_quote)
	body_quote = body_quote or nothing
	return ret_type
		and terra([arg_syms]): ret_type body_quote end
		 or terra([arg_syms]) body_quote end
end

--pluck the arg symbols from a defined function.
local function func_arg_syms(func)
	local syms = {}
	for i,var_ in ipairs(func.definition.parameters) do
		add(syms, var_.symbol)
	end
	return syms
end

--build a terra function in a lexical environment. arg_syms are additional
--symbols to inject in the function environment and pass as first args.
function class:compile_terra(ast, env, arg_syms)
	local fenv = setmetatable({}, {__index = env})
	local self_sym = symbol(&self.T, 'self')
	fenv.self = self_sym
	local arg_syms = arg_syms and extend({}, arg_syms, ast.arg_syms) or ast.arg_syms
	for i,arg_sym in ipairs(arg_syms) do
		fenv[arg_sym.displayname] = arg_sym
	end
	local body_quote = ast.body_stmts(fenv); ast.body_stmts = nil
	local func = mkterra(arg_syms, ast.ret_type, body_quote)
	return func, arg_syms
end

--build a macro in a lexical environment.
function class:compile_macro(ast, env)
	local fenv = setmetatable({}, {__index = env})
	local func = macro(function(self, ...)
		fenv.self = self
		for i,arg in ipairs(ast.args) do
			fenv[arg.name] = select(i, ...)
		end
		return ast.body_stmts(fenv)
	end)
	return func
end

class.compile_hook = {} --{where -> compile}

--attach a "before" hook to a method.
function class.compile_hook:before(ast, env, func)
	local ret_type = func:gettype().returntype
	local before_func, arg_syms = self:compile_method(ast, env)
	return terra([arg_syms]): ret_type
		before_func([arg_syms])
		return func([arg_syms])
	end
end

--attach an "after" hook to a method.
function class.compile_hook:after(ast, env, func)
	local ret_type = func:gettype().returntype
	--make `retval` available to "after" hooks
	local retval_sym = symbol(ret_type, 'retval')
	local after_func, arg_syms = self:compile_method(ast, env, {retval_sym})
	table.remove(arg_syms, 1)
	if after_func:gettype().returntype:isunit() then
		return terra([arg_syms]): ret_type
			var [retval_sym] = func([arg_syms])
			after_func([retval_sym], [arg_syms])
			return [retval_sym]
		end
	else --hook returns a value: return it back.
		return terra([arg_syms]): ret_type
			return after_func(func([arg_syms]), [arg_syms])
		end
	end
end

--override a method.
function class.compile_hook:over(ast, env, func)
	local func_type = func:gettype()
	local inherited_sym = symbol(&func_type, 'inherited')
	local over_func, arg_syms = self:compile_method(ast, env, {inherited_sym})
	table.remove(arg_syms, 1)
	return terra([arg_syms]): func_type.returntype
		var [inherited_sym] = func
		return over_func([inherited_sym], [arg_syms])
	end
end

function class:compile_method(ast, env, arg_syms)
	if ast.compiling then
		--recursive reference, make a stub to be replaced later.
		local arg_syms = arg_syms and extend({}, arg_syms, ast.arg_syms) or ast.arg_syms
		ast.stub = ast.stub or mkterra(arg_syms, ast.ret_type)
		return ast.stub
	end
	if ast.func then --already compiled
		return ast.func
	end
	ast.compiling = true --catch recursive references from __methodmissing
	local func
	if self.ismacro then
		func, arg_syms = self:compile_macro(ast, env, arg_syms)
	else
		func, arg_syms = self:compile_terra(ast, env, arg_syms)
	end
	if ast.hooks then
		assert(not ast.hook) --hooks can't have hooks
		for i,ast in ipairs(ast.hooks) do
			func = self.compile_hook[ast.hook](self, ast, env, func)
		end
	end
	ast.compiling = false
	if ast.stub then
		if ast.stub:gettype().returntype ~= func:gettype().returntype then
			error('must provide return type for method '..ast.name)
		end
		ast.stub:resetdefinition(func)
		func = ast.stub; ast.stub = nil
	end
	if not ast.hook then --publish it if it's a base method (not a hook)
		self:_addmethod(ast.name, ast.sig, func)
	end
	ast.func = func
	return func, arg_syms
end

function class:make_virtual(sig, func)
	local vidx = self.vindex[sig]
	if vidx then --replace implementation
		self.vtable[vidx] = func
	else --allocate a new slot and method stub
		vidx = #self.vtable + 1
		self.vtable[vidx] = func
		self.vindex[sig] = vidx
		local func_type = func:gettype()
		local ret_type = func_type.returntype
		local arg_syms = func_arg_syms(func)
		return terra([arg_syms]): ret_type
			var fn = [&func_type]([arg_syms[1]].__vtable[vidx-1])
			return fn([arg_syms])
		end
	end
end

function class:_addmethod(name, sig, func)
	local m = self.methods[sig]
	if m then --override inherited
		if not hook then
			error('duplicate method definition for '..name)
		end
		--TODO:
	else
		self.methods[sig] = {name = name, sig = sig, func = func}
	end
	func = self:make_virtual(sig, func)
	if not func then return end --virtual impl. replaced
	--add or overload a method.
	local func0 = self.T.methods[name]
	if not func0 then
		self.T.methods[name] = func
	else
		local typ0 = terralib.type(func0)
		if typ0 == 'overloadedterrafunction' then
			func0:adddefinition(func)
		elseif typ0 == 'terrafunction' then
			local func = terralib.overloadedfunction(name, {func0, func})
			self.T.methods[name] = func
		elseif typ0 == 'terramacro' then
			error'NYI'
		end
	end
end

--user API to add a macro or terra method.
function class:addmethod(name, method)
	local typ = terralib.type(method)
	if typ == 'terramacro' then
		local info = debug.getinfo(method.fromterra, 'u')
		assert(not info.isvararg, 'NYI')
		assert(info.nparams >= 1, 'NYI')
		local sig = self:macro_signature(name, info.nparams-1)
		self:_addmethod(name, sig, method)
	elseif typ == 'overloadedterrafunction' then
		for i,def in method:getdefinitions():ipairs() do
			local sig = self:terra_signature(name, def:gettype().parameters)
			self:_addmethod(name, sig, method)
		end
	elseif typ == 'terrafunction' then
		local sig = self:terra_signature(name, method:gettype().parameters)
		self:_addmethod(name, sig, method)
	else
		error('macro or terra function expected, got '..typ)
	end
end

--implicit cast of &derived to &ancestor. serves two purposes:
--calling ancestor methods with a derived object as self without a cast.
--inheriting ancestor methods into derived types without re-typing them.
local function __cast(from, to, exp)
	if from:ispointer() and to:ispointer() then
		local to_ptr = to
		local from, to = from.type, to.type
		while from ~= to and from ~= nil do
			local class = from.metamethods.class
			from = class and class.super
		end
		if from ~= nil then
			return `[to_ptr](exp)
		end
	end
	error(tostring(from)..' is not a subtype of '..tostring(to))
end

function class:compile(env)

	self.T = newstruct(self.name)
	self.T.metamethods.class = self

	--eval super_expr in `class <name>: <super_expr>`
	if self.ast.super_expr then
		self.super_type = self.ast.super_expr(env)
		if self.super_type then
			assert(self.super_type:isaggregate(),
				'trying to inherit from a non-agregate type')
		end
		self.super = self.super_type.metamethods.class
	end

	if self.super_type then
		--inherit super's fields at the same offsets.
		for i,entry in ipairs(self.super_type.entries) do
			if entry.private then
				--give it its due space but no name.
				--TODO: macros accessing this field won't work anymore!
				self.T.entries[i] = {type = entry.type, field = nil}
			else
				self.T.entries[i] = entry
			end
		end
	else
		add(self.T.entries, {field = '__vtable', type = &&opaque})
	end

	for i,f in ipairs(self.ast.fields) do
		self:compile_field(T, f, env)
	end

	self:init_signature_function(self.super)

	self.methods = {} --{sig  -> m}
	self.vtable  = {} --{vidx -> vfunc}
	self.vindex  = {} --{sig  -> vidx}

	if self.super_type then

		--inherit the vtable (later modified with overrides).
		if self.super then
			extend(self.vtable, self.super.vtable)
			update(self.vindex, self.super.vindex)
		end

		--inherit non-private methods from a type.
		--TODO: macros that call private methods won't work on this type.
		for name, method in pairs(self.super_type.methods) do
			self:addmethod(name, method)
		end
	end

	--type the methods and assign method hooks to methods based on signature.
	local to_compile = {}
	for i,ast in ipairs(self.ast.methods) do
		self:type_method(ast, env)
		local base_ast = to_compile[ast.sig]
		if base_ast then --method hook on uncompiled method
			if ast.hook then --add to base method
				assert(not base_ast.hook)
				base_ast.hooks = base_ast.hooks or {}
				add(base_ast.hooks, ast)
			else
				error('duplicate method definition for '..ast.name)
			end
		elseif self.vindex[ast.sig] then --override inherited virtual method
			if ast.hook then

			else
				error('redefinition of virtual method for '..ast.name)
			end
		elseif self.methods[ast.sig] then --override inherited static method
			if ast.hook then

			else
				error('redefinition of inherited static method for '..ast.name)
			end
		else --new method
			if ast.hook then
				error('method hook without a base method for '..ast.name)
			end
			to_compile[ast.sig] = ast
			add(to_compile, ast)
		end
	end

	self.T.metamethods.__methodmissing = macro(function(name, obj, ...)
		--compile dependent methods/macros on-demand.
		--find the right method/macro to compile based on its signature.
		local nargs = select('#', ...)
		local arg_types = {}
		for i = 1, nargs do
			local arg = select(i, ...)
			arg_types[i] = arg:gettype()
		end
		local sig =
			   self:terra_signature(name, arg_types)
			or self:macro_signature(name, nargs)
		local ast = to_compile[sig]
		if ast then
			local func = self:compile_method(ast, env)
			local args = {...}
			return `func(&obj, [args])
		end

		if nargs == 0 then --default getter
			return `obj.[name]
		elseif nargs == 1 then --default setter
			local arg = ...
			return quote obj.[name] = arg end
		end
	end)

	--compile all methods so that we can build the vtable.
	--we compile the methods in order of appearance in the code, because:
	-- 1) even though the order of definitions in the code shouldn't matter,
	--there's an exception: functions that are recursed into cannot have their
	--return type inferred if they are compiled before their recursors, but
	--they can have it inferred if they are compiled after.
	-- 2) in case of errors, to stop at the same error every time so as to not
	--mindfuck the user.
	for _,ast in ipairs(to_compile) do
		self:compile_method(ast, env)
	end

	local vtable = constant(`arrayof([&opaque], [self.vtable]))

	--can't be virtual since it has to initialize the vtable, and it doesn't
	--have to be becuase no method in a descendant would call this anyway.
	terra self.T:init()
		self.__vtable = vtable
		return self
	end

	self.T.metamethods.__cast = __cast

	return self.T
end

--language extension ---------------------------------------------------------

local oopslang = {
	name = 'oopslang';
	entrypoints = {'class'};
	keywords = {'before', 'after', 'over'};
}

function oopslang:statement(lex)
	local cls = class()
	cls:parse(lex)
	local ctor = function(get_env)
		return cls:compile(get_env())
	end
	return ctor, {cls.name} --statement: name = ctor(get_env))
end

oopslang.expression = oopslang.statement
oopslang.localstatement = oopslang.statement

return oopslang

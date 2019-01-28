--[[

	OOP DSL for Terra.
	Written by Cosmin Apreutesei. Public Domain.

]]

if not ... then require'oops_test'; return end

--local checkreturns = require'oopslang_checkreturns'
setfenv(1, require'low')

local oopslang = {
	name = 'oopslang';
	entrypoints = {'class', 'before', 'after'};
	keywords = {};
}

local lang = {}

function lang:parse_class(lex)
	lex:expect'class'
	local cls = {fields = {}, methods = {}}
	--class name
	cls.name = lex:expect(lex.name).value
	--super class
	if lex:nextif':' then
		cls.super = lex:expect(lex.name).value
	end
	--declarations
	while not lex:nextif'end' do
		if lex:matches'before' or lex:matches'after' then
			local where = lex:next().type
			local name = lex:expect(lex.name).value
			local m = lang:parse_method_def(lex, name)
			m.hook = where
			add(cls.methods, meth)
		else
			local name = lex:expect(lex.name).value
			if lex:nextif':' then --field definition
				local field = {name = name}
				field.type_expr = lex:luaexpr()
				if lex:nextif'=' then
					field.val_expr = lex:luaexpr()
				end
				add(cls.fields, field)
			elseif lex:matches'(' then --method definition
				local m = lang:parse_method_def(lex, name)
				add(cls.methods, m)
			else
				lex:error'field or method definition expected'
			end
		end
	end
	return cls
end

function lang:parse_method_def(lex, name)
	lex:expect'('
	local m = {name = name, args = {}}
	while not lex:nextif')' do --method args
		--TODO: support escapes in arg list (splice per Terra semantics)
		local arg = {}
		arg.name = lex:expect(lex.name).value
		lex:expect':'
		arg.type_expr = lex:luaexpr()
		lex:nextif','
		add(m.args, arg)
	end
	if lex:nextif':' then --method return type (optional)
		m.returntype_expr = lex:luaexpr()
	end
	m.body_stmts = lex:terrastats()
	lex:expect'end'
	return m
end

function lang:compile_field(T, f, env)
	add(T.entries, {field = f.name, type = f.type_expr(env)})
end

local function checkreturns(body_quote, arg_syms)
	local terra helper([arg_syms]) body_quote end
	return helper:gettype().returntype
end

function lang:compile_method(T, m, env)
	local fenv = setmetatable({}, {__index = env})
	local self_sym = symbol(&T, 'self')
	fenv.self = self_sym
	local arg_syms = {self_sym}
	for i,arg in ipairs(m.args) do
		local arg_type = arg.type_expr(env)
		local arg_sym = symbol(arg_type, arg.name)
		add(arg_syms, arg_sym)
		fenv[arg.name] = arg_sym
	end
	local body_quote = m.body_stmts(fenv)
	local ret_type = m.returntype_expr
		and m.returntype_expr(env)
		or checkreturns(body_quote, arg_syms)
	local func = terra([arg_syms]) : ret_type
		[ body_quote ]
	end
	T.methods[m.name] = func
end

function lang:compile_class(cls, env)
	cls.T = newstruct(cls.name)
	for i,f in ipairs(cls.fields) do
		self:compile_field(cls.T, f, env)
	end
	for i,m in ipairs(cls.methods) do
		self:compile_method(cls.T, m, env)
	end
	return cls.T
end

function oopslang:statement(lex)
	if lex:matches'class' then
		local cls = lang:parse_class(lex)
		local ctor = function(get_env)
			return lang:compile_class(cls, get_env())
		end
		return ctor, {cls.name} -- statement: name = ctor(get_env))
	elseif lex:matches'after' or lex:matches'before' then
		local where = lex:next().type
		local cls_name = lex:expect(lex.name).value
		lex:ref(cls_name)
		lex:expect':'
		local name = lex:expect(lex.name).value
		local m = lang:parse_method_def(lex, name)
		m.hook = where
		local ctor = function(get_env)
			local env = get_env()
			local T = env[cls_name]
			return lang:compile_method(T, m, env)
		end
		return ctor
	else
		lex:error'expected: class, before, after'
	end
end

oopslang.expression = oopslang.statement
oopslang.localstatement = oopslang.statement

return oopslang


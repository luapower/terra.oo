--[[

	OOP DSL for Terra.
	Written by Cosmin Apreutesei. Public Domain.

]]

if not ... then require'oopslang_test'; return end

setfenv(1, require'low')

local oopslang = {
	name = 'oopslang';
	entrypoints = {'class'};
	keywords = {'before', 'after'};
}

local lang = {}

function lang:parse_method_def(lex, name)
	lex:expect'('
	local meth = {name = name, args = {}}
	while not lex:nextif')' do --method args
		--TODO: support escapes in arg list (splice per Terra semantics)
		local arg = {}
		arg.name = lex:expect(lex.name).value
		lex:expect':'
		arg.type_expr = lex:luaexpr()
		lex:nextif','
		add(meth.args, arg)
	end
	if lex:nextif':' then --method return type
		meth.returntype_expr = lex:luaexpr()
	end
	meth.body_stmts = lex:terrastats()
	lex:expect'end'
	return meth
end

function lang:compile_field(cls, f, env)
	add(cls.T.entries, {field = f.name, type = f.type_expr(env)})
end

function lang:compile_method(cls, m, env)
	local fenv = setmetatable({}, {__index = env})
	local self_sym = symbol(&cls.T, 'self')
	fenv.self = self_sym
	local arg_syms = {self_sym}
	for i,arg in ipairs(m.args) do
		local arg_type = arg.type_expr(env)
		local arg_sym = symbol(arg_type, arg.name)
		add(arg_syms, arg_sym)
		fenv[arg.name] = arg_sym
	end
	local ret_type = m.returntype_expr and m.returntype_expr(env) or {}
	local body_quote = m.body_stmts(fenv)
	local func = terra([arg_syms]) : ret_type
		[ body_quote ]
	end
	cls.T.methods[m.name] = func
end

function lang:compile_class(cls, env)
	cls.T = newstruct(cls.name)
	for i,f in ipairs(cls.fields) do
		self:compile_field(cls, f, env)
	end
	for i,m in ipairs(cls.methods) do
		self:compile_method(cls, m, env)
	end
	return cls.T
end

function oopslang:statement(lex)
	local cls = {fields = {}, methods = {}}
	lex:expect'class'
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
			local meth = lang:parse_method_def(lex, name)
			meth.hook = where
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
				local meth = lang:parse_method_def(lex, name)
				add(cls.methods, meth)
			else
				lex:error'field or method definition expected'
			end
		end
	end
	local ctor = function(environment_function)
		local env = environment_function()
		return lang:compile_class(cls, env)
	end
	return ctor, {cls.name} -- create the statement: name = ctor(environment_function))
end

oopslang.expression = oopslang.statement
oopslang.localstatement = oopslang.statement

return oopslang


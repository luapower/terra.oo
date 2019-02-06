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
			local ast = self:parse_method_or_macro_def(lex, name)
			ast.hook = hook
			add(self.ast.methods, ast)
		else
			local name = lex:expect(lex.name).value
			if lex:nextif':' then --<field_def>
				local ast = self:parse_field_def(lex, name)
				add(self.ast.fields, ast)
			elseif lex:matches'(' then --<method_def> | <macro_def>
				local ast = self:parse_method_or_macro_def(lex, name)
				add(self.ast.methods, ast)
			else
				lex:error'field or method definition expected'
			end
		end
	end
end

--<field_name>: field_type_expr [=init_const_expr]
function class:parse_field_def(lex, name)
	local f = {name = name, private = name:starts'_' or nil}
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
	local ast = {name = name, args = {}, ismacro = false, virtual = true,
		private = name:starts'_' or nil}
	while not lex:nextif')' do --method args
		--TODO: support escapes in arg list (splice per Terra semantics)
		local arg = {}
		arg.name = lex:expect(lex.name).value
		if lex:nextif':' then
			arg.type_expr = lex:luaexpr()
		else
			ast.ismacro = true
		end
		lex:nextif','
		add(ast.args, arg)
	end
	if lex:nextif':' then --method return type (optional)
		ast.ret_type_expr = lex:luaexpr()
	end
	ast.body_stmts = ast.ismacro and lex:luastats() or lex:terrastats()
	lex:expect'end'
	return ast
end

--compiler -------------------------------------------------------------------

function class:compile_field(ast, env)
	add(self.T.entries, {
		name = ast.name,
		field = self.name..'.'..ast.name,
		type = ast.type_expr(env),
		private = ast.private,
	})
end

function class:entry(name)
	local field = self.name..'.'..name
	for i,e in ipairs(self.T.entries) do
		if e.field == field then
			return e.type
		end
	end
end

--return a unique identity for each method signature across all methods.
--terra methods are overloadable by (name, arg1_type, ...).
--macros are overloadable by (name, arg_count).
local sigmt = {}
function sigmt:__tostring()
	local t = {}; for i=1,#self do add(t, tostring(self[i])) end
	return '('..table.concat(t, ', ')..')'
end
local signature = memoize(function(...) return setmetatable({...}, sigmt) end)
function class:signature(...) return signature(...) end

--eval Lua expressions for argument and return types and compute
--the method signature. make arg symbols too for building the method later.
function class:type_method(ast, env)
	if not ast.ismacro then
		local arg_types = {}
		ast.arg_syms = {symbol(&self.T, 'self')}
		for _,arg in ipairs(ast.args) do
			local arg_type = arg.type_expr(env); arg.type_expr = nil
			add(arg_types, arg_type)
			add(ast.arg_syms, symbol(arg_type, arg.name))
		end
		if ast.ret_type_expr then
			ast.ret_type = ast.ret_type_expr(env); ast.ret_type_expr = nil
		end
		ast.sig = self:signature(ast.name, unpack(arg_types))
	else
		ast.sig = self:signature(ast.name, #ast.args)
	end
end

--make a terra function from scratch given all the necessary ingredients.
local nothing = quote end
local function mkterra(arg_syms, ret_type, body_quote)
	body_quote = body_quote or nothing
	return ret_type
		and terra([arg_syms]): ret_type body_quote end
		 or terra([arg_syms]) body_quote end
end

--make a terra function with the same type as another function but with its
--body constructed by a constructor which receives the arg symbols.
local function mksameterra(func, mkbody)
	local arg_syms = {} --pluck arg symbols from func's definition.
	for i,var_ in ipairs(func.definition.parameters) do
		add(arg_syms, var_.symbol)
	end
	local ret_type = func:gettype().returntype
	local body_quote = mkbody and mkbody(arg_syms)
	return mkterra(arg_syms, ret_type, body_quote)
end

--build a terra function in a lexical environment. extra_arg_syms are extra
--symbols to inject in the function environment and pass as first args.
--compilation is re-entrant to support recursive references.
function class:compile_terra(ast, env, extra_arg_syms, inherited_func)

	local arg_syms = extra_arg_syms
		and extend({}, extra_arg_syms, ast.arg_syms) or ast.arg_syms

	if ast.compiling then --recursive reference, make a stub to be replaced later.
		ast.stub = ast.stub or mkterra(arg_syms, ast.ret_type)
		return ast.stub
	end

	local fenv = setmetatable({}, {__index = env})
	local self_sym = symbol(&self.T, 'self')
	fenv.self = self_sym
	for i,arg_sym in ipairs(arg_syms) do
		fenv[arg_sym.displayname] = arg_sym
	end
	ast.compiling = true --trap recursive references from __methodmissing
	local saved_inherited_func = self.T.methods.inherited
	self.T.methods.inherited = inherited_func
	local body_quote = ast.body_stmts(fenv); ast.body_stmts = nil
	local func = mkterra(arg_syms, ast.ret_type, body_quote)
	self.T.methods.inherited = saved_inherited_func
	ast.compiling = false

	if ast.stub then
		if ast.stub:gettype().returntype ~= func:gettype().returntype then
			error('must provide return type for method '..ast.name)
		end
		ast.stub:resetdefinition(func)
		func = ast.stub; ast.stub = nil
	end

	return func
end

--build a macro in a lexical environment.
function class:compile_macro(ast, env)
	local fenv = setmetatable({}, {__index = env})
	return macro(function(self, ...)
		fenv.self = self
		for i,arg in ipairs(ast.args) do
			fenv[arg.name] = select(i, ...)
		end
		return ast.body_stmts(fenv)
	end)
end

function class:compile_method(ast, env, inherited_func)
	if ast.ismacro then
		return self:compile_macro(ast, env, inherited_func)
	else
		return self:compile_terra(ast, env, inherited_func)
	end
end

function class:compile_getter(sig)
	local name = sig[1]
	local val_t = self:entry('_'..name)
	return val_t and terra(self: &self.T): val_t
		return self.['_'..name]
	end
end

function class:compile_setter(sig)
	local name = sig[1]:gsub('^set_', '')
	local val_t = self:entry('_'..name)
	return val_t and terra(self: &self.T, val: val_t)
		self.['_'..name] = val
	end
end

function class:compile_macro_override(ast, env, func)
	return self:compile_macro(ast, env)
end

local compile_terra_override = {}

function compile_terra_override:over(ast, env, func)
	local func_type = func:gettype()
	local inherited_sym = symbol(&func_type, 'inherited')
	local over_func = self:compile_terra(ast, env, {inherited_sym}, func)
	return mkterra(ast.arg_syms, ast.ret_type, quote
		var [inherited_sym] = func
		return over_func([inherited_sym], [ast.arg_syms])
	end)
end

function compile_terra_override:before(ast, env, func)
	local before_func = self:compile_terra(ast, env, nil, func)
	return mkterra(ast.arg_syms, ast.ret_type, quote
		before_func([ast.arg_syms])
		return func([ast.arg_syms])
	end)
end

function compile_terra_override:after(ast, env, func)
	local ret_type = func:gettype().returntype
	--make `retval` available in the hook's environment
	local retval_sym = symbol(&ret_type, 'retval')
	local after_func = self:compile_terra(ast, env, {retval_sym}, func)
	if after_func:gettype().returntype:isunit() then
		return mkterra(ast.arg_syms, ast.ret_type, quote
			var retval = func([ast.arg_syms])
			after_func(&retval, [ast.arg_syms])
			return retval
		end)
	else --hook returns a value: return it back.
		return mkterra(ast.arg_syms, ast.ret_type,
			`after_func(func([ast.arg_syms]), [ast.arg_syms])
		)
	end
end

function class:compile_terra_override(ast, env, func)
	return compile_terra_override[ast.hook](self, ast, env, func)
end

function class:compile_method_override(ast, env, func)
	assert(ast.hook and func)
	if ast.ismacro then
		assert(type(func) == 'terramacro')
		return self:compile_macro_override(ast, env)
	else
		assert(type(func) == 'terrafunction')
		return self:compile_terra_override(ast, env, func)
	end
end

--[[
function class:_addmethod(sig, func, virtual)
	if virtual then
		func = self:make_virtual(sig, func)
	end
	if not func then return end --virtual impl. replaced
	--add or overload a method.
	local name = assert(sig[1])
	local func0 = self.T.methods[name]
	if not func0 then
		self.T.methods[name] = func
	else
		if type(func0) == 'overloadedterrafunction' then
			func0:adddefinition(func)
		elseif type(func0) == 'terrafunction' then
			local func = terralib.overloadedfunction(name, {func0, func})
			self.T.methods[name] = func
		elseif type(func0) == 'terramacro' then
			error'NYI: overloading a macro'
		end
	end
end

--user API to add a macro, terra function or terra overloaded function.
function class:addmethod(name, func, virtual)
	if type(func) == 'terramacro' then
		local info = debug.getinfo(func.fromterra, 'u')
		assert(not info.isvararg, 'NYI')
		assert(info.nparams >= 1, 'NYI')
		local sig = self:signature(name, info.nparams-1)
		self:_addmethod(sig, func, virtual)
	elseif type(func) == 'overloadedterrafunction' then
		for i,def in func:getdefinitions():ipairs() do
			local sig = self:signature(name, unpack(def:gettype().parameters))
			self:_addmethod(sig, func, virtual)
		end
	elseif type(func) == 'terrafunction' then
		local sig = self:signature(name, unpack(func:gettype().parameters))
		self:_addmethod(sig, func, virtual)
	else
		error('macro or terra function expected, got '..type(func))
	end
end
]]

--this contains all the logic of method generation including deciding when
--to compile a method, when to create getters and setters, when to make
--a static method virtual and when to override an inherited (or new) method.
function class:resolve_method(sig, env, used)
	local func
	local m = self.methods[sig]
	if m then
		func = m.vfunc or m.func --previously compiled or inherited
		if not func and m.ast then --new
			func = self:compile_method(m.ast, env)
		end
		if m.hooks then --hooks over new or inherited
			for _,ast in ipairs(m.hooks) do
				func = self:compile_method_override(ast, env, func)
			end
			m.hooks = nil
		end
	elseif #sig == 1 then
		func = self:compile_getter(sig)
	elseif #sig == 2 and sig[1]:starts'set_' then
		func = self:compile_setter(sig)
	end
	if not func then
		return
	end
	if not m then
		m = {}; self.methods[sig] = m
	end
	--new implementation, update it.
	if func ~= (m.vfunc or m.func) then
		if m.vfunc then --virtual, replace the implementation
			m.vfunc = func
			self.vtable[self.vindex[sig]] = func
		else --static
			m.func = func
		end
	end
	--make the method virtual if needed and possible (not self.compiled).
	if used and not m.private and not m.vfunc and not self.compiled then
		assert(not self.vindex[sig])
		--allocate a new slot and method stub
		local vidx = #self.vtable + 1
		self.vindex[sig] = vidx
		self.vtable[vidx] = func
		m.vfunc = func
		local func_type = func:gettype()
		func = mksameterra(func, function(args)
			return quote
				var fn = [&func_type]([args[1]].__vtable[vidx-1])
				return fn([args])
			end
		end)
		m.func = func
	end
	return func
end

--implicit cast of &derived to &ancestor. serves two purposes:
--calling ancestor methods with a derived object as self without a cast.
--inheriting ancestor methods into derived types without re-typing them.
local function __cast(from, to, exp)
	if from:ispointer() and to:ispointer() then
		local to_ptr = to
		local from, to = from.type, to.type
		while from ~= to and from ~= nil do
			local mm = from.metamethods
			from = mm and mm.class and mm.class.super_type
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

	--FIELDS

	--inherit super's fields at the same offsets.
	if self.super_type then
		for i,entry in ipairs(self.super_type.entries) do
			self.T.entries[i] = {
				type = entry.type,
				name = entry.name,
				field = entry.private
					and (self.super and entry.field or tostring(i))
					 or entry.name and self.name..'.'..entry.name or entry.field,
				private = entry.private,
			}
		end
	else
		add(self.T.entries, {field = '__vtable', type = &&opaque})
	end

	--qualify field names (this is how super's private fields get hidden).
	self.T.metamethods.__entrymissing = macro(function(name, obj)
		if name:find('.', 1, true) then
			error('invalid field '..name)
		end
		return `obj.[self.name..'.'..name]
	end)

	--add new fields.
	for i,f in ipairs(self.ast.fields) do
		self:compile_field(f, env)
	end

	--METHODS

	self.methods = {} --{sig  -> method}
	self.vtable  = {} --{vidx -> vfunc}
	self.vindex  = {} --{sig  -> vidx}

	if self.super then
		--inherit the vtable (later modified with overrides).
		extend(self.vtable, self.super.vtable)
		update(self.vindex, self.super.vindex)
		--inherit non-private methods.
		for sig,m in pairs(self.super.methods) do
			if not m.private then
				self.methods[sig] = {
					func = m.func,
					vfunc = m.vfunc,  --virtual function, this is the impl.
				}
			end
		end
	elseif self.super_type then
		--TODO: inherit methods from any aggregate type.
	end

	--add new (to-compile) methods and their hooks, in order.
	for i,ast in ipairs(self.ast.methods) do
		self:type_method(ast, env)
		local m = self.methods[ast.sig]
		if not m then
			assert(not ast.hook, 'missing method to override for '..ast.name)
			m = {ast = ast, private = ast.private}
			self.methods[ast.sig] = m
		else
			assert(ast.hook, 'duplicate definition for '..ast.name)
			m.hooks = m.hooks or {}
			add(m.hooks, ast)
		end
	end

	--__methodmissing hook to compile dependent methods on-demand and to figure
	--out which ones are called by other methods so as to make them virtual.
	self.T.metamethods.__methodmissing = macro(function(name, obj, ...)
		local nargs = select('#', ...)
		local arg_types = {}
		for i = 1, nargs do
			local arg = select(i, ...)
			arg_types[i] = arg:gettype()
		end
		local sig = self:signature(name, unpack(arg_types))
		local func =
			   self:resolve_method(sig, env, true)
			or self:resolve_method(self:signature(name, nargs), env, true)
		if not func then
			error('method missing '..tostring(sig))
		end
		local args = {...}
		return `func(&obj, [args])
	end)

	--compile all methods so that we can build the vtable.
	--methods are compiled in order of appearance in the code because:
	-- 1) the before/after/over hooks need to be applied in order.
	-- 2) functions that are recursed into cannot have their return type
	-- inferred if they are compiled before their recursors, but they can have
	-- it inferred if they are compiled after.
	-- 3) to stop at the same compilation error every time.
	for i,ast in ipairs(self.ast.methods) do
		self:resolve_method(ast.sig, env)
	end

	local vtable = constant(`arrayof([&opaque], [self.vtable]))
	self.compiled = true --can't add more virtual methods from this point on.

	--init() can't be virtual since it has to initialize the vtable.
	--that's ok because init() is strictly user API.
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

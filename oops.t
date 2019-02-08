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
	--nocompile flag
	if lex:nextif'nocompile' then
		self.nocompile = true
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

	self.T = newstruct(self.name)
	self.T.metamethods.class = self
	self.T.compile = self.tcompile
	return self.T
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
	local ast = {name = name, args = {}, ismacro = false,
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
		val = ast.val_expr and ast.val_expr(env),
	})
	ast.val_expr = nil
	ast.type_expr = nil
end

--return a unique identity for each method signature across all methods.
--terra methods are overloadable by (name, arg1_type, ...).
--macros are not overloadable so their signature is (name, true).
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
		ast.sig = self:signature(ast.name, true)
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
local function mksameterra(func, mkbody, self_type)
	local arg_syms = {} --pluck arg symbols from func's definition.
	for i,var_ in ipairs(func.definition.parameters) do
		add(arg_syms, var_.symbol)
	end
	if self_type then
		arg_syms[1] = symbol(&self_type, 'self')
	end
	local ret_type = func:gettype().returntype
	local body_quote = mkbody and mkbody(arg_syms)
	return mkterra(arg_syms, ret_type, body_quote)
end

--build a terra function in a lexical environment. extra_arg_syms are extra
--symbols to inject in the function environment and pass as first args.
function class:compile_terra(ast, env, extra_arg_syms, inherited_func)
	local arg_syms = extra_arg_syms
		and extend({}, extra_arg_syms, ast.arg_syms) or ast.arg_syms
	local fenv = setmetatable({}, {__index = env})
	local self_sym = symbol(&self.T, 'self')
	fenv.self = self_sym
	for i,arg_sym in ipairs(arg_syms) do
		fenv[arg_sym.displayname] = arg_sym
	end
	local saved_inherited_func = self.T.methods.inherited
	self.T.methods.inherited = inherited_func
	local body_quote = ast.body_stmts(fenv); ast.body_stmts = nil
	local func = mkterra(arg_syms, ast.ret_type, body_quote)
	self.T.methods.inherited = saved_inherited_func
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

--add or overload a method.
function class:add_method(sig, func)
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
			error('duplicate definition for macro '..name..'()')
		end
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
			local mm = from.metamethods
			from = mm and mm.class and mm.class.super and mm.class.super.T
		end
		if from ~= nil then
			return `[to_ptr](exp)
		end
	end
	error(tostring(from)..' is not a subtype of '..tostring(to))
end

function class:compile(env)

	--eval super_expr in `class <name>: <super_expr>`
	if self.ast.super_expr then
		local st = self.ast.super_expr(env)
		if st then
			assert(st:isstruct(), 'trying to inherit from non-struct type',st)
			self.super = assert(st.metamethods.class, 'struct is not a class ',st)
			st:compile()
		end
	end

	--FIELDS

	--inherit super's fields at the same offsets.
	if self.super then
		for i,entry in ipairs(self.super.T.entries) do
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
	for i,ast in ipairs(self.ast.fields) do
		self:compile_field(ast, ast.env or env)
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
					ismacro = m.ismacro,
				}
			end
		end
	end

	--add new (to-compile) methods and method hooks, in order.
	for i,ast in ipairs(self.ast.methods) do
		self:type_method(ast, ast.env or env)
		local m = self.methods[ast.sig]
		if not m then
			assert(not ast.hook, 'missing method to override for ',ast.sig)
			m = {ast = ast, private = ast.private, ismacro = ast.ismacro}
			self.methods[ast.sig] = m
		else
			assert(ast.hook, 'duplicate definition for ',ast.sig)
			assert(m.ast or m.vfunc, 'trying to override static method ',ast.sig)
			m.inherited = m.vfunc
		end
	end

	--make stubs for all methods that have a new implementation so that
	--method references can be resolved by terra eagerly.
	--NOTE: method references can also be solved in a __methodmissing
	--handler but that would require duplicating the terra logic for chosing
	--an overloaded definition for a particular combination of arg types.
	--This method has its own big drawback: return-type inference doesn't work
	--anymore.
	for sig,m in pairs(self.methods) do
		if not m.ismacro then
			local stub
			if m.ast then
				stub = mkterra(m.ast.arg_syms, m.ast.ret_type)
			elseif m.inherited then
				stub = mksameterra(m.inherited, nil, self.T)
			end
			if stub then
				if m.inherited then --replace entry in vtable
					m.vfunc = stub
					self.vtable[self.vindex[sig]] = stub
				elseif not m.private then --create new entry in vtable
					assert(not self.compiled)
					assert(not self.vindex[sig])
					m.vfunc = stub
					--allocate a new slot and method stub
					local vidx = #self.vtable + 1
					self.vindex[sig] = vidx
					self.vtable[vidx] = stub
					local func_type = stub:gettype()
					m.func = mksameterra(stub, function(args)
						return quote
							var fn = [&func_type]([args[1]].__vtable[vidx-1])
							return fn([args])
						end
					end)
				else --static private method
					m.func = stub
				end
			end
		end
		self:add_method(sig, m.func)
	end

	--compile all methods and method hooks so we can build the vtable.
	for i,ast in ipairs(self.ast.methods) do
		local m = self.methods[ast.sig]
		local stub = m.vfunc or m.func
		if not ast.ismacro then
			if not ast.hook then
				local func = self:compile_terra(ast, ast.env or env)
				stub:resetdefinition(func)
			else
				local func = self:compile_terra_override(ast, ast.env or env, m.inherited or m.vfunc)
				stub:resetdefinition(func)
				m.inherited = func
			end
		else
			assert(not ast.hook or ast.hook == 'over')
			local func = self:compile_macro(ast, ast.env or env)
			m.vfunc = func
			m.func = func
			self.T.methods[ast.sig[1]] = func
		end
	end

	local vtable = constant(`arrayof([&opaque], [self.vtable]))
	self.compiled = true --can't add more virtual methods from this point on.

	terra self.T:__init_vtable()
		self.__vtable = vtable
	end

	--init() can't be virtual since it has to initialize the vtable.
	--that's ok because init() is strictly user API.
	local entries = self.T.entries
	local function init_fields(self)
		local t = {}
		for i,e in ipairs(entries) do
			if e.val then
				add(t, quote self.[e.name] = [e.val] end)
			end
		end
		return t
	end
	local super_init = self.super and self.super.T.methods.init
	if super_init then
		terra self.T:init()
			super_init(self)
			self.__vtable = vtable
			[init_fields(self)]
			return self
		end
	else
		terra self.T:init()
			self.__vtable = vtable
			[init_fields(self)]
			return self
		end
	end

	self.T.metamethods.__cast = __cast
end

function class.tcompile(T)
	local self = T.metamethods.class
	if self.compiled then return end
	self:compile(self.env)
end

--language extension ---------------------------------------------------------

local oopslang = {
	name = 'oopslang';
	entrypoints = {'class', 'fn', 'before', 'after', 'over'};
	keywords = {'nocompile'};
}

function oopslang:statement(lex)
	if lex:matches'class' then
		local cls = class()
		local T = cls:parse(lex)
		local ctor = function(get_env)
			if cls.nocompile then
				cls.env = get_env()
			else
				cls:compile(get_env())
			end
			return T
		end
		return ctor, {cls.name} --statement: name = ctor(get_env))
	elseif lex:matches'fn' or lex:matches'before' or lex:matches'after' or lex:matches'over' then
		--fn|before|after|over <classname>:<method_def>|<macro_def>
		local hook = lex:next().type; if hook == 'fn' then hook = nil end
		--TODO: support a.b:c syntax
		local clsname = lex:expect(lex.name).value
		lex:ref(clsname)
		lex:expect':'
		local name = lex:expect(lex.name).value
		local ast = class:parse_method_or_macro_def(lex, name)
		ast.hook = hook
		return function(get_env)
			ast.env = get_env()
			local T = ast.env[clsname]
			local self = T.metamethods.class
			add(self.ast.methods, ast)
		end
	end
end

oopslang.expression = oopslang.statement
oopslang.localstatement = oopslang.statement

return oopslang

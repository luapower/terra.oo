setfenv(1, require'low')
import'oops'

function test_private_fields()
	local class C1
		_x: int --private field
		 y: int --public field
	end

	local class C2 : C1 end --private fields are not inherited.

	local class C3 : C1
		_x: int --private with the same name as super's: not clashing.
		--y: int
		--^^uncomment: public field with the same name as super's: clashing.
	end

	local terra test()
		var c1: C1; c1:init()
			c1._x = 1
			c1.y = 1
		var c2: C2; c2:init()
			--c2._x = 1
			--^^uncomment: invalid field: private, not inherited.
			c2.y = 1 --public field: inherited.
		var c3: C3; c3:init()
			c3._x = 1
			c3.y = 1
	end
	--C1:printpretty()
	--C2:printpretty()
	--C3:printpretty()
	test()
end

function test_inheritance()
	local class C1
		f(x: int): int return x+1 end
		g(x: int): int return self:f(x) end
	end
	local class C2: C1
		h(x: int): int return x*x end
		over f(x: int): int return self:inherited(x)+1 end --override
	end
	local terra test()
		var c2: C2; c2:init()
		var x = c2:g(5) --inherited, not overriden. calls overriden C2:f()
		assert(x == 7)
		assert(c2:h(3) == 9) --not inherited, not overriden.
		assert([&C1](&c2):f(5) == 7) --calling C2:f() still
	end
	test()
end

function test_recursion()
	local class C1
		h(x: int): int return iif(x > 0, self:f(x), x) end
		g(x: int): int return self:f(x) end
		f(x: int): int return iif(x > 0, self:h(-x), self:g(-x)) end
		s(x: int): int return iif(x > 0, self:s(x-1), x) end --self-recursion
	end
	local terra test()
		var c1: C1; c1:init()
		assert(c1:h(5) == -5)
		assert(c1:s(10) == 0)
	end
	test()
end

function test_macros()
	local class C1
		g(x) return `-self:f(x) end --decl. order doesn't matter
		f(x)
			if x:asvalue() > 0 then
				return `self:f(-x)
			else
				return `x
			end
		end --recursive ref.
	end
	local terra test()
		var c1: C1; c1:init()
		assert(c1:g(5) == 5)
	end
	test()
end

function test_macro_override()
	local class C1
		f(x) return `x*x end
		g(x) return `-self:f(x) end
		_f(x) end --private
		h(x) return `-self:_f(x) end
	end
	local class C2: C1
		over f(x) return `x end --no access to inherited (don't use self:inherited()!)
		_f(x) return `x end --_f not inherited, so no `over`
	end
	local terra test()
		var c2: C2; c2:init()
		assert(c2:g(5) == -5) --macros act like virtual methods sometimes,
		assert([&C1](&c2):g(5) == -25) --and like static methods other times.
		assert(c2:h(5) == -5)
		assert([&C1](&c2):g(5) == -25)
	end
	test()
end

function test_hooks()
	local class C1
		f(x: int): int return x+1 end
		g(x: int): int return self:f(x) end
	end
	local class C2: C1
		before_hit: bool
		after_hit: bool
		before f(x: int): int self.before_hit = true end --hook on inherited
		after  f(x: int) self.after_hit = true; @retval = -x end --hook on overriden
	end
	local terra test()
		var c2: C2; c2:init()
		assert(c2:g(5) == -5)
		assert(c2.before_hit)
		assert(c2.after_hit)
	end
	test()
end

function test_init_values()
	local class C1
		_x: int = 5
		 y: int = 7
	end

	local class C2 : C1
		_x: int = 3
		 z: int = 9
	end

	local terra test()
		var c1: C1; c1:init()
		var c2: C2; c2:init()
		assert(c1._x == 5)
		assert(c1. y == 7)
		assert(c2._x == 3)
		assert(c2. z == 9)
		assert(c2. y == 7)
	end
	test()
end

function test_nocompile()
	local class C1 nocompile end
	local class C2 : C1 nocompile end

	over   C2:f() print'C2:over['; self:inherited() print']' end
	after  C2:f() print'after C2:f()' end
	before C2:f() print'before C2:f()' end

	fn C1:f() print'C1:f()' end
	fn C1:g() print'C1:g()' self:f() end

	C2:compile() --auto-compiles super

	terra test()
		var c2: C2; c2:init()
		c2:f()
	end
	test()
end

function test_struct_fields()
	--
end

--test_private_fields()
--test_inheritance()
--test_recursion()
--test_macro_override()
--test_hooks()
--test_init_values()
test_nocompile()

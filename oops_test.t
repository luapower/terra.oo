setfenv(1, require'low')
import'oops'

class C

	before f(x: int)
		print('2 f(x)', x); return x+1
	end

	before f(y: int)
		print('1 f(y)', y)
	end

	after f(a: int)
		print('3 f(a)', a, retval)
		return retval+1
	end

end

terra test()
	var c: C; c:init()
	print(c:f(5))
end
test()

--[[


local x = 0
local y = 5
local p = symbol(int)

class C

	g() self:f(20) end --uses undeclared self.f and self.x

	x: int

	f(x: int)
		self.x = x
	end

	h() print'h0' end
	h(i: int) print'h1' end

	x(i: int) print'setx'; self.x = i end
	x() print 'getx'; return self.x end

	m(xx) print('m: '..(xx:asvalue())); return `self end

	after x(i: int) print('after x(i:int)', i) end
	before x(i: int) print('before x(i:int)', i) end
	after x() retval = 25; print('after x()', retval) end
	before x() print'before x()' end

end

class C1: C

	f(x: int)
		print('override', x)
	end

end

terra f()
	var c: C1; c:init()
	c:g()
	--c:f(5)
	--c:h()
	--c:h(1)
	--c:x(2)
	--print(c:x())
	--print(c:m(5))
	--print(c.x)
end
f()
]]


--[[
class C0

	myfield: int = x + y

	mymethod(p: int): {int,char}
		var x = 1
	end

	myvirtual(i: int): int
		return 5
	end

	callsvirtual(i: int): int
		return self:myvirtual(i)
	end

	--inherit C0.*

end

class C1: C0

	myfield2: char

	_border_width: int

	border_width: int

	get_border_width()
		return 5
	end

	set_border_width(v: int)
		return 7
	end

	myvirtual(i: int): int
		return inherited(self, i) + 1
	end

	myvirtual(i: int, j: int): int --overloaded
		border_width = i
	end

	before callsvrtual(i: int)
		print('before', i)
	end

	after callsvirtual(i: int)
		print('after', i)
	end

	after set_border_width(v: int)

	end

end
]]

--pr(C0)
--pr(C1)


--[[

TODO:

- access to super's method from inside the override
- private / public fields
- init values for fields
- escapes in method arg list

]]

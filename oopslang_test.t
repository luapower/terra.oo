setfenv(1, require'low')
import'oopslang'

local x = 0
local y = 5
local p = symbol(int)

class C

	x: int

	f(x: int)
		print(x)
		self.x = 1
		return x
	end

end

terra f()
	var c: C
	c:f(5)
end
f()


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

todo:

- escapes in method arg list
- infer rettype based on type of expr of return statements
- infer arg types and rettype for getters and setters
- implicit self

what can we do:

- inheritance
- auto-overriding
- access to super's method from inside the override
- auto-overloading
- composing with before, after
- private / public fields
- init values for fields
-

]]

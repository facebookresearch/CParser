-- Copyright (c) Facebook, Inc. and its affiliates.
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.

if false then -- set to true and use lua53 to get the correct output
   local bit = {}
   function bit.bnot(a) return ~a end
   function bit.bor(a,b) return a | b end
   function bit.band(a,b) return a & b end
   function bit.bxor(a,b) return a ~ b end
   function bit.lshift(a,b) return a < 0 and b < 0 and ~((~a) << b) or a << b end
   return bit
else
   local function bor(a,b)
      local r, c, d = 0, 1, -1
      while a > 0 or b > 0 or a < -1 or b < -1 do
	 if a % 2 > 0 or b % 2 > 0 then r = r + c end
	 a, b, c, d = math.floor(a / 2), math.floor(b / 2), c * 2, d * 2 end
      if a < 0 or b < 0 then r = r + d end
      return r end
   bit = {}
   function bit.bnot(a) return -1-a end
   function bit.bor(a,b) return bor(a,b) end
   function bit.band(a,b) return -1-bor(-1-a,-1-b) end
   function bit.bxor(a,b) return bor(-1-bor(a,-1-b),-1-bor(-1-a,b)) end
   function bit.lshift(a,b) return math.floor(a * 2 ^ b) end
end

-- cparser = require 'cparser'

local args = {}

local s = 314
for i=1,10000 do
   s = (s * 6900069) % 8989889
   z = (s % 2) > 0 and 1 or -1
   args[1+#args] = z * math.floor(s / 2)
end

local function test(s,f)
   for i=1,#args do
      local a = args[i]
      local b = args[1+(i+3)%(#args)]
      print(s,a,b,f(a,b))
   end
end

test("bitnot",bit.bnot)
test("bitand",bit.band)
test("bitor",bit.bor)
test("bitxor",bit.bxor)
test("bitshift",function(a,b)
	local s = b % 2
	local bb = (math.floor(b/2) % 33) * (-1 ^ s)
	return bit.lshift(a,bb) end)


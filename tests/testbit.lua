-- Copyright (c) Facebook, Inc. and its affiliates.
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.

cparser = require 'cparser'

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


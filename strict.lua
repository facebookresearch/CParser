
--[[

-- strict.lua
-- checks uses of undeclared global variables
-- All global variables must be 'declared' through a regular assignment
-- (even assigning nil will do) in a main chunk before being used
-- anywhere or assigned to inside a function.

Lua License
-----------

Lua is licensed under the terms of the MIT license reproduced below.
This means that Lua is free software and can be used for both academic
and commercial purposes at absolutely no cost.

For details and rationale, see http://www.lua.org/license.html .

===============================================================================

Copyright (C) 1994-2012 Lua.org, PUC-Rio.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

===============================================================================
]]--


local getinfo, error, rawset, rawget = debug.getinfo, error, rawset, rawget

local mt = getmetatable(_G)
if mt == nil then
  mt = {}
  setmetatable(_G, mt)
end

mt.__declared = {}

local function what ()
  local d = getinfo(3, "S")
  return d and d.what or "C"
end

mt.__newindex = function (t, n, v)
  if not mt.__declared[n] then
    local w = what()
    if w ~= "main" and w ~= "C" then
      error("assign to undeclared variable '"..n.."'", 2)
    end
    mt.__declared[n] = true
  end
  rawset(t, n, v)
end
  
mt.__index = function (t, n)
  if not mt.__declared[n] and what() ~= "C" then
    error("variable '"..n.."' is not declared", 2)
  end
  return rawget(t, n)
end


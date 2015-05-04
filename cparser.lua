-- Parse C declarations in Lua
-- (C) 2015 Leon Bottou, Facebook


local DEBUG = true

local string = require 'string'
local coroutine = require 'coroutine'
local table = require 'table'
local io = require 'io'

local unpack = unpack or table.unpack

if DEBUG then require 'strict' end


---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
-- DOCUMENTATION



-- ==================================================
-- CPARSER.CPP
--
-- SYNOPSIS
--  cparser.cpp(<filename>, [<outputfile>, [<options>]])
--
-- SUMMARY
--  Preprocesses file <filename>.
--     The optional argument <outputfile> specifies where to write the
--  preprocessed file and may be a string or a file descriptor.  The
--  default is the standard output.
--     The optional argument <options> contains an array of option
--  strings.  Most of these options are valid for all cparser exported
--  functions and are only documented here. Note that option <-dM> is
--  specific to this function and option <-Zpass> is on by default
--  unless one gives option "-Znopass".
--
-- OPTIONS
--  Array <options> can contain the following options.
--  All other options are ignored silently.
--
-- -Werror   Cause all warning to be treated as errors.
--           Note that parsing cannot resume after an error.
--           The parser simply throws a Lua error.
--
-- -w        Do not print warning messages.
--
-- -D<x>=<y>
-- -D<x>     Define preprocessor symbol <x> to value <y>.
--           The default value of <y> is 1
--
-- -U<x>     Undefine preprocessor symbol <x>.
--
-- -I<dir>   Add directory <dir> to the search path for
--           included files. Note that there is no default
--           search path. When an include file is not found
--           the include directive is simply ignored with
--           a warning (but see also option -Zpass).
--           Therefore all include directives are ignored
--           unless one uses option <-I> to specify the
--           search path.
--
-- -I-       Marks the beginning of the system include path.
--           When an included file is given with angle brackets,
--           for instance, #include <stdio.h>, one only
--           searches directories specified by the <-I> options
--           that follow <-I->. Therefore all these include
--           directives are ignored unless one uses option <-I->
--           followed by one or more option <-I>.
--
-- -dM       Instead of producing the preprocessed file,
--           dumps all macros defined at the end of the parse.
--
-- -Zcppdef  Run the native preprocessor to initialize
--           the predefined macros (cpp -dM < dev/null).
--
-- -Zpass    This option should be enabled when the output
--           of cparser.cpp is passed to another preprocessor.
--           When this is enabled,
--             * the #pragma and #ident directives are copied
--               verbatim to the output,
--             * the #include directives are copied to the output when
--               the file cannot be found in the provided search path,
--             * any directive prefixed with a double ## is copied to
--               the output with a single #.  This last feature is
--               useful for #if directives that depend on symbols
--               defined by system files.
--
-- EXTENSIONS
--  This preprocessor implements several nonstandard features.  The
--  main feature are multiline macros. The other features are mostly
--  here because they make multiline macros more useful.
--
--  Multiline macros
--    Preprocessor directives #defmacro and #endmacro can be used to
--    define a function-like macro whose body spans several lines. The
--    #defmacro directive contains the macro name and a mandatory
--    argument list. The body of the macro is composed of all the
--    following lines up to the matching #endmacro. This offers
--    several benefits:
--
--      * The line numbers of the macro-expansion is preserved. This
--        ensures that the compiler produces error messages with
--        meaningful line numbers.
--
--      * The multi-line macro can contain preprocessor directives.
--        Conditional directives are very useful in this context.
--        Note however that preprocessor definitions (with <#define>,
--        <#defmacro>, or <#undef>) nested inside multiline macros are
--        only valid within the macro.
--
--      * The stringification <#> and token concatenation <##>
--        operators can be used freely in the body of multiline
--        macros.  Note that these operators only work with the
--        parameters of the multiline macros and not with ordinary
--        preprocessor definitions. This is consistent with the
--        standard behavior of these operators in ordinary
--        preprocessor macros.
--           Gory details: the values of the macro parameters are
--        normally macro-expanded before substituting them into the
--        text of the macro. However this macro-expansion does not
--        happen when the substitution occurs in the context of a
--        stringification or token concatenation operator.  All this
--        is consistent with the standard. The novelty is that this
--        macro-expansion does not occur either when the parameter
--        appears in a nested preprocessor directive or multiline
--        macro.
--           More gory details: the stringification operator only
--        works when the next non-space token is a macro parameter.
--        This provides a good way to distinguish a nested directive
--        from a stringification operator appearing in the beginning
--        of a line.
--
--  String comparison in conditional expressions
--    The standard specifies that the expressions following #if
--    directives are expressions of C integer type. However this
--    processor also handles strings. The only valid operations on
--    strings are the equality and ordering comparisons.
--      This is quite useful to special cases the parameters
--    of a multiline macro:
--
--      #defmacro DEFINE_DOT(TYPE)
--        #if #TYPE == "double"
--           // call blas code ...
--        #else
--           // direct implementation ...
--
--  Negative comma in variadic macros
--    Consider the following variadic macro
--
--      #define macro(msg, ...)  printf(msg, __VA_ARGS__)
--
--    The standard says that it is an error to call this
--    macro with only one argument. Calling this macro with
--    an empty second argument -- macro(msg,) -- leaves an
--    annoying comma in the expansion -- printf(msg,) --
--    and causes a compiler syntax error.
--      This preprocessor accepts invocations of such a macro with a
--    single argument. The value of parameter <__VA_ARGS__> is then a
--    so-called negative comma, meaning that the preceding comma is
--    eliminated when this parameter appears in the macro definition
--    between a comma and a closing parenthesis.
--
--  Recursive macros
--    When a new invocation of the macro appears in the expansion of a
--    macro, the standard specifies that the preprocessor must rescan
--    the expansion but should not recursively expand the macro.
--    Although this restriction is both wise and useful, there are
--    rare cases where one would like to use recursive macros.  As an
--    experiment, this recursion prevention feature is turned off when
--    one defines a multiline macro with #defrecursivemacro instead of
--    #defmacro. Note that this might prevent the preprocessor from
--    terminating unless the macro eventually takes a conditional
--    branch that does not recursively invoke the macro. 
--
--
-- PREDEFINED MACROS
--  The preprocessor always defines <__CPARSER__> to value 1.  The
--  value of symbols <__STDC__>, <__STDC_VERSION__>, <__GNUC__>,
--  <__GNUC_MINOR__> are either obtained using option <-Zcppdef> or
--  dependent on the C dialect selection option <-std=xxx>.
--  See function <initialDefines(options)> for all the details.
--
-- ==================================================


-- ==================================================
-- CPARSER.CPPTOKENITERATOR
--
-- SYNOPSIS
--  cppTokenIterator(<options>, <lines>, [<prefix>]])

-- SUMMARY
--  Argument <lines> is an iterator that produces code lines.  This
--  function returns an iterator that produces the tokens obtained by
--  preprocessing these code lines. Each call of the token iterator
--  returns a token type, a token string, and a location string. Space
--  tokens are filtered out. Therefore the token types are 'number',
--  'string', 'keyword', 'punctuator', and 'identifier'.  The location
--  string follows the pattern <"filename:linenumber">. Optional
--  argument <prefix> specifies the initial filename.  
--     See the description of <cparser.cpp> for the options
--  and extensions supported by the token iterator. Note that
--  option <-Zpass> is no longer on by default.
--
-- EXAMPLE
--  The following lua expression returns tokens for file <f>:
--
--  for typ,tok,n in cparser.cppTokenIterator({},io.lines(f),f) do
--      print(typ,tok,n)
--  end
--
-- ==================================================




---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
-- ALL UGLY HACKS SHOULD BE HERE


-- Sometimes we cannot find system include files but need to know at
-- least things about them. For instance, certain system include files
-- define alternate forms for keywords. 

local knownIncludeQuirks = {}

knownIncludeQuirks["<complex.h>"] = {
   "#ifndef complex",
   "# define complex _Complex",
   "#endif"
}

knownIncludeQuirks["<stdbool.h>"] = {
   "#ifndef bool",
   "# define bool _Bool",
   "#endif"
}




---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
-- TAGGED TABLES


-- Utilities to produce and print tagged tables.
-- The tag name is simply the contents of table key <tag>.
-- Function <newTag> returns a node constructor
--
-- Example:
--
-- > Foo = newTag('Foo')
-- > Bar = newTag('Bar')
--
-- > print( Foo{const=true,next=Bar{name="Hello"}} )
-- Foo{next=Bar{name="Hello"},const=true}
--
-- > print( Bar{name="hi!", Foo{1}, Foo{2}, Foo{3}} )
-- Bar{Foo{1},Foo{2},Foo{3},name="hi!"}

local function newTag(tag)
   -- the printing function
   local function tostr(self)
      local function str(x)
	 if type(x)=='string' then
	    return string.format("%q",x):gsub("\\\n","\\n")
	 elseif type(x)=='table' and not getmetatable(x) then
	    return "{..}"
	 else
	    return tostring(x)
	 end
      end
      local p = string.format("%s{", self.tag or "Node")
      local s = {}
      local seqlen = 0
      for i=1,#self do
	 if self[i] then seqlen=i else break end end
      for i=1,seqlen do
	 s[1+#s] = str(self[i]) end
      for k,v in pairs(self) do
	 if type(k) == 'number' then
	    if k<1 or k>seqlen then
	       s[1+#s] = string.format("[%s]=%s",k,str(v)) end
	 elseif type(k) ~= 'string' then
	    s.extra = true
	 elseif k:find("^_") then
	    -- hidden field
	 elseif k ~= 'tag' then
	    s[1+#s] = string.format("%s=%s",k,str(v)) end
      end
      if s.extra then s[1+#s] = "..." end
      return p .. table.concat(s,',') .. '}'
   end
   -- the constructor
   return function(t) -- must be followed by a table constructor
      t = t or {}
      assert(type(t)=='table')
      setmetatable(t, { __tostring=tostr } )
      t.tag = tag
      return t
   end
end

Node = newTag(nil) -- hack to print any table: print(Node(nn))


---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
-- UTILITIES


-- Many functions below have an optional argument 'options' which is
-- simply an array of compiler-like options that are specified in the
-- toplevel call and passed to nearly all functions. This function
-- provides tests whether a particular option has been given.

local function hasOption(options, opt, rehash)
   if not options then
      return false
   elseif rehash or not options.hash then
      options.hash = {}
      for i,v in ipairs(options) do
	 options.hash[v] = i
      end
   end
   return options.hash[opt]
end

-- The above function adds a field 'hash' into the options array.
-- Over time, more and more such additions proved useful.
-- The following function is called at the beginning of
-- the user facing functions to make a copy of the
-- user provided option array and avoid messing it up.

local function copyOptions(options)
   options = options or {}
   assert(type(options)=='table')
   local noptions = {}
   for i,v in ipairs(options) do noptions[i]=v end
   return noptions
end



-- Generic functions for error messages

local function xmessage(err, options, lineno, message, ...)
   local msg = string.format("cparser: (%s) ",lineno)
   msg = msg .. string.format(message,...)
   if options.silent then
      if err == 'error' then error(message, 0) end
   else
      if err == 'warning' and hasOption(options, "-Werror") then err = 'error' end
      if err == 'error' or not hasOption(options, "-w") then print(msg) end
      if err == 'error' then error("cparser: aborted",0) end
   end
end

local function xwarning(options, lineno, message, ...)
   xmessage('warning', options, lineno, message, ...)
end

local function xerror(options, lineno, message, ...)
   xmessage('error', options, lineno, message, ...)
end

local function xassert(cond, ...)
   if not cond then xerror(...) end
end

local function xdebug(lineno,message,...)
   local msg = string.format("\t\t[%s] ", lineno)
   msg = msg .. string.format(message,...)
   print(msg)
end

   
-- Nil-safe max

local function max(a,b)
   a = a or b
   b = b or a
   return a > b and a or b
end

-- Deep table comparison
-- (not very efficient, no loop detection)

local function tableCompare(a,b)
   if a == b then
      return true
   elseif type(a) == 'table' and type(b) == 'table' then
      for k,v in pairs(a) do
	 if not tableCompare(v,b[k]) then return false end
      end
      for k,v in pairs(b) do
	 if not tableCompare(a[k],v) then return false end
      end
      return true
   else
      return false
   end
end


-- Evaluate a lua expression, return nil on error.

local function evalLuaExpression(s)
   assert(type(s)=='string')
   local f = load(string.gmatch(s,".*"))
   local function r(status,...) 
      if status then return ... end end
   return r(pcall(f or error))
end

-- Bitwise manipulations
-- try lua53 operators otherwise revert to iterative version

local bit = evalLuaExpression [==[
   local bit = {} 
   function bit.bnot(a) return ~a end
   function bit.bor(a,b) return a | b end
   function bit.band(a,b) return a & b end
   function bit.bxor(a,b) return a ~ b end
   function bit.lshift(a,b) return a < 0 and b < 0 and ~((~a) << b) or a << b end
   return bit 
]==]

if not bit then 
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


-- Coroutine helpers.
-- This code uses many coroutines that yield lines or tokens.
-- All functions that can yield take an options table as first argument.

-- Wrap a coroutine f into an iterator
-- The options and all the extra arguments are passed
-- to the coroutine when it starts

local function wrap(options, f, ...)
   local function g(...) coroutine.yield(nil) f(...) end
   local c = coroutine.create(g)
   coroutine.resume(c, options, ...)
   local function r(s,...)
      if not s then local m = ... ; error(m, 0) end
      return ...
   end
   return function()
      if coroutine.status(c) ~= 'dead' then
	 return r(coroutine.resume(c))
      end
   end
end

-- Collect coroutine outputs into an array
-- The options and the extra arguments are passed to the coroutine.

local function callAndCollect(options, f, ...)
   local collect = {}
   for s in wrap(options, f, ...) do
      collect[1+#collect] = s
   end
   return collect
end

-- Yields all outputs from iterator iter.
-- This function unpacks table values and yields the result.
-- For all other values, this function yields the value plut its extra arguments.
-- Argument options is ignored.

local function yieldFromIterator(options, iter)
   local function yes(v,...) coroutine.yield(v,...) return v end
   while yes(iter()) do end
end

-- Yields all values from array <arr>.
-- This function unpacks table values and yields the result.
-- For all other values, this function yields the value plut its extra arguments.
-- Argument options is ignored.

local function yieldFromArray(options, arr, ...)
   for i,v in ipairs(arr) do
      if type(v) == 'table' then
	 coroutine.yield(unpack(v))
      else
	 coroutine.yield(v, ...)
      end
   end
end





---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
-- INITIAL PREPROCESSING 



-- A routine that pulls lines from a line iterator
-- and yields them together with a location
-- composed of the optional prefix, a colon, and a line number.
-- Argument options is ignored.
-- Lua provides good line iterators such as:
--   io.lines(filename) filedesc:lines()  str:gmatch("[^\n]+")


local function yieldLines(options,lineIterator,prefix)
   prefix = prefix or ""
   assert(type(prefix)=='string')
   local n = 0
   for s in lineIterator do
      n = n + 1
      coroutine.yield(s, string.format("%s:%d", prefix, n))
   end
end


-- A routine that obtain lines from coroutine <lines>,
-- joins lines terminated by a backslash, and yield the
-- resulting lines. The coroutine is initialized with
-- argument <options> and all extra arguments.
-- Reference: https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html (3)

local function joinLines(options, lines, ...)
   local li = wrap(options, lines, ...)
   for s, n in li do
      while type(s) == 'string' and s:find("\\%s*$") do
	 local t = li() or ""
	 s = s:gsub("\\%s*$", "") .. t
      end
      coroutine.yield(s, n)
   end
end


-- A routine that obtain lines from coroutine <lines>, eliminate the
-- comments and yields the resulting lines.  The coroutine is
-- initialized with argument <options> and all extra arguments.
-- Reference: https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html (4)

local function eliminateComments(options, lines, ...)
   local lineIterator = wrap(options, lines, ...)
   local inComment = false
   local s,n = lineIterator()
   while type(s) == 'string' do
      while inComment do
	 local p = s:find("%*/")
	 if p ~= nil then
	    inComment = false
	    s = s:sub(p+2)
	 else
	    local m = n
	    s, n = lineIterator()
	    xassert(s ~= nil, options, m, "Unterminated comment")
	 end
      end
      local inString = false
      local q = s:find("[\'\"\\/]", 1)
      while q ~= nil do
	 if hasOption(options,"-d:comments") then 
	    xdebug(n, "comment: [%s][%s] %s",s:sub(1,q-1),s:sub(q),inString)
	 end
	 local c = s:byte(q)
	 if inString then
	    if c == 92 then -- \
	       q = q + 1
	    elseif c == inString then
	       inString = false
	    end
	 else
	    if c == 34 or c == 39 then -- " or '
	       inString = c
	    elseif c == 47 and s:byte(q+1) == 47 then -- "//"
	       s = s:sub(1,q-1)
	    elseif c == 47 and s:byte(q+1) == 42 then -- "/*"
	       local p = s:find("%*/",q+2)
	       if p ~= nil then
		  s = s:sub(1,q-1) .. " " .. s:sub(p+2)
	       else
		  s = s:sub(1,q-1)
		  inComment = true
	       end
	    end
	 end
	 q = s:find("[\'\"\\/]", q+1)
      end
      coroutine.yield(s, n)
      s, n = lineIterator()
   end
end



---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
-- TOKENIZER



local keywordTable = {
   ------ Standard keywords
   "auto", "break", "case", "char", "const", "continue", "default", "do",
   "double", "else", "enum", "extern", "float", "for", "goto", "if", "int",
   "long", "register", "return", "short", "signed", "sizeof", "static", "struct",
   "switch", "typedef", "union", "unsigned", "void", "volatile", "while",
   ------ All keywords starting with underline
   "__declspec", "__attribute__", "__asm__",
   "_Complex", "__inline__", "__restrict__",
   ------ Nonstandard or dialect specific keywords do not belong here.
}
   
local punctuatorTable = {
   "+", "-", "*", "/", "%", "&", "|", "^", ">>", "<<", "~",
   "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", ">>=", "<<=",
   "(", ")", "[", "]", "{", "}", "++", "--",
   "==", "!=", ">=", "<=", ">", "<", "&&", "||", "!",
   ".", "->", "*", "&", "?", ":", "::", "->*", ".*", ";", ",",
   "#", "##", "..." -- preprocessor stuff
}

local keywordHash = {}
for i,v in ipairs(keywordTable) do
   keywordHash[v] = true
end

local punctuatorHash = {}
for i,v in ipairs(punctuatorTable) do
   local l = v:len()
   local b = v:byte()
   punctuatorHash[v] = true
   punctuatorHash[b] = max(l,punctuatorHash[b])
end


-- The following functions test the types of the tokens returned by the tokenizer.
-- They should not be applied to arbitrary strings

local function isSpace(tok)
   return tok and tok:find("^%s") ~= nil end
local function isNewline(tok) -- Subtype of space
   return tok and tok:find("^\n") ~= nil end 
local function isNumber(tok)
   return tok and tok:find("^[.0-9]") ~= nil end
local function isString(tok)
   if tok then return tok:find("^[\'\"]") ~= nil
      or tok:find("^<") and tok:find(">$") end end
local function isPunctuator(tok)
   return tok and punctuatorHash[tok] ~= nil end
local function isIdentifier(tok)
   return tok and tok:find("^[A-Za-z_$]") ~= nil end
local function isKeyword(tok) -- Subtype of identifier
   return tok and keywordHash[tok] ~= nil end
local function isName(tok) -- Subtype of identifier
   return tok and isIdentifier(tok) and not keywordHash[tok] end
   
local function tokenType(tok, withKeyword)
   if isNewline(tok) then return 'newline'
   elseif isSpace(tok) then return 'space'
   elseif isNumber(tok) then return 'number'
   elseif withKeyword and isKeyword(tok) then return 'keyword'
   elseif isPunctuator(tok) then return 'punctuator'
   elseif isIdentifier(tok) then return 'identifier'
   elseif isString(tok) then return 'string'
   else return nil end
end


-- The tokenizeLine() function takes a line, splits it into tokens,
-- and yields tokens and locations. The number tokens are the weird
-- preprocessor numbers defined by ansi c. The string tokens include
-- character constants and angle-bracket delimited strings occuring
-- after an include directive. Every line begins with a newline
-- token giving the proper indentation. All subsequent spaces
-- are reduced to a single space character.

local function tokenizeLine(options, s, n, notNewline)
   -- little optimization for multiline macros
   -- s may be an array of precomputed tokens
   if type(s) == 'table' then
      return yieldFromArray(options, s, n)
   end
   -- normal operation
   assert(type(s) == 'string')
   local p = s:find("[^%s]")
   -- produce a newline token
   if p and not notNewline then
      local r = '\n' .. s:sub(1,p-1)
      coroutine.yield(r, n)
   end
   -- produce one token
   local function token()
      local b, l, r
      if hasOption(options, "-d:tokenize") then
	 xdebug(n, "[%s][%s]",s:sub(1,p-1),s:sub(p))
      end
      -- space
      l = s:find("[^%s]", p)
      if l == nil then
	 return nil
      elseif l > p then
	 p = l
	 return " ", n
      end
      -- identifier
      r = s:match("^[a-zA-Z_$][a-zA-Z0-9_$]*", p)
      if r ~= nil then
	 p = p + r:len()
	 return r, n
      end
      -- preprocessor numbers
      r = s:match("^%.?[0-9][0-9a-zA-Z._]*", p)
      if r ~= nil then
	 l = r:len()
	 while r:find("[eEpP]$") and s:find("^[-+]", p+l) do
	    r = r .. s:match("^[-+][0-9a-zA-Z._]*", p+l)
	    l = r:len()
	 end
	 p = p + l
	 return r, n
      end
      -- angle-delimited strings in include directives
      b = s:byte(p)
      if b == 60 and s:find("^%s*#%s*include") then
	 r = s:match("^<[^>]+>", p)
	 if r ~= nil then
	    p = p + r:len()
	    return r, n
	 end
      end
      -- punctuator
      l = punctuatorHash[b]
      if l ~= nil then
	 while l > 0 do
	    r = s:sub(p,p+l-1)
	    if punctuatorHash[r] then
	       p = p + l
	       return r, n
	    end
	    l = l - 1
	 end
      end
      -- string
      if b == 34 or b == 39 then -- quotes
	 local q = p
	 repeat
	    q = s:find("[\'\"\\]", q + 1)
	    l = s:byte(q)
	    xassert(q ~= nil, options, n, "Unterminated string or character constant")
	    if l == 92 then
	       q = q + 1
	    end
	 until l == b
	 r = s:sub(p,q)
	 p = q + 1
	 return r, n
      end
      -- error
      xerror(options, n,"Unrecognized character (%s)", s:sub(p))
   end
   -- loop
   if p then
      for tok,n in token do
	 coroutine.yield(tok, n)
      end
   end
end

-- Obtain lines from coroutine <lines>,
-- and yields their tokens. The coroutine is initialized with
-- argument <options> and all extra arguments.

local function tokenize(options, lines, ...)
   for s,n in wrap(options, lines, ...) do
      tokenizeLine(options, s, n)
   end
end


---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
-- PREPROCESSING 


-- Preprocessing is performed by two coroutines. The first one
-- processes all the preprocessor directives and yields the remaining
-- lines. The second one processes tokens from the remaining lines and
-- perform macro expansions. Both take a table of macro definitions as
-- argument. The first one writes into the table and the second one
-- reads from it.

-- Each macro definition is simply an array of tokens.
-- If the macro takes arguments (function-like macro),
-- the table also contains an entry 'args' with a list
-- of argument names. Finally the table may contain
-- a function which is called at macro-expansion time.
-- This provides for complicated situations.

-- forward declarations
local function expandMacros() end
local function processDirectives() end


-- Starting with the second coroutine which takes a token producing
-- coroutine and yields the preprocessed tokens. Argument macros is
-- the macro definition table.

expandMacros = function(options, macros, tokens, ...)
   local ti = wrap(options, tokens, ...)
   local tok, n = ti()
   -- redefine ti() to ensure tok,n remain up-to-date
   local ti = function() tok,n=ti() return tok,n end
   -- create a macro table that inherits <macros> but hides <symbol>
   local function hideMacro(macros,symbol,nargs)
      local nmacros = {}
      setmetatable(nmacros, {__index=macros})
      if macros[symbol] and macros[symbol].recursive then
	 -- limit number of recursive macro invocations
	 if not options.maxrecursion then
	    for i,v in ipairs(options) do
	       if type(v)=='string' and v:find("^%-Zmaxrecursion=") then
		  options.maxrecursion = tonumber(v:match("-Zmaxrecursion=%s*(%d+)%s*$"))
	       end
	    end
	 end
	 -- we use macros[0] to keep track of the recursion count.
	 -- this is not going to collide with any macro symbol because 0 is a number.
	 local maxrecursion = options.maxrecursion or 100
	 nmacros[0] = 1 + (macros[0] or 0) 
	 xassert(nmacros[0] < maxrecursion, options, n,
	 	 "more than %d recursive macro invocations", maxrecursion)
      else
	 -- standard recursion prevention feature
	 nmacros[symbol] = false
      end
      return nmacros
   end
   -- collect one macro arguments into an array
   -- stop when reaching a closing parenthesis or a comma 
   local function collectArgument(ti, varargs)
      local count = 0
      local tokens = {}
      local tok = ti()
      while isSpace(tok) do
	 tok = ti()
      end
      while tok do
	 if tok == ')' and count == 0 then
	    break
	 elseif tok == ')' then
	    count = count - 1
	 elseif tok == '(' then
	    count = count + 1
	 elseif tok == ',' and count == 0 and not varargs then
	    break
	 end
	 if isSpace(tok) then tok = " " end
	 tokens[1+#tokens] = tok
	 tok = ti()
      end
      if #tokens > 0 and isSpace(tokens[#tokens]) then
	 tokens[#tokens] = nil
      end
      return tokens
   end
   -- collects all macro arguments
   local function collectArguments(ti,args,ntok,nn)
      local nargs = { [0]={} }
      for i,name in ipairs(args) do
	 if tok == ')' and name == "__VA_ARGS__" then
	    nargs[0][name] = { negComma=true }
	    nargs[name] = { negComma=true }
	 else
	    xassert(tok=='(' or tok==',', options, nn, "not enough arguments for macro '%s'", ntok)
	    local arg = collectArgument(ti, name == "__VA_ARGS__")
	    nargs[0][name] = arg
	    nargs[name] = callAndCollect(options, expandMacros, macros, yieldFromArray, arg, nn)
	 end
      end
      xassert(tok, options, nn, "unterminated arguments for macro '%s'", tok)
      xassert(tok==')', options, nn, "too many arguments for macro '%s'", tok)
      return nargs
   end
   -- coroutine that substitute the macro arguments
   -- and stringification and concatenation are handled here
   local function substituteArguments(options, def, nargs, n, inDirective)
      local uargs = nargs[0] -- unprocessed values
      if inDirective then nargs = uargs end  -- always use unprocessed in directives
      -- prepare loop
      local i,j,k = 1,1,1
      while def[i] do
	 if isSpace(def[i]) then
	    -- copy spaces
	    coroutine.yield(def[i], n)
	 else
	    -- positions j and k on next non-space tokens
	    local function updateJandK()
	       if j <= i then j=i
		  repeat j=j+1 until def[j] == nil or not isSpace(def[j]) end
	       if k <= j then k=j
		  repeat k=k+1 until def[k] == nil or not isSpace(def[k]) end
	    end
	    updateJandK()
	    -- alternatives
	    if def[i]=='#' and def[j] and nargs[def[j]] then
	       -- stringification
	       local v = def[j]
	       local s = string.format("%q", table.concat(uargs[v])):gsub("\\\n","\\n")
	       coroutine.yield(s, n)
	       i = j
	    elseif def[j]=='##' and def[k] and not inDirective then
	       -- concatenation
	       local u = {}
	       local function addToU(s)
		  if nargs[s] then for i,v in ipairs(uargs[s]) do u[1+#u] = v end
		  else u[1+#u]=s end
	       end
	       addToU(def[i])
	       while def[j] == '##' and def[k] do
		  addToU(def[k])
		  i = k
		  updateJandK()
	       end
	       tokenizeLine(options, table.concat(u), n, true)
	    elseif def[i]==',' and def[j]=='__VA_ARGS__' and def[k]==')'
	       and nargs[def[j]].negComma then
	       -- negative comma
	       i = j
	    elseif nargs[def[i]] then
	       -- substitution
	       yieldFromArray(options, nargs[def[i]], n)
	    else
	       -- copy
	       coroutine.yield(def[i], n)
	    end
	 end
	 i = i + 1
      end
   end
   -- main loop
   local newline, directive = true, false
   while tok ~= nil do
      -- detects Zpassed directives
      if newline and tok == '#' then
	 newline, directive = false, true
      elseif not isSpace(tok) then
	 newline = false
      elseif isNewline(tok) then
	 newline, directive = true, false
      end
      -- process code
      local def = macros[tok]
      if not def or directive then
	 -- not a macro
	 coroutine.yield(tok, n)
      elseif type(def) == 'function' then
	 -- magic macro
	 def(ti,tok,n)
      elseif def.args == nil then
	 -- object-like macro
	 local nmacros = hideMacro(macros,tok)
	 expandMacros(options, nmacros, yieldFromArray, def, n)
      elseif def.lines == nil then
	 -- single-line function-like macro
	 local ntok,nn = tok,n
	 repeat ti() until not isSpace(tok)
	 if (tok ~= '(') then
	    coroutine.yield(ntok, nn)
	    coroutine.yield(tok, nn)
	 else
	    local nmacros = hideMacro(macros,ntok)
	    local nargs = collectArguments(ti,def.args,ntok,nn)
	    expandMacros(options, nmacros, substituteArguments, def, nargs, nn)
	 end
      else
	 -- multi-line function-like macro
	 local ntok,nn = tok,n
	 repeat ti() until not isSpace(tok)
	 if (tok ~= '(') then
	    coroutine.yield(ntok, nn)
	    coroutine.yield(tok, nn)
	 else
	    local lines = def.lines
	    local nargs = collectArguments(ti,def.args,ntok,nn)
	    local nmacros = hideMacro(macros,ntok,nargs)
	    -- a coroutine that yields the macro definition
	    local function yieldMacroLines()
	       local count = 0
	       for i=1,#lines,2 do
		  local ls,ln = lines[i], lines[i+1]
 		  -- are we possibly in a cpp directive
		  local dir = false
		  if ls[2] and ls[2]:find('^#') then
		     dir = isIdentifier(ls[3]) and ls[3] or ls[4]
		  end
		  if dir and nargs[dir] then
		     dir = false      -- leading stringification
		  elseif dir == 'defmacro' then
		     count = count + 1  -- entering a multiline macto
		  elseif dir == 'endmacro' then
		     count = count - 1  -- leaving a multiline macro
		  end
		  dir = dir or count > 0
		  -- substitute
		  --print(string.format("<<< [%s]",table.concat(ls,"|",2)))
		  ls = callAndCollect(options,substituteArguments,ls,nargs,ln,dir)
		  --print(string.format(">>> [%s]",table.concat(ls,"|",2)))
		  -- compute lines (optimize speed by passing body lines as tokens)
		  if ls[2] and ls[2]:find("^#") then -- but not directives
		     ls = ls[1]:sub(2) .. table.concat(ls, nil, 2)
		  end
		  coroutine.yield(ls,ln)
	       end
	    end
	    -- reenter the preprocessing subroutines
	    expandMacros(options, nmacros,
			 tokenize, processDirectives, nmacros,
			 yieldMacroLines)
	 end
      end
      ti()
   end
end


-- Processing conditional directive requires evaluating conditions
-- This function takes an iterator on preprocessed expression tokens
-- and computes the value. This does not handle defined(X) expressions.
-- Optional argument resolver is a function that takes an indentifer
-- name and returns a value. Otherwise zero is assumed

local function evaluateCppExpression(options, tokenIterator, n, resolver)
   -- redefine token iterator to skip spaces and update tok
   local tok
   local function ti()
      repeat tok = tokenIterator()
      until not isSpace(tok) return tok
   end
   -- operator tables
   local unaryOps = {
      ["!"] = function(v) return v == 0 and 1 or 0 end,
      ["~"] = function(v) return bit.bnot(v) end,
      ["+"] = function(v) return v end,
      ["-"] = function(v) return -v end
   }
   local binaryOps = {
      ["*"] = function(a,b) return a * b end,
      ["/"] = function(a,b) xassert(b~=0,options,n,"division by zero"); return math.floor(a / b) end,
      ["%"] = function(a,b) xassert(b~=0,options,n,"division by zero"); return a % b end,
      ["+"] = function(a,b) return a + b end,
      ["-"] = function(a,b) return a - b end,
      [">>"] = function(a,b) return bit.lshift(a, -b) end,
      ["<<"] = function(a,b) return bit.lshift(a, b) end,
      [">="] = function(a,b) return a >= b and 1 or 0 end,
      ["<="] = function(a,b) return a <= b and 1 or 0 end,
      [">"] = function(a,b) return a > b and 1 or 0 end,
      ["<"] = function(a,b) return a < b and 1 or 0 end,
      ["=="] = function(a,b) return a == b and 1 or 0 end,
      ["!="] = function(a,b) return a ~= b and 1 or 0 end,	 
      ["&"] = function(a,b) return bit.band(a,b) end,
      ["^"] = function(a,b) return bit.bxor(a,b) end,
      ["|"] = function(a,b) return bit.bor(a,b) end,
      ["&&"] = function(a,b) return (a ~= 0 and b ~= 0) and 1 or 0 end,
      ["||"] = function(a,b) return (a ~= 0 or b ~= 0) and 1 or 0 end,
   }
   local binaryPrec = {
      ["*"] = 1, ["/"] = 1, ["%"] = 1,
      ["+"] = 2, ["-"] = 2,
      [">>"] = 3, ["<<"] = 3,
      [">="] = 4, ["<="] = 4, ["<"] = 4, [">"] = 4,
      ["=="] = 5, ["!="] = 5,
      ["&"] = 6, ["^"] = 7, ["|"] = 8,
      ["&&"] = 9, ["||"] = 10
   }
   -- forward
   local function evaluate() end
   -- unary operations
   local function evalUnary()
      if unaryOps[tok] then
	 local op = unaryOps[tok]
	 ti(); return op(evalUnary())
      elseif tok == '(' then
	 ti(); local v = evaluate()
	 xassert(tok == ')', options, n, "missing closing parenthesis")
	 ti(); return v
      elseif tok == 'defined' then -- magic macro should have removed this
	 xerror(options, n, "syntax error after <defined>")
      elseif isIdentifier(tok) then
         local v = type(resolver) == 'function' and resolver(tok,ti)
	 ti(); return v or 0
      elseif isNumber(tok) then
	 local v = tok:gsub("[ULul]+$","")
	 if v:find("^0[0-7]+$") then
	    v = tonumber(v,8) -- octal
	 elseif v:find("^0[bB][01]+") then
	    v = tonumber(v:sub(3),2) -- binary
	 else
	    v = tonumber(v) -- lua does the rest
	 end
	 xassert(v and v == math.floor(v), options, n, "syntax error (invalid integer '%s')", tok)
	 ti(); return v
      elseif isString(tok) then
	 xassert(tok:find("^[\'\"]"), options, n, "syntax error (invalid value '%s')", tok)
	 local v = tok
	 if v:find("^'") then -- interpret character constant as number
	    v = evalLuaExpression(string.format("return string.byte(%s)", tok))
	    xassert(type(v)=='string', options, n, "syntax error (invalid value '%s')", tok)
	    v = v:byte()
	 end
	 ti(); return v
      end
      xerror(options, n, "syntax error (invalid value '%s')", tok)
   end
   -- binary operations
   local function evalBinary(p)
      if p == 0 then
	 return evalUnary()
      else
	 local val = evalBinary(p-1)
	 while binaryPrec[tok] == p do
	    local op = binaryOps[tok]  ti()
	    local oval = evalBinary(p-1)
	    xassert(p==4 or p==5 or type(val)=='number', options, n,
		    "expression uses arithmetic operators on strings")
	    xassert(type(val) == type(oval), options, n,
		    "expression compares numbers and strings")
	    val = op(val,oval)
	 end
	 return val
      end
   end
   -- eval ternary conditonal
   local function evalTernary()
      local c = evalBinary(10)
      if tok ~= '?' then return c end
      ti() local v1 = evalBinary(10)
      xassert(tok == ':', options, n, "expecting ':' after '?'")
      ti() local v2 = evalBinary(10)
      if c == 0 then return v2 else return v1 end
   end

   -- actual definition of evaluate
   evaluate = function()
      return evalTernary()
   end
   -- main function
   ti()
   xassert(tok, options, n, "constant expression expected");
   local result = evaluate()
   if hasOption(options, "-d:eval") then
      xdebug(n, "eval %s", result)
   end
   -- warn about garbage when called from cpp (but not when called with resolver)
   xassert(resolver or not tok, options, n, "garbage after conditional expression");
   return result
end

   

-- Now dealing with the coroutine thay processes all directives.
-- This coroutine obtains lines from coroutine <lines>,
-- processes all directives, and yields remaining lines

processDirectives = function(options, macros, lines, ...)
   local li = wrap(options, lines, ...)
   local s, n = li()
   -- redefine li to make sure vars s and n are up-to-date
   local li = function() s,n=li() return s,n end
   -- directives store their current token in these vars
   local dirtok, tok, spc
   -- forward declaration
   local function processLine(okElif) end
   -- simple directives
   local function doIgnore(ti)
      if hasOption(options, "-Zpass") then coroutine.yield(s, n) end
   end
   local function doError(ti)
      xerror(options, n, "unexpected preprocessor directive #%s", dirtok)
   end
   local function doMessage()
      local msg = s:match("^%s*#+%s*[a-z]*%s+([^%s].*)")
      xmessage(dirtok, options, n, msg or '#' .. dirtok)
   end
   -- undef
   local function doUndef(ti)
      ti()
      xassert(isIdentifier(tok), options, n, "symbol expected after #undef")
      if hasOption(options, "-d:defines") then xdebug(n, "undef %s", tok) end
      macros[tok] = false -- intentional
      if ti() then xwarning(options, n, "garbage after #undef directive") end
   end
   -- define
   local function getMacroArguments(ti)
      local args = {}
      local msg = "argument list in function-like macro"
      ti()
      while tok and tok ~= ')' do
	 local nam = tok
	 ti()
	 xassert(nam ~= "__VA_ARGS__", options, n, "name __VA_ARGS__ is not allowed here")
	 xassert(tok == ')' or nam ~= '...', options, n, "ellipsis in argument list must appear last")
	 xassert(tok == ')' or tok == ',', options, n, "bad " .. msg)
	 if tok == ',' then ti() end
	 if nam == '...' then nam = "__VA_ARGS__" end
	 xassert(isIdentifier(nam), options, n, "bad " .. msg)
	 args[1+#args] = nam
      end
      xassert(tok == ')', options, n, "unterminated " .. msg)
      ti()
      return args
   end
   local function doDefine(ti)
      xassert(isIdentifier(ti()), options, n, "symbol expected after #define")
      local nam, args = tok, nil
      -- collect arguments
      if ti() == '(' and not spc then
	 args = getMacroArguments(ti)
      end
      -- collect definition
      local def = { tok, args = args }  
      while ti(true) do
	 def[1+#def] = tok
      end 
      -- define macro
      if macros[nam] and not tableCompare(def,macros[nam]) then
	 xwarning(options, n,"redefinition of preprocessor symbol '%s'", nam)
      end
      if hasOption(options, "-d:defines") then
	 if args then args = "(" .. table.concat(args,",") .. ")" else args = "" end
	 xdebug(n, "define %s%s = %s", nam, args, table.concat(def,' '))
      end
      macros[nam] = def
      -- this hack is used to capture integer macro definitions in the declarationparser
      -- index 1 will not collide with any symbol because it is a number.
      -- see function processMacroCapture in parseDeclarations below.
      local captureTable = rawget(macros,1)
      if args == nil and type(captureTable) == 'table' then
	 local i = 0
	 local v = callAndCollect(options, expandMacros, macros, yieldFromArray, def, n)
	 local function ti() i=i+1 return v[i] end
	 local s,r = pcall(evaluateCppExpression,{silent=true}, ti, n, error)
	 if s and type(r)=='number' then captureTable[nam] = {name=nam,intval=r,where=n} end
      end
   end
   -- defmacro
   local function checkDirective(stop)
      xassert(s, options, n, "unterminated macro (missing #%s)", stop)
      local r = type(s) == 'string' and s:match("^%s*#%s*([a-z]+)")
      if r == "endmacro" or r == "endif" then
	 if s:find(r .. "%s*[^%s]") then
	    xwarning(options, n, "garbage after #%s directive", r)
	 end
      end
      return r
   end
   local function doMacroLines(lines, stop)
      while true do
	 li()
	 local s = callAndCollect(options,tokenizeLine,s,n)
	 if #s > 0 then lines[1+#lines] = s lines[1+#lines] = n end
	 local r = checkDirective(stop)
	 if r == "endmacro" or r == "endif" then
	    xassert(r==stop,options,n, "unbalanced directives (got #%s instead of #%s)",r,stop)
	    return r
	 elseif r=="defmacro" then
	    doMacroLines(lines,"endmacro")
	 elseif r == "if" or r == "ifdef" or r == "ifndef" then
	    doMacroLines(lines,"endif")
	 end
      end
   end
   local function doDefmacro(ti)
      xassert(isIdentifier(ti()), options, n, "symbol expected after #defmacro")
      local nam,nn = tok,n
      xassert(ti()=='(', options, n, "argument list expected in #defmacro")
      local args = getMacroArguments(ti)
      xassert(not tok, options, n, "garbage after argument list in #defmacro")
      -- collect definition
      local lines = {}
      local def = { args=args, lines=lines, recursive=(dirtok=="defrecursivemacro") }
      local r = doMacroLines(lines,"endmacro")
      lines[#lines] = nil
      lines[#lines] = nil
      if hasOption(options,"-d:directives") then
	 xdebug(n, "directive: #endmacro")
      end
      if macros[nam] and not tableCompare(def,macros[nam]) then
	 xwarning(options, n,"redefinition of preprocessor symbol '%s'", nam)
      end
      if hasOption(options, "-d:defines") then
	 xdebug(nn, "defmacro %s(%s) =", nam, table.concat(args,','))
	 for i=1,#lines,2 do
	    xdebug(lines[i+1], "\t%s", table.concat(lines[i]):gsub("^\n","")) end
      end
      macros[nam] = def
   end
   -- include
   local function doInclude(ti)
      -- get filename
      local pti = wrap(options, expandMacros, macros, yieldFromIterator, ti)
      local tok = pti()
      while isSpace(tok) do tok=pti() end
      xassert(isString(tok) and tok:find("^[\"<]"), options, n, "string expected after #include")
      if pti() then xwarning(options, n, "garbage after #include directive") end
      -- interpret filename
      local sys = tok:byte() == 60
      local min = dirtok=="include_next" and options.includedir or 0
      local fname = evalLuaExpression(string.format("return '%s'", tok:sub(2,-2)))
      local pname, fd, fdi
      for i,v in ipairs(options) do
	 if v == "-I-" then
	    sys=false
	 elseif i > min and v:find("^%-I") and not sys then
            pname = v:match("^%-I%s*(.*)") .. '/' .. fname
	    fdi, fd = i, io.open(pname, "r")
	    if fd then break end
	 end
      end
      if fd then
	 -- include file
	 if hasOption(options, "-d:include") then xdebug(n, "including %q", pname) end
	 local savedfdi = options.includedir
	 options.includedir = fdi -- saved index to implement include_next
	 processDirectives(options, macros, eliminateComments, joinLines,
			   yieldLines, fd:lines(), pname)
	 options.includedir = savedfdi
      elseif hasOption(options, "-Zpass") then
	 -- include file not found: pass preprocessed directive
	 coroutine.yield(string.format("#include %s",tok), n)
      elseif knownIncludeQuirks[tok] then
	 -- include file not found but known quirks
	 xwarning(options, n, "include directive (%s) was unresolved but has known quirks", tok)
	 processDirectives(options, macros, eliminateComments,
			   yieldFromArray, knownIncludeQuirks[tok], n)
      else
	 -- include file not found but with known quirks
	 xwarning(options, n, "include directive (%s) was unresolved", tok)
      end
   end
   -- conditionals
   local function doConditionalBranch(execute)
      local r = checkDirective("endif")
      while true do
	 li()
	 local r = checkDirective("endif")
	 if r == "else" or r == "elif" or r == "endif" then
	    return r
	 elseif execute then
	    processLine()
	 elseif r == "if" or r == "ifdef" or r == "ifndef" then
	    while doConditionalBranch(false) ~= "endif" do end
	 end
      end
   end
   local function doConditional(result)
      local r = doConditionalBranch(result)
      if r == 'elif' and not result then
	 return processLine(true)
      end
      while r ~= "endif" do
	 r = doConditionalBranch(not result)
      end
      if hasOption(options,"-d:directives") then
	 xdebug(n, "directive: %s",s)
      end
   end
   local function doIfdef(ti)
      ti()
      xassert(isIdentifier(tok), options, n, "symbol expected after #%s", dirtok)
      local result = macros[tok]
      if ti() then xwarning(options, n, "garbage after #undef directive") end
      if dirtok == 'ifndef' then result = not result end
      doConditional(result)
   end
   local function doIf(ti)
      -- magic macro for 'defined'
      local nmacros = {}
      setmetatable(nmacros,{__index=macros})
      nmacros['defined'] = function(ti)
	 local tok,n = ti()
	 if tok == '(' then tok = ti()
	    if ti() ~= ')' then tok = nil end end
	 if isIdentifier(tok) then
	    coroutine.yield(macros[tok] and "1" or "0", n)
	 else
	    coroutine.yield("defined", n) -- error
	 end
      end
      -- evaluate and branch
      local pti = wrap(options, expandMacros, nmacros, yieldFromIterator, ti)
      local result = evaluateCppExpression(options, pti, n)
      doConditional(result ~= 0)
   end
   -- table of directives
   local directives = {
      ["else"] = doError, ["elif"] = doError, ["endif"] = doError,
      ["pragma"] = doIgnore, ["ident"] = doIgnore, ["line"] = doIgnore,
      ["error"] = doMessage, ["warning"] = doMessage,
      ["if"] = doIf, ["ifdef"] = doIfdef, ["ifndef"] = doIfdef,
      ["define"] = doDefine, ["undef"] = doUndef,
      ["defmacro"] = doDefmacro, ["defrecursivemacro"] = doDefmacro,
      ["endmacro"] = doError, 
      ["include"] = doInclude, ["include_next"] = doInclude,
   }
   -- process current line
   processLine = function(okElif)
      if type(s) == 'table' then
	 -- optimization for multiline macros:
	 -- When s is an an array of precomputed tokens, code is assumed.
	 coroutine.yield(s, n)
      elseif not s:find("^%s*#") then
	 -- code
	 coroutine.yield(s, n)
      elseif s:find("^%s*##") and hasOption(options, "-Zpass") then
	 -- pass 
	 local ns = s:gsub("^(%s*)##","%1#")
	 coroutine.yield(ns, n)
      else
	 if hasOption(options, "-d:directives") then
	    xdebug(n, "directive: %s",s)
	 end
	 -- tokenize directive
	 local ti = wrap(options, tokenizeLine, s, n)
	 -- a token iterator that skips spaces unless told otherwise
	 local ti = function(keepSpaces)
	    tok = ti()
	    spc = isSpace(tok)
	    while isSpace(tok) and not keepSpaces do
	       tok = ti()
	    end
	    return tok, n
	 end
	 -- start parsing directives
	 ti()
	 assert(tok=='#' or tok=='##')
	 if tok == '##' then
	    xwarning(options, n, "directive starts with ## without -Zpass") end
	 dirtok = ti()
	 if isIdentifier(tok) then
	    local f = directives[dirtok]
	    if okElif and dirtok == "elif" then f = doIf end
	    xassert(f, options, n, "unrecognized preprocessor directive #%s", tok)
	    f(ti)
	 elseif tok ~= nil then
	    xerror(options, n, "unrecognized preprocessor directive '#%s'", s:gsub("^%s*",""))
	 end
      end
   end
   -- main loop
   while s ~= nil do
      processLine()
      li()
   end
end


-- This function yields initialization lines

local function initialDefines(options)
   -- cpp-extracted definitions
   if hasOption(options, "-Zcppdef") then
      local fd = io.popen("cpp -dM < /dev/null","r")
      yieldLines(options, fd:lines(), "<cppdef>")
      fd:close()
   end
   -- dialect selection
   options.dialect = 'gnu99'
   for i,v in ipairs(options) do
      if v:find("^%-std=%s*[^%s]") then
	 options.dialect = v:match("^%-std=%s*(.-)%s*$")
      end
   end
   options.dialectGnu = options.dialect:find("^gnu")
   options.dialect99 = options.dialect:find("9[9x]$")
   options.dialect11 = options.dialect:find("1[1x]$")
   options.dialectAnsi = not options.dialectGnu
   options.dialectAnsi = options.dialectAnsi and not options.dialect99
   options.dialectAnsi = options.dialectAnsi and not options.dialect11
   -- builtin definitions
   local sb = { "#define __CPARSER__ 1" }
   local function addDef(s,v)
      sb[1+#sb] = string.format("#ifndef %s",s)
      sb[1+#sb] = string.format("# define %s %s",s,v)
      sb[1+#sb] = string.format("#endif")
   end
   addDef("__STDC__", "1")
   local stdc = "199409L"
   if options.dialect11 then stdc = "201112L" end
   if options.dialect99 then stdc = "199901L" end
   addDef("__STDC_VERSION__", stdc)
   if options.dialectGnu then
      addDef("__GNUC__", 4)
      addDef("__GNUC_MINOR__", 2)
   end
   yieldLines(options, wrap(options, yieldFromArray, sb), "<builtin>")
   -- command line definitions
   local sc = {}
   for i,v in ipairs(options) do
      local d
      if v:find("^%-D(.*)=") then
	 d = v:gsub("^%-D%s*(.*)%s*=%s*(.-)%s*$","#define %1 %2")
      elseif v:find("^%-D") then
	 d = v:gsub("^%-D%s*(.-)%s*$","#define %1 1")
      elseif v:find("^%-U") then
	 d = v:gsub("^%-U%s*(.-)%s*$","#undef %1")
      end
      if d then sc[1+#sc] = d end
   end
   yieldLines(options, wrap(options, yieldFromArray, sc), "<cmdline>")
end


-- This function creates the initial macro directory

local function initialMacros(options)
   local macros = {}
   -- magic macros
   macros["__FILE__"] = function(ti,tok,n)
      local f
      if type(n) == 'string' then f=n:match("^[^:]*") end
      coroutine.yield(string.format("%q", f or "<unknown>"), n)
   end
   macros["__LINE__"] = function(ti,tok,n)
      local d = n
      if type(d) == 'string' then d=tonumber(d:match("%d*$")) end
      coroutine.yield(string.format("%d", d or 0), n)
   end
   -- initial macros
   local li = wrap(options,processDirectives,macros,initialDefines)
   for s,n in li do end
   -- return
   return macros
end


-- This function dumps the macros

local function dumpMacros(macros, outputfile)
   outputfile = outputfile or io.output()
   assert(type(macros) == 'table')
   assert(io.type(outputfile) == 'file')
   for k,v in pairs(macros) do
      if type(v) == 'table' then
	 local a = ""
	 if v.args then a = table.concat(v.args,','):gsub("(,?)__VA_ARGS__$", "%1...") end
	 if v.args then a = "(" .. a .. ")" end
	 outputfile:write(string.format("#define %s%s %s\n",k, a, table.concat(v)))
      end
   end
end


-- A coroutine that filters out spaces and directives.

local function filterSpaces(options, tokens, ...)
   local ti = wrap(options, tokens, ...)
   local tok,n = ti()
   while tok do
      -- skip directives
      while isNewline(tok) do
	 tok,n = ti()
	 if tok == '#' then
	    while not isNewline(tok) do
	       tok, n = ti()
	    end
	 end
      end
      -- output nonspaces
      if not isSpace(tok) then
	 local typ = tokenType(tok,true)
	 coroutine.yield(tok, n)
      end
      tok,n = ti()
   end
end

-- This function takes a line iterator and an optional location prefix.
-- It returns a token iterator for the preprocessed tokens
-- and a table of macro definitions.

local function cppTokenIterator(options, lines, prefix)
   options = copyOptions(options)
   prefix = prefix or ""
   assert(type(options)=='table')
   assert(type(lines)=='function')
   assert(type(prefix)=='string')
   local macros = initialMacros(options)
   local ti = wrap(options,
		   filterSpaces,
		   expandMacros, macros,
		   tokenize,
		   processDirectives, macros,
		   eliminateComments,
		   joinLines,
		   yieldLines, lines, prefix)
   return ti, macros
end


-- A coroutine that reconstructs lines from the preprocessed tokens

local function preprocessedLines(options, tokens, ...)
   local ti = wrap(options, tokens, ...)
   local tok,n = ti()
   while tok do
      local curn = n
      local curl = {}
      if isNewline(tok) then
	 curn = n
	 curl[1+#curl] = tok:sub(2)
	 tok, n = ti()
      end
      while tok and not isNewline(tok) do
	 curl[1+#curl] = tok
	 tok, n = ti()
      end
      coroutine.yield(table.concat(curl), curn)
   end
end


-- This function preprocesses file <filename>.
-- The optional argument <outputfile> specifies where to write the
-- preprocessed file and may be a string or a file descriptor.
-- The optional argument <options> contains an array of option strings.
-- Note that option "-Zpass" is added, unless the option "-Znopass" is present.

local function cpp(filename, outputfile, options)
   -- handle optional arguments
   options = copyOptions(options)
   outputfile = outputfile or "-"
   assert(type(filename)=='string')
   assert(type(options)=='table')
   local closeoutputfile = false
   if io.type(outputfile) ~= 'file' then
      assert(type(outputfile) == 'string')
      if outputfile == '-' then
	 outputfile = io.output()
      else
	 closeoutputfile = true
	 outputfile = io.open(outputfile,"w")
      end
   end
   assert(io.type(outputfile) == 'file')
   -- makes option -Zpass on by default
   if not hasOption(options,"-Znopass") then
      options.hash["-Zpass"] = true
   end
   -- prepare iterator
   local dM = hasOption(options, "-dM")
   local macros = initialMacros(options)
   local li = wrap(options,
		   preprocessedLines,
		   expandMacros, macros,
		   tokenize,
		   processDirectives, macros,
		   eliminateComments,
		   joinLines,
		   yieldLines, io.lines(filename), filename)
   -- iterate, inserting line markers
   local lm = hasOption(options,"-Zpass") and "line" or ""
   local cf, cn
   for s,n in li do
      if not dM and s:find("[^%s]") then
	 local xf, xn
	 if type(n) == 'number' then
	    xn = n
	 elseif type(n) == 'string' then
	    xf, xn = n:match("^([^:]*).-(%d*)$")
	    xn = tonumber(xn)
	 end
	 if cf ~= xf or cn ~= xn then
	    cf, cn = xf, xn
	    outputfile:write(string.format("#%s %d %q\n", lm, cn, cf))
	 end
	 outputfile:write(s)
	 outputfile:write("\n")
	 cn = cn + 1
      end
   end
   if dM then
      dumpMacros(macros, outputfile)
   end
end



---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
-- PARSING DECLARATIONS

-- Simple tuples are constructed using "tuple=Pair{a,b}" and accessed
-- as tuple[1],tuple[2] etc.  Although this is named Pair, one can use
-- more than two args.

Pair = newTag('Pair')

-- Types are represented by a series of tagged data structures.
-- Subfield <t> usually contains the base type or the function return
-- type.  Subfield <n> contains the name of the structure, union, or
-- enum.  Numerical indices are used for struct components and
-- function arguments. The construct Type{n=...} is used for named
-- types, including basic types, typedefs, and tagged struct, unions
-- or enums. When the named type has a better definition the hidden
-- field <_def> contains it. They should be constructed with function
-- namedType() because it is expected that there is only one copy of
-- each named type. The construct Qualified{t=...} is used to
-- represent const/volatile/restrict variants of the base type.
--
-- Examples
--   long int a                  Type{n="long int"}
--   int *a                      Pointer{t=Type{n="int"}}
--   const int *a                Pointer{t=Qualified{const=true,t=Type{n="int"}}}
--   int* const a                Qualified{const=true,t=Pointer{t=Type{n="int"}}}
--   void foo(int bar)           Function{Pair{Type{n="int"},"bar"},t=Type{n="void"}}}
--   int foo(void)               Function{t=Type{n="int"}}
--   int foo()                   Function{t=Type{n="int"},withoutProto=true}

Type = newTag('Type')
Qualified = newTag('Qualified')
Pointer = newTag('Pointer')
Array = newTag('Array')
Enum = newTag('Enum')
Struct = newTag('Struct')
Union = newTag('Union')
Function = newTag('Function')

-- This function creates a qualified variant of a type.

local function addQualifier(ty, q)
   assert(q=='const' or q=='volatile' or q=='restrict')
   if ty.Tag ~= 'Qualified' then ty = Qualified{t=ty} end
   ty[q] = true
   return ty
end

-- This function compares two types. When optional argument <oki> is
-- not false and types t1 or t2 are incomplete, the function returns
-- true if the types are compatible: an unsized array matches a sized
-- array, a function type without prototype matches one with a
-- prototype. Furthermore, if oki is <1>, the function will patch type
-- <t1> to contain the complete information.

local function compareTypes(t1, t2, oki)
   if t1 == t2 then
      return t1
   elseif t1.tag == 'Type' and t1._def then
      return compareTypes(t1._def, t2, oki)
   elseif t2.tag == 'Type' and t2._def then
      return compareTypes(t1, t2._def, oki)
   elseif t1.tag ~= t2.tag then
      return false
   elseif t1.tag == 'Qualified' then
      if t1.const ~= t2.const then return false end
      if t1.volatile ~= t2.volatile then return false end
      if t1.restrict ~= t2.restrict then return false end
      return compareTypes(t1.t, t2.t, oki)
   elseif t1.tag == 'Array' then
      if compareTypes(t1.t, t2.t, oki) then
	 if t1.size == t2.size then return true end
	 if t1.size == nil or t2.size == nil then
	    if oki == 1 and t1.size == nil then t1.size = t2.size end
	    return oki
	 end
      end
   elseif t1.tag == 'Function' then
      if compareTypes(t1.t, t2.t, oki) then
	 if t1.withoutProto or t2.withoutProto then
	    if t1.withoutProto and t2.withoutProto then return true end
	    if oki == 1 and t1.withoutProto then
	       for i=1,#t2 do t1[i] = t2[i] end
	       t1.withoutProto = nil
	    end
	    return oki
	 elseif #t1 == #t2 then
	    for i=1,#t1 do
	       if t1[i] == nil or t2[i] == nil then
		  return t1[i].ellipsis and t2[i].ellipsis
	       elseif not compareTypes(t1[i][1],t2[i][1],oki) then
		  return false
	       end
	    end
	    return true
	 end
      end
   elseif t1.tag == 'Enum' then
      return false
   elseif #t1 == #t2 then -- struct or union
      for i=1,#t1 do
	 if t1[i][2] ~= t2[i][2] then return false end
	 if t1[i].bitfield ~= t2[i].bitfield then return false end
	 if not compareTypes(t1[i][1],t2[i][1],oki) then return false end
      end
      if t1.n == t2.n then return true end
      if t1.n == nil or t2.n == nil then
	 if oki == 1 and t1.n == nil then t1.n = t2.n end
	 return oki
      end
   end
   return false
end

-- Constructs a string suitable for declaring a variable <nam> of type
-- <ty> in a C program. Argument <nam> defaults to "%s".

local function typeToString(ty, nam)
   nam = nam or "%s"
   assert(type(nam) == 'string')
   local function parenthesize(nam)
      return '(' .. nam .. ')'
   end
   local function insertword(word,nam)
      if nam:find("^[A-Za-z0-9$_%%]") then nam = ' ' .. nam end
      return word .. nam 
   end
   local function insertqual(ty,nam)
      if ty.restrict then nam = insertword("restrict",nam) end
      if ty.volatile then nam = insertword("volatile",nam) end      
      if ty.const then nam= insertword("const",nam) end
      return nam
   end
   local function makelist(ty,sep)
      local s = ''
      for i=1,#ty do
	 if i>1 then s = s .. sep end
	 if ty[i].ellipsis then  s = s .. '...'
	 else s = s .. typeToString(ty[i][1], ty[i][2] or "") end
	 if ty[i].bitfield then
	    s = s .. ':' .. tostring(ty[i].bitfield)
	 end
      end
      return s
   end
   -- main loop
   while true do
      if ty.tag == 'Type' then
	 return insertword(ty.n, nam)
      elseif ty.tag == 'Qualified' then
 	 if ty.t and ty.t.tag == 'Type' then nam = insertword(ty.t.n, nam) end
	 if ty.restrict then nam = insertword("restrict",nam) end
	 if ty.volatile then nam = insertword("volatile",nam) end      
	 if ty.const then nam= insertword("const",nam) end
	 if ty.t and ty.t.tag == 'Type' then ty = nil else ty = ty.t end
	 if not ty then return nam end
      elseif ty.tag == 'Pointer' then
	 local star = ty.block and '^' or '*'
	 nam = star .. insertqual(ty, nam)
	 ty = ty.t
      elseif ty.tag == 'Array' then
	 if nam:find("^[*^]") then nam = parenthesize(nam) end
	 local sz = ty.size or ''
	 nam = nam .. '[' .. tostring(sz) .. ']'
	 ty = ty.t
      elseif ty.tag == 'Function' then
	 if nam:find("^[*^]") then nam = parenthesize(nam) end
	 if #ty == 0 and ty.withoutProto then nam = nam .. '()'
	 elseif #ty == 0 then nam = nam .. '(void)'
	 else nam = nam .. '(' .. makelist(ty,',') .. ')' end
	 if ty.const then nam = nam .. 'const' end
	 ty = ty.t
      elseif ty.tag == 'Enum' then
	 local s = 'enum'
	 if ty.n then s = s .. ' ' .. ty.n end
	 s = s .. '{'
	 for i=1,#ty do
	    if i > 1 then s = s .. ',' end
	    s = s .. ty[i][1]
            if ty[i][2] then 
               s = s .. '=' .. tostring(ty[i][2]) 
            end
	 end
	 return s .. '}' .. nam
      else
	 local s = string.lower(ty.tag)
	 if ty.n then s = s .. ' ' .. ty.n end
	 return s .. '{' .. makelist(ty,';') .. '}' .. nam
      end
   end
end


-- Tables Definition{} and Declaration{} represent variable and
-- constant definitions and declarations found in the code. Field
-- <where> is the location of the definition or declaration, field
-- <name> is the name of the variable or constant being defined, field
-- <type> contains the type, field <init> optionally contain the
-- initialization or the function body. Field <sclass> contains the
-- storage class such as <extern>, <static>, <auto>. Special storage
-- class '[enum]' is used to define enumeration constants. Special
-- storage class '[cpp]' is used to communicate certain preprocessor
-- constants. Table TypeDef{} represents type definitions and contains
-- pretty much the same fields. Note that storage class <typedef> is
-- used for an actual <typedef> and storage class <[typetag]> is used
-- when the type definition results from a tagged structure union or
-- enum.

TypeDef = newTag('TypeDef')
Definition = newTag('Definition')
Declaration = newTag('Declaration')

local function declToString(action)
   local n = (action.sclass == '[typetag]') and "" or action.name
   local s = typeToString(action.type, n)
   if action.type.inline then
      s = 'inline' .. ' ' .. s
   end
   if action.sclass then
      s = action.sclass .. ' ' .. s
   end
   if action.intval then
      s = s .. ' = ' .. action.intval
   elseif action.init then
      local r = (action.type.tag == 'Function') and ' {..}' or '= (...)'
      s = s .. r
   end
   return s
end


-- The symbol table is implemented by a table that contains Type{}
-- nodes for type definitions (possibly with a hidden <_def> field
-- pointing to the full definition), Definition{} or Declaration{]
-- nodes for all other names.

local function isTypeName(symtable, name)
   local ty = symtable[name]
   if ty and ty.tag == 'Type' then return ty end
   return false
end

local function isDefined(symtable, name)
   return symtable[name] and symtable[name].tag ~= 'Declaration'
end

local function newScope(symtable)
   local newSymtable = {}
   setmetatable(newSymtable, {__index=symtable})
   return newSymtable
end


-- Returns an iterator that can look tokens ahead.
-- Calling it without arguments works like an ordinary iterator.
-- Calling it with argument 0 returns the current token.
-- Calling it with a positive argument look <arg> positions ahead.
-- Calling it with argument -1 pushes back the last token.

local function lookaheadTokenIterator(ti)
   local tok,n = ti()
   local fifo = {}
   return function(arg)
      if not arg then
	 if fifo[1] then
	    tok,n = unpack(fifo[1])
	    table.remove(fifo,1)
	    return tok,n
	 else
	    tok,n = ti()
	    return tok,n
	 end
      elseif arg == 0 then
	 return  tok, n
      elseif arg == -1 then
	 table.insert(fifo,1,{tok,n})
	 return  tok, n
      else
	 assert(type(arg)=='number' and arg > 0)
	 while arg > #fifo do fifo[1+#fifo] = {ti()} end
	 return unpack(fifo[arg])
      end
   end
end

-- Concatenate two possibly null arrays

local function appendSequences(a1, a2)
   if not a1 then
      return a2
   elseif not a2 then
      return a1
   else
      local a = {}
      for i,v in ipairs(a1) do a[1+#a] = v end
      for i,v in ipairs(a2) do a[1+#a] = v end
      return a
   end
end


-- Evaluation of constant expression.
--   We avoid writing a complete expression parser by reusing the cpp
-- expression parser and either returning an integer (when we can
-- evaluate) or a string containing the expression (when we can't) or
-- nil (when we are sure this is not a number).  Array <arr> contains
-- tokens (odd indices) followed by location (even indices). Argument
-- <tstable> is the typespecifier table (optional).
--   The alternative is to write a proper expression parse with
-- constant folding as well as providing means to evaluate the
-- value of the sizeof and alignof operators. This is tricky
-- but might be needed if one wants to compute struct layouts.

local function tryEvaluateConstantExpression(options, n, arr, symtable)
   -- array initializers never are constant integers
   if arr[1] == '{' then return nil,false end
   -- try direct evaluation
   local i = -1
   local function ti(arg)
      if not arg then i = i + 2 ; arg = 0 end
      if arg < 0 and i > -1 then i = i - 2 ; arg = 1 end
      return arr[i+2*arg],arr[i+2*arg+1]
   end
   local function rsym(tok)
      local s = symtable and symtable[tok]
      xassert(s and type(s.intval)=='number', {silent=true}, n,
	      "symbol '%s' does not resolve to a constant integer")
      return s.intval
   end
   local s,r = pcall(evaluateCppExpression, {silent=true}, ti, n, rsym)
   if s and type(r)=='number' and not ti() then return r,true end
   if s and r and type(r)~='number' then return nil,false end
   -- just return an expression string
   local function spacebetween(t1,t2)
      if not t1 or not t2 then return false end
      local it1 = isIdentifier(t1) or isNumber(t1)
      local it2 = isIdentifier(t2) or isNumber(t2)
      if it1 and it2 then return true end
      if it1 and not it2 or not it1 and it2 then return false end
      local z = callAndCollect(options,tokenizeLine,t1..t2,n,true)
      return z[1]~=t1 or z[2]~=t2
   end
   local s = {}
   for i=1,#arr,2 do
      if spacebetween(arr[i-2],arr[i]) then
	 s[1+#s] = ' '
      end
      if isName(arr[i]) and symtable[arr[i]] and symtable[arr[i]].eval then
	 s[1+#s] = string.format("(%s)", symtable[arr[i]].eval)
      else
	 s[1+#s] = arr[i]
      end
   end
   s = table.concat(s)
   xwarning(options, n, "cparser cannot evaluate '%s' as an integer constant"
	       .. " and is using the literal expression instead", s)
   return s, false
end


-- This coroutine is the declaration parser
-- Argument <globals> is the global symbol table.
-- Argument <tokens> is a coroutine that yields program tokens.

local function parseDeclarations(options, globals, tokens, ...)
   -- see processMacroCaptures around the end of this function
   if type(options.macros) == 'table' then options.macros[1] = {} end
   
   -- define a lookahead token iterator that also ensures that
   -- variables tok,n always contain the current token
   local ti = lookaheadTokenIterator(wrap(options, tokens, ...))
   local tok,n = ti(0)
   local ti = function(arg)
      if arg then return ti(arg) end
      tok,n = ti()
      -- print(string.format("*** [%s] (%s)",tok,n))
      return tok,n
   end

   -- this function is used to retrieve or construct Type{} nodes for
   -- named types. Since the Type constructor should not be used we
   -- override it with a function that calls assert(false)
   local function namedType(symtable, nam)
      local ty = symtable[nam]
      if ty and ty.tag == 'Type' then
	 return ty
      elseif ty and ty.tag ~= 'Type' then
	 local msg = " previous declaration at %s"
	 if rawget(symtable,nam) then
	    xerror(options, n, "type name '%s' conflicts with" .. msg, nam, ty.where)
	 else
	    xwarning(options, n, "type name '%s' shadows" .. msg, nam, ty.where)
	 end
      end
      ty = Type{n=nam}
      symtable[nam] = ty
      return ty
   end
   local function Type() assert(false) end
   
   -- check that current token is one of the provided token strings
   local function check(s1,s2)
      if tok == s1 then return end
      if tok == s2 then return end
      if not s2 then
	 xerror(options,n,"expecting '%s' but got '%s'", s1, tok)
      else
	 xerror(options,n,"expecting '%s' or '%s' but got '%s'", s1, s2, tok)
      end
   end
   
   -- record tokens into array arr if non nil
   local function record(arr)
      if arr then arr[1+#arr]=tok arr[1+#arr]=n end
   end
   
   -- skip parenthesized expression stating on current token.
   -- return nil if current token is not a left delimiter.
   -- new current token immediately follow right delimiter.
   -- optionally record tokens into arr and return arr 
   local function skipPar(arr)
      local dleft =  { ["("]=")", ["{"]="}", ["["]="]" }
      local dright = { [")"]=1,   ["}"]=1,   ["]"]=1 }
      local stok = dleft[tok]
      if stok then
	 local sn = n
	 local ltok = tok
	 record(arr) ti()
	 while not dright[tok] do
	    xassert(tok, options, sn, "no matching '%s' for this '%s'", stok, ltok)
	    if dleft[tok] then skipPar(arr) else record(arr) ti() end
	 end
	 xassert(tok==stok, options, n, "expecting '%s' but got '%s'", tok, stok)
	 record(arr) ti()
	 return arr
      end
   end
   
   -- skip balanced tokens until reaching token s1 or s2 or s2.
   -- in addition s1 may be a table whose keys are the stop token
   -- new current token immediately follow the stop token
   -- optionally record tokens into arr and return arr 
   local function skipTo(arr,s1,s2,s3,s4)
      local sn = n
      while tok and tok ~= s1 and tok ~= s2 and tok ~= s3 and tok ~= s4 do
	 if type(s1) == 'table' and s1[tok] then break end
	 if not skipPar(arr) then record(arr) ti() end end
      xassert(tok,options,sn,"unterminated expression")
      return arr
   end

   
   -- processDeclaration.
   -- Argument <where> is the file/line of the declaration.
   -- Argument <symtable> is the current symbol table.
   -- Argument <context> is 'global', 'param', 'local'
   local function processDeclaration(where, symtable, context, name, ty, sclass, init)
      local dcl
      -- handle type definitions
      if sclass == 'typedef' or sclass == '[typetag]' then
	 local nty = namedType(symtable, name)
	 nty._def = ty
	 dcl = TypeDef{name=name,type=ty,where=where,sclass=sclass}
	 symtable[name] = nty
	 if context == 'global' then coroutine.yield(dcl) end
	 return
      end
      -- handle variable and constants
      if ty.tag == 'Function' then
	 if init then
	    dcl = Definition{name=name,type=ty,sclass=sclass,where=where,init=init}
	 else
	    dcl = Declaration{name=name,type=ty,sclass=sclass,where=where}
	 end
      else
	 if sclass == 'extern'
	    or ty.const and not init and sclass ~= '[enum]'
	    or ty.tag=='Array' and not ty.size and not init
	 then
	    xassert(not init,options,n,"extern declaration cannot have initializers")
	    dcl = Declaration{name=name,type=ty,sclass=sclass,where=where}
	 else
	    local v = ty.const and init
	    if type(v) == 'table' then
	       v = tryEvaluateConstantExpression(options,where,init,symtable)
	    end
	    dcl = Definition{name=name,type=ty,sclass=sclass,where=where,init=init,intval=v}
	 end
      end
      -- check for duplicate declaration
      local ddcl = dcl
      if dcl.tag ~= 'TypeDef' then
	 local odcl = symtable[name]
	 local samescope = rawget(symtable, name)
	 -- compare types
	 if odcl and samescope then
	    if dcl.tag == 'Definition' and odcl.tag == 'Definition'
	    or not compareTypes(dcl.type,odcl.type,true) then
	       print("***0",dcl)
	       print("***1",odcl)
	       xerror(options,where,
		      "%s of symbol '%s' conflicts with earlier %s at %s",
		      string.lower(dcl.tag), name,
		      string.lower(odcl.tag), odcl.where)
	    end
	    if odcl.tag == 'Definition' then
	       ddcl = odcl
	       compareTypes(ddcl.type, dcl.type, 1)
	    else
	       compareTypes(ddcl.type, odcl.type, 1)
	    end
	 end
	 -- compare storage class
	 if odcl and dcl.sclass ~= odcl.sclass then
	    if dcl.sclass == 'static' or samescope and odcl.sclass == 'static' then
	       xerror(options, n, "inconsistent linkage for '%s' (previous at %s)",
		      name, odcl.where)
	    end
	 end
	 -- install dcl in symtable and yield global declarations
	 symtable[name] = ddcl
	 if context == 'global' then coroutine.yield(dcl) end
      end
   end
   
   -- forward declations of parsing functions
   local parseDeclaration
   local parseDeclarationSpecifiers
   local parseDeclarator, parsePrototype
   local parseEnum, parseStruct

   -- C declarations have a left part that contains a type
   -- and comma separated right parts that contain the variable
   -- name in expressions that mimic how one would use the
   -- variable to obtain the type specified by the left part.
   -- The left part is called a DeclarationSpecifier
   -- and the right parts are called Declarators.
   
   -- token classification table for speeding up type parsing
   local specifierTable = {
      typedef    = 'sclass',
      extern     = 'sclass',
      static     = 'sclass',   
      auto       = 'sclass',
      register   = 'sclass',
      void       = 'type',
      char       = 'type',
      float      = 'type',
      int        = 'type',
      double     = 'type',
      short      = 'size',
      long       = 'size',
      signed     = 'sign',
      unsigned   = 'sign',
      const      = 'const',
      volatile   = 'volatile',
      __inline__    = 'inline',
      __asm         = 'attr',
      __asm__       = 'attr',
      __declspec    = 'attr',
      __restrict__  = 'restrict',
      __attribute__ = 'attr',
      __extension__ = 'extension',
      _Bool         = not options.dialectAnsi and 'type',
      __restrict    = not options.dialectAnsi and 'restrict',
      restrict      = not options.dialectAnsi and 'restrict',
      _Complex      = not options.dialectAnsi and 'complex',
      _Imaginary    = not options.dialectAnsi and 'complex',
      _Atomic       = not options.dialectAnsi and 'atomic',
      __inline      = not options.dialectAnsi and 'inline',
      inline        = options.dialectGnu or options.dialect11 and 'inline',
      asm           = options.dialectGnu and "attr",
      _Pragma       = not options.dialectAnsi and "attr",
      _Alignas      = options.dialect11 and "attr",
   }
   
   -- appends attributes to table
   local function isAttribute()
      return specifierTable[tok]=='attr' or 
         options.dialect11 and tok=='[' and ti(1)=='['
   end
   local function collectAttributes(arr)
      while isAttribute() do
         arr = arr or {}
         if tok~='[' then arr[1+#arr]=tok; ti() end
         if tok=='(' or tok =='[' then skipPar(arr) end
      end
      return arr
   end
   
   -- This function parses the left part and returns the type, and a table
   -- containing all the additional information we could collect, namely the
   -- presence of an inline keyword or the tokens associated with
   -- compiler-specific attribute syntax.
   parseDeclarationSpecifiers = function(symtable, context, abstract)
      local ty
      local nn = {}
      while true do
	 local ltok = tok
	 local p = specifierTable[tok]
         if options.dialect11 and tok=='[' and ti(1)=='[' then p='attr' end
         if p == 'attr' then
            nn.attr = collectAttributes(nn.attr)
         elseif p then
            ti()
         elseif tok == 'enum' then
	    p = 'type'; ty = parseEnum(symtable, context, abstract, nn)
	 elseif tok == 'struct' or tok == 'union' then
	    p = 'type'; ty = parseStruct(symtable, context, abstract, nn)
         elseif isName(tok) then
	    local tt = isTypeName(symtable, tok)
            if tt then
               p = 'type'; ty = tt; ti()
            else
               local tok1 = ti(1)
               local yes = not nn.type and not nn.size and not nn.sign and not nn.complex
               local no = not abstract and tok1:find("^[;,[]")
               if yes and not no then -- assuming this is a type name
		  p = 'type'; ty = namedType(globals, tok); ti()
               end
            end
	 end
	 if not p then
	    break
	 end
	 if p == 'size' and ltok == 'long' and nn[p] == 'long' then
	    nn[p] = 'long long'
	 elseif p=='attr' then
	    -- already done
         elseif p=='type' and nn[p] then
	    xerror(options,n,"conflicting types '%s' and '%s'", nn[p], ltok)
	 elseif nn[p] then
	    xerror(options,n,"conflicting type specifiers '%s' and '%s'", nn[p], ltok)
	 else
	    nn[p] = ltok
	 end
      end
      -- resolve multi-keyword type names
      if not nn.type then
	 if nn.size or nn.sign then
	    nn.type = 'int'
	 elseif nn.complex then
	    xwarning(options, n, "_Complex used without a type, assuming 'double'")
	    nn.type = 'double'
	 elseif nn.sclass then
	    xwarning(options, n, "missing type specifier defaults to 'int'")
	    nn.type = 'int'
	 else
	    xerror(options, n, "missing type specifier")
	 end
      end
      if nn.type == 'char' then
	 nn.integral = true
	 if nn.sign then
	    nn.type=nn.sign..' '..nn.type nn.sign=nil end
      elseif nn.type == 'int' then
	 nn.integral = true
	 if nn.size then
	    nn.type=nn.size..' '..nn.type nn.size=nil end
	 if nn.sign then
	    nn.type=nn.sign..' '..nn.type nn.sign=nil end
      elseif nn.type == 'double' then
	 if nn.size and nn.size:find('long') then
	    nn.type=nn.size..' '..nn.type nn.size=nil end
	 if nn.complex then
	    nn.type=nn.complex..' '..nn.type nn.complex=nil end
      elseif nn.type == 'float' then
	 if nn.complex then
	    nn.type='_Complex '..nn.type nn.complex=nil end
      end
      if nn.atomic then
         nn.type='_Atomic '..nn.type nn.atomic = nil -- could be narrower
      end
      local msg = "qualifier '%s' cannot be applied to type '%s'"
      xassert(not nn.sign, options, n, msg, nn.sign, nn.type)
      xassert(not nn.size, options, n, msg, nn.size, nn.type)
      xassert(not nn.complex, options, n, msg, nn.complex, nn.type)
      xassert(not nn.atomic, options, n, msg, nn.complex, nn.type)
      -- signal meaningless register storage classes
      local sclass = nn.sclass
      local msg = "storage class '%s' is not appropriate in this context"
      if context == 'global' then
	 xassert(sclass~='register' and sclass~='auto', 
                 options, n, msg, sclass)
      elseif context == 'param' then
	 xassert(sclass~='static' and sclass~='extern' and sclass~='typedef', 
                 options, n, msg, sclass)
      end
      -- return
      if not ty then ty = namedType(globals, nn.type) end
      if nn.const then ty = addQualifier(ty, 'const') end
      if nn.volatile then ty = addQualifier(ty, 'volatile') end
      xassert(not nn.restrict, options, n,
	      "qualifier '%s' is not adequate here", nn.restrict)
      return ty, nn
   end

   -- This function parse the right parts and returns the identifier
   -- name, its type, and a storage class. Its arguments are the
   -- outputs of the corresponding <parseDeclarationSpecifier> plus
   -- the same arguments as <parseDeclarationSpecifier>.
   parseDeclarator = function(ty, extra, symtable, context, abstract)
      -- because of the curious syntax of c types, it turns out that
      -- it is easier to construct the chain of types in reverse
      local name
      local parseRev
      parseRev = function()
	 local ty
	 if isName(tok) then
	    xassert(not name, options, n, "extraneous identifier '%s'", tok)
	    name = tok
	    ti()
	 elseif tok == '*' or tok == '^' then --pointer
	    local block
	    if tok == '^' then
               block = true -- code blocks (apple)
	    end
	    ti()
	    local nt, pt
	    while tok=='const' or tok=='volatile'
	    or specifierTable[tok]=='restrict' do
	       nt = nt or Qualified{}
	       nt[specifierTable[tok]] = true
	       ti()
	    end
	    pt = parseRev()
	    if nt then nt.t = pt; pt = nt; end
	    ty = Pointer{t=pt, block=block}
	 elseif tok == '(' then
	    ti()
	    local p = specifierTable[tok] or isTypeName(tok) or tok == ')'
	    if abstract and p then
	       ty = parsePrototype(ty,symtable,context,abstract)
	    else
	       ty = parseRev()
	       check(')') ti()
	    end
	 elseif tok ~= '[' then
	    return ty
	 end
	 while tok == '(' or tok == '[' and ti(1) ~= '[' do
	    if tok == '(' then ti()
	       ty = parsePrototype(ty,symtable,context,abstract)
	       check(")") ti()
               while isAttribute() or tok == 'const' do
                  if tok == 'const' or tok == 'volatile' then 
                     ty[tok] = true; ti() 
                  else 
                     ty.attr = collectAttributes(ty.attr) 
                  end
               end
	    elseif tok == '[' then -- array
	       xassert(ty==nil or ty.tag ~= 'Function', options,n,
		       "functions cannot return arrays (they can return pointers)")
	       if ti() == ']' then
		  xassert(ty==nil or ty.tag~='Array', options, n,
			  "only the outer array can be specified without a size")
		  ty = Array{t=ty}
		  ti()
	       else
		  local size = skipTo({},']',',',';')
		  local v = tryEvaluateConstantExpression(options, n, size, symtable)
		  xassert(v, options, n,
			  "syntax error in array size specification")
		  xassert(type(v)~='number' or v>=0, options, n,
			  "invalid array size '%s'", v)
		  check(']') ti()
		  ty = Array{t=ty, size=v}
	       end
	    end
	 end
	 return ty
      end
      -- get reversed type and reverse it back
      local where = n
      local rty = parseRev()
      while rty do
	 local nty = rty.t
	 rty.t = ty
	 ty = rty
	 rty = nty
         -- syntax checks
         if ty.tag == 'Pointer' and ty.block then
            xassert(ty.t and ty.t.tag == 'Function', options, where,
                    "invalid use of code block operator '^'")
         end
      end
      -- finalize
      if extra.inline then
	 xassert(ty.tag == 'Function', options, where, "only functions can be declared inline")
	 ty.inline = true
      end
      ty.attr = appendSequences(extra.attr, collectAttributes(ty.attr))
      -- return
      xassert(abstract or name, options, n, "an identifier was expected")
      return name, ty, extra.sclass
   end
   
   -- We are now ready to parse a declaration in the specified context
   parseDeclaration = function(symtable, context)
      -- parse declaration specifiers
      local where = n
      local lty, lextra = parseDeclarationSpecifiers(symtable,context,false)
      -- loop over declarators
      if isName(tok) or tok=='*' or tok=='&' or tok == '^' or tok=='(' or tok=='[' then
	 -- parse declarator
	 local name,ty,sclass = parseDeclarator(lty, lextra, symtable, context, false)
	 -- first declarator may be a function definition
	 if context == 'global' and name and ty.tag=='Function' and tok == '{' then
	    local body = skipPar({})
	    xassert(sclass ~= 'typedef', options, where,
		    "storage class %s is not adequate for a function definition", sclass)
	    processDeclaration(where, symtable, context, name, ty, sclass, body)
	    return
	 end
	 -- process declarators
	 while true do
	    if ty.tag == 'Function' then
	       if not where then error() end
	       processDeclaration(where, symtable, context, name, ty, sclass)
	    else
	       local init
	       if tok == '=' then
		  xassert(sclass ~= 'typedef', options, n, "a typedef cannot have an initializer")
		  ti()
		  init = skipTo({}, specifierTable, ';', ',')
	       end
	       processDeclaration(where, symtable, context, name, ty, sclass, init)
	    end
	    if tok ~= ',' then break else ti() end
	    where = n 
	    name,ty,sclass = parseDeclarator(lty, lextra, symtable, context, false)
	 end
      else
	 xassert(lextra.newtype, options, where, "empty declaration")
      end
      -- the end
      check(';') ti()
   end
   
   parsePrototype = function(rty,symtable,context,abstract)
      local nsymtable = newScope(symtable)
      local ty = Function{t=rty}
      local i=0
      while tok ~= ')' do
	 if tok == '...' then
	    i = i + 1
	    ty[i] = Pair{ellipsis=true}
	    ti() check(')')
	 else
	    local lty, lextra = parseDeclarationSpecifiers(nsymtable, 'param', true)
	    local pname, pty, psclass = parseDeclarator(lty, lextra, nsymtable, 'param', true)
	    if pty.tag == 'Type' and pty.n == 'void' then
	       xassert(i==0 and not pname and tok==')',options,n,
		       "void in function parameters must appear first and alone")
	       return ty
	    else
	       if pty.tag == 'Array' then pty = Pointer{t=pty.t} end
	       i = i + 1
	       local def
	       if tok == '=' then ti() def=skipTo({}, specifierTable,';',',') end
	       ty[i] = Pair{pty,pname,defval=def}
	       if tok == ',' then ti() else check(',',')') end
	    end
	 end
      end
      if i == 0 then ty.withoutProto = true end
      return ty
   end
   
   parseStruct = function(symtable, context, abstract, nn)
      check('struct', 'union')
      local kind = tok ; ti()
      nn.attr = collectAttributes(nn.attr)
      local ttag, tnam
      if isName(tok) then ttag=tok; tnam=kind..' '..ttag; nn.newtype=true; ti() end
      if ttag and tok ~= '{' then return namedType(symtable, tnam) end
      -- parse real struct definition
      local ty
      if kind == 'struct' then ty = Struct{n=ttag} else ty = Union{n=ttag} end
      local where = n
      check('{') ti()
      while tok and tok ~= '}' do
	 local where = n
	 local lty, lextra = parseDeclarationSpecifiers(symtable, context)
	 xassert(lextra.sclass == nil, options, where, 
                 "storage class '%s' is not allowed here", lextra.sclass)
	 if tok == ';' then -- anonymous member
	    xassert(lty.tag=='Struct' or lty.tag=='Union' , options, where, "empty declaration")
	    ty[1+#ty] = Pair{lty}
	 else
	    while true do
	       if tok == ':' then ti() -- unnamed bitfield
		  xassert(lextra.integral, options, where, "bitfields must be of integral types")
		  local size = skipTo({},',',';')
		  local v = tryEvaluateConstantExpression(options, where, size, symtable)
		  xassert(v, options, where,
			  "syntax error in bitfield specification")
		  xassert(type(v)~='number' or v>=0, options, where,
			  "invalid anonymous bitfield size (%s)", v)
		  ty[1+#ty] = Pair{lty,bitfield=v}
	       else
		  local pname, pty, psclass = parseDeclarator(lty, lextra, symtable, context)
		  if pty.tag == 'Array' and not pty.size then
		     xwarning(options, where, "unsized arrays are not allowed here (ignoring)")
		  elseif pty.tag == 'Function' then
		     xerror(options, where, "member functions are not allowed in C")
		  end
		  if tok == ':' then ti()
		     xassert(lty == pty and lextra.integral, options, where, 
                             "bitfields must be of integral types")
		     local size = skipTo({},',',';')
		     local v = tryEvaluateConstantExpression(options,where,size,symtable)
		     xassert(v, options, where,
			     "syntax error in bitfield specification")
		     xassert(type(v)~='number' or v>0, options, where,
			     "invalid bitfield size (%s)", v)
		     ty[1+#ty] = Pair{pty,pname,bitfield=v}
		  else
		     ty[1+#ty] = Pair{pty,pname}
		  end
	       end
	       check(',',';')
	       if tok == ',' then ti() else break end
	    end
	 end
	 check(';','}')
	 if tok == ';' then ti() end
      end
      check("}") ti()
      -- change tagged type as newtype
      if ttag then
	 nn.newtype = true
	 processDeclaration(where, symtable, context, tnam, ty, '[typetag]')
	 return namedType(symtable, tnam)
      else 
	 return ty
      end
   end
   
   parseEnum = function(symtable, context, abstract, nn)
      local kind = tok ; ti()
      nn.attr = collectAttributes(nn.attr)
      local ttag, tnam
      if isName(tok) then ttag=tok; tnam=kind..' '..ttag; nn.newtype=true; ti() end
      if ttag and tok ~= '{' then return namedType(symtable, tnam) end
      -- parse real struct definition
      local i = 1
      local v,a = 1,0
      local ty = Enum{n=ttag}
      local ity = namedType(globals, "int")
      local where = n
      check('{') ti()
      repeat
	 local nam = tok
	 local init
	 xassert(isName(nam),options,n,"identifier expected, got '%s'", tok)
         collectAttributes(nil) -- parsed but lost for now
	 if ti() == '=' then ti()
            init = skipTo({},',','}')
	    v = tryEvaluateConstantExpression(options, n, init, symtable)
	    xassert(v,options,n,"invalid value for enum constant")
	    a = 0
	 end
	 local x
	 if type(v) == 'number' then
	    x = v + a
	 elseif a > 0 then
	    x = string.format("%d+(%s)",a,v)
	 else
	    x = v
	 end
	 ty[i] = Pair{nam, init and v}
	 a = a + 1
	 i = i + 1
	 processDeclaration(n, symtable, context, nam, ity, '[enum]', x)
	 if tok == ',' then ti() else check(',','}') end
      until tok == nil or tok == '}' 
      check('}') ti()
      -- change tagged type as newtype
      nn.newtype = true
      if ttag then
	 processDeclaration(where, symtable, context, tnam, ty, '[typetag]')
	 return namedType(symtable, tnam)
      else 
	 return ty
      end
   end

   -- When macros[1] is a table, the preprocessor attempts to
   -- preprocess and evaluate the definition of object-like macros. If
   -- the evaluation is successful, it adds it to the table.
   local function processMacroCaptures()
      local macros = options.macros
      local ity = Qualified{const=true}
      if type(macros) == 'table' and type(macros[1]) == 'table' then
	 for k,v in pairs(macros[1]) do
	    local a = Definition{where=v.where, name=k, type=ity,
				 intval=v.intval, sclass='[cpp]'}
	    coroutine.yield(a)
	 end
	 macros[1] = {}
      end
   end
   
   -- main loop
   local symtable = newScope(globals)
   while tok do
      while tok == ';' do ti() end
      processMacroCaptures()
      parseDeclaration(symtable,"global")
      processMacroCaptures()
   end
   return globals
end



-- processes the typedef options <-Ttypename>
-- and create the initial symbol table.

local function initialSymbols(options)
   local symbols = {}
   for i,v in ipairs(options) do
      if v:find("^%-T") then
	 local d = v:gsub("^%-T%s*(.-)%s*$")
	 xassert(d and d:find("[A-Za-z_$][A-Za-z0-9_$]*"),
		 options,"<commandline>",
		 "option -T must be followed by a valid identifier")
	 symbols[d] = TypeDef{n=d}
      end
   end
   return symbols
end


-- this function return an iterator function that
-- successively returns actions as tagged tables
-- with tags TypeDef, VarDef, FuncDef, or Declaration.

local function declarationIterator(options, lines, prefix)
   options = copyOptions(options)
   prefix = prefix or ""
   local symbols = initialSymbols(options)
   local macros = initialMacros(options)
   assert(type(options)=='table')
   assert(type(lines)=='function')
   assert(type(prefix)=='string')
   assert(type(symbols)=='table')
   assert(type(macros)=='table')
   options.macros = macros
   options.symbols = symbols
   local di = wrap(options,
		   parseDeclarations, symbols, 
		   filterSpaces,
		   expandMacros, macros,
		   tokenize,
		   processDirectives, macros,
		   eliminateComments,
		   joinLines,
		   yieldLines, lines, prefix)
   return di, symbols, macros
end

   







---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
-- EXPORTS

cparser = {}
cparser.cpp = cpp
cparser.cppTokenIterator = cppTokenIterator
cparser.typeToString = typeToString
cparser.declarationIterator = declarationIterator

if DEBUG then
   function tstInitial(filename, options)
      options = copyOptions(options)
      local li = wrap(options,eliminateComments,joinLines,yieldLines,io.lines(filename))
      for s,n in li do print(string.format("(%s) %s",n,s)) end
   end
   function tstTokenizeLine(s,n,options)
      options = copyOptions(options)
      local ti = wrap(options,tokenizeLine,s,n)
      for tok,n in ti do print(string.format("(%s) %q\t%s",n,tok,tokenType(tok))) end
   end
   function tstTokenize(filename, options)
      options = copyOptions(options)
      local ti = wrap(options,tokenize,eliminateComments,joinLines,yieldLines,io.lines(filename))
      for tok,n in ti do print(string.format("(%s) %q\t%s",n,tok,tokenType(tok))) end
   end
   function tstPreprocess(filename,options)
      local li = cppTokenIterator(options, io.lines(filename), filename)
      for tok,n in li do print(string.format("(%s) %10s\t %s",n,tokenType(tok,true),tok)) end
   end
   function tstParse(filename,options)
      local li = declarationIterator(options, io.lines(filename), filename)
      for action in li do
	 print("+--",action)
	 print("|\t",declToString(action))
      end
   end
 end

return cparser
  

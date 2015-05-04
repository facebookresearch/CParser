# CParser

(c) 2015 Léon Bottou -- Facebook AI Research

This pure Lua module implements (1) a standard compliant C
preprocessor with a couple useful extensions, and (2) a parser that
provides a Lua friendly description of all declarations and
definitions in a C header or C program file.

The driver program `lcpp` invokes the preprocessor and outputs
preprocessed code. Although it can be used as a replacement for the
normal preprocessor, it is more useful as an extra preprocessing step
(see option `-Zpass` which is on by default.)  The same capabilities
are offered by functions `cparser.cpp` and `cparser.cppTokenIterator`
provided by the module `cparser`.

The driver program `lcdecl` analyzes a C header file and a C program
file and outputs a short descriptions of the declarations and
definitions. This program is mostly useful to understand the
representations produced by the `cparser` function
`cparser.declarationIterator`.

## Program `lcpp`

### Synopsis

```sh
    lcpp [options] inputfile.c [-o outputfile.c]
```
Preprocesses file `inputfile.c` and writes the preprocessed code into
file `outputfile.c` or to the standard output.

### Options

The following options are recognized:

- `-Werror`  
  Cause all warning to be treated as errors.
  Note that parsing cannot resume after an error.
  The parser simply throws a Lua error.

- `-w`   
  Do not print warning messages.

- `-D`*sym*`[=`*val*`]`  
   Define preprocessor symbol `sym` to value `val`.
   The default value of `val` is `1`

- `-U`*sym*   
  Undefine preprocessor symbol `sym`.

- `-I`*dir*   
  Add directory `dir` to the search path for included files. Note
  that there is no default search path. When an include file is not
  found the include directive is simply ignored with a warning (but
  see also option `-Zpass`).  Therefore all include directives are
  ignored unless one uses option `-I` to specify the search path.

- `-I-`   
  Marks the beginning of the system include path. When an included
  file is given with angle brackets, (as in `#include <stdio.h>`),
  one only searches directories specified by the `-I` options that
  follow `-I-`. Therefore all these include directives are ignored
  unless one uses option `-I-` followed by one or more option `-I`.

- `-dM`   
  Instead of producing the preprocessed file,
  dumps all macros defined at the end of the parse.

- `-Zcppdef`   
  Run the native preprocessor using command `cpp -dM < dev/null`
  and copy its predefined symbols. This is useful when using
  `lcpp` as a full replacement for the standard preprocessor.

- `-Zpass`   
  This option is enabled by default (use `-Znopass` to disable)
  and indicates that the output of `lcpp` is going to be
  reprocessed by a C preprocessor and compiler.
  When this feature is enabled,
  * the `#pragma` and `#ident` directives are copied
    verbatim to the output,
  * the `#include` directives are copied to the output when
    the included file cannot be found in the provided search path,
  * preprocessor directives prefixed with a double `##` are
    copied verbatim to the output with a single `#`.  This is
    useful for `#if` directives that depend on symbols
    defined by unresolved `#include` directives.

- `-std=(c|gnu)(89|99|11)`  
  This option selects a C dialect.
  In the context of the preprocessor, this only impacts
  the symbols predefined by `lcpp`.
  * Symbol `__CPARSER__` is always defined with value <1>.
  * Symbols `__STDC__` and `__STDC_VERSION__` are either defined by
    option `-Zcppdef` or take values suitable for the target C
    dialect.
  * Symbols `__GNUC__` and `__GNUC_MINOR__` are either defined bu
    option `-Zcppdef` or are defined to values `4` and `2` if the
    target dialect starts with string `gnu`.
  This can be further adjusted using the `-D` or `-U` options.
  

### Preprocessor extensions

The `lcpp` preprocessor implements several useful nonstandard features.
The main feature are multiline macros. The other features are mostly
here because they make multiline macros more useful.

####  String comparison in conditional expressions

The C standard specifies that the expressions following `#if`
directives are constant expressions of integral type. However this
processor also handles strings. The only valid operations on strings
are the equality and ordering comparisons. This is quite useful to
make special cases for certain values of the parameters of a multiline
macro, as shown later.

####  Multiline macros

Preprocessor directives `#defmacro` and `#endmacro` can be used to
define a function-like macro whose body spans several lines. The
`#defmacro` directive contains the macro name and a mandatory argument
list. The body of the macro is composed of all the following lines up
to the matching `#endmacro`. This offers several benefits:

* The line numbers of the macro-expansion is preserved. This ensures
  that the compiler produces error messages with meaningful line
  numbers.

* The multi-line macro can contain preprocessor directives.
  Conditional directives are very useful in this context.  Note
  however that preprocessor definitions (with `#define`, `#defmacro`,
  or `#undef`) nested inside multiline macros are only valid within
  the macro.

* The standard stringification `#` and token concatenation `##`
  operators can be used freely in the body of multiline macros.  Note
  that these operators only work with the parameters of the multiline
  macros and not with ordinary preprocessor definitions. This is
  consistent with the standard behavior of these operators in ordinary
  preprocessor macros.

  Example

```C
      #defmacro DEFINE_VDOT(TNAME, TYPE)
        TYPE TNAME##Vector_dot(TYPE *a, TYPE *b, int n)
        {
          /* try cblas */
        #if #TYPE == "float"
          return cblas_sdot(n, a, 1, b, 1);
        #elif #TYPE == "double"
          return cblas_ddot(n, a, 1, b, 1);
        #else
          int i;
          TYPE s = 0;
          for(i=0;i<n;i++)
            s += a[i] * b[i];
          return s;
        #endif
        }
      #endmacro

      DEFINE_VDOT(Float,float);
      DEFINE_VDOT(Double,double);
      DEFINE_VDOT(Int,int);
```

Details -- The values of the macro parameters are normally
macro-expanded before substituting them into the text of the
macro. However this macro-expansion does not happen when the
substitution occurs in the context of a stringification or token
concatenation operator.  All this is consistent with the standard. The
novelty is that this macro-expansion does not occur either when the
parameter appears in a nested preprocessor directive or multiline
macro.
	
More details -- The stringification operator only works when the next
non-space token is a macro parameter.  This provides a good way to
distinguish a nested directive from a stringification operator
appearing in the beginning of a line.


####  Negative comma in variadic macros

Consider the following variadic macro

```C
     #define macro(msg, ...)  printf(msg, __VA_ARGS__)
```

The C standard says that it is an error to call this macro with only
one argument. Calling this macro with an empty second argument
--`macro(msg,)`-- leaves an annoying comma in the expansion
--`printf(msg,)`-- and causes a compiler syntax error.

This preprocessor accepts invocations of such a macro with a single
argument. The value of parameter `__VA_ARGS__` is then a so-called
negative comma, meaning that the preceding comma is eliminated when
this parameter appears in the macro definition between a comma and a
closing parenthesis.


####  Recursive macros

When a new invocation of the macro appears in the expansion of a
macro, the standard specifies that the preprocessor must rescan the
expansion but should not recursively expand the macro.  Although this
restriction is both wise and useful, there are rare cases where one
would like to use recursive macros.  As an experiment, this recursion
prevention feature is turned off when one defines a multiline macro
with `#defrecursivemacro` instead of `#defmacro`. Note that this might
prevent the preprocessor from terminating unless the macro eventually
takes a conditional branch that does not recursively invoke the macro.



## Program `lcdecl`

## Module `cparser`


## WORK IN PROGRESS


```

 ==================================================
 CPARSER.CPPTOKENITERATOR

 SYNOPSIS
  cppTokenIterator(<options>, <lines>, [<prefix>]])

 SUMMARY
  Argument <lines> is an iterator that produces code lines.  This
  function returns an iterator that produces the tokens obtained by
  preprocessing these code lines. Each call of the token iterator
  returns a token type, a token string, and a location string. Space
  tokens are filtered out. Therefore the token types are 'number',
  'string', 'keyword', 'punctuator', and 'identifier'.  The location
  string follows the pattern <"filename:linenumber">. Optional
  argument <prefix> specifies the initial filename.  
     See the description of <cparser.cpp> for the options
  and extensions supported by the token iterator. Note that
  option <-Zpass> is no longer on by default.

 EXAMPLE
  The following lua expression returns tokens for file <f>:

  for typ,tok,n in cparser.cppTokenIterator({},io.lines(f),f) do
      print(typ,tok,n)
  end

 ==================================================



```lua
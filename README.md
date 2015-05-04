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

- `-Werror` \
  Cause all warning to be treated as errors.
  Note that parsing cannot resume after an error.
  The parser simply throws a Lua error.

- `-w` \
  Do not print warning messages.

- `-D`*sym*`[=`*val*`]` \
   Define preprocessor symbol `sym` to value `val`.
   The default value of `val` is `1`

- `-U`*sym* \
  Undefine preprocessor symbol `sym`.

- `-I`*dir*\
  Add directory `dir` to the search path for included files. Note
  that there is no default search path. When an include file is not
  found the include directive is simply ignored with a warning (but
  see also option `-Zpass`).  Therefore all include directives are
  ignored unless one uses option `-I` to specify the search path.

- `-I-`\
  Marks the beginning of the system include path. When an included
  file is given with angle brackets, (as in `#include <stdio.h>`),
  one only searches directories specified by the `-I` options that
  follow `-I-`. Therefore all these include directives are ignored
  unless one uses option `-I-` followed by one or more option `-I`.

- `-dM`\
  Instead of producing the preprocessed file,
  dumps all macros defined at the end of the parse.

- `-Zcppdef`\
  Run the native preprocessor to initialize
  the predefined macros (cpp -dM < dev/null).

- `-Zpass`\
  This option should be enabled when the output
  of `lcpp` is passed to another preprocessor.
  When this option is provided,
  * the `#pragma` and `#ident` directives are copied
    verbatim to the output,
  * the `#include` directives are copied to the output when
    the file cannot be found in the provided search path,
  * and any directive prefixed with a double `##` is copied to
    the output with a single `#`.  This last feature is
    useful for `#if` directives that depend on symbols
    defined by unresolved `#include` directives.


### Extensions

### Predefined macros



## Program `lcdecl`

## Module `cparser`


## WORK IN PROGRESS


```

 ==================================================
 CPARSER.CPP

 SYNOPSIS
  cparser.cpp(<filename>, [<outputfile>, [<options>]])

 SUMMARY
  Preprocesses file <filename>.
     The optional argument <outputfile> specifies where to write the
  preprocessed file and may be a string or a file descriptor.  The
  default is the standard output.
     The optional argument <options> contains an array of option
  strings.  Most of these options are valid for all cparser exported
  functions and are only documented here. Note that option <-dM> is
  specific to this function and option <-Zpass> is on by default
  unless one gives option "-Znopass".

 OPTIONS
  Array <options> can contain the following options.
  All other options are ignored silently.

 -Werror   Cause all warning to be treated as errors.
           Note that parsing cannot resume after an error.
           The parser simply throws a Lua error.

 -w        Do not print warning messages.

 -D<x>=<y>
 -D<x>     Define preprocessor symbol <x> to value <y>.
           The default value of <y> is 1

 -U<x>     Undefine preprocessor symbol <x>.

 -I<dir>   Add directory <dir> to the search path for
           included files. Note that there is no default
           search path. When an include file is not found
           the include directive is simply ignored with
           a warning (but see also option -Zpass).
           Therefore all include directives are ignored
           unless one uses option <-I> to specify the
           search path.

 -I-       Marks the beginning of the system include path.
           When an included file is given with angle brackets,
           for instance, #include <stdio.h>, one only
           searches directories specified by the <-I> options
           that follow <-I->. Therefore all these include
           directives are ignored unless one uses option <-I->
           followed by one or more option <-I>.

 -dM       Instead of producing the preprocessed file,
           dumps all macros defined at the end of the parse.

 -Zcppdef  Run the native preprocessor to initialize
           the predefined macros (cpp -dM < dev/null).

 -Zpass    This option should be enabled when the output
           of cparser.cpp is passed to another preprocessor.
           When this is enabled,
             * the #pragma and #ident directives are copied
               verbatim to the output,
             * the #include directives are copied to the output when
               the file cannot be found in the provided search path,
             * any directive prefixed with a double ## is copied to
               the output with a single #.  This last feature is
               useful for #if directives that depend on symbols
               defined by system files.

 EXTENSIONS
  This preprocessor implements several nonstandard features.  The
  main feature are multiline macros. The other features are mostly
  here because they make multiline macros more useful.

  Multiline macros
    Preprocessor directives #defmacro and #endmacro can be used to
    define a function-like macro whose body spans several lines. The
    #defmacro directive contains the macro name and a mandatory
    argument list. The body of the macro is composed of all the
    following lines up to the matching #endmacro. This offers
    several benefits:

      * The line numbers of the macro-expansion is preserved. This
        ensures that the compiler produces error messages with
        meaningful line numbers.

      * The multi-line macro can contain preprocessor directives.
        Conditional directives are very useful in this context.
        Note however that preprocessor definitions (with <#define>,
        <#defmacro>, or <#undef>) nested inside multiline macros are
        only valid within the macro.

      * The stringification <#> and token concatenation <##>
        operators can be used freely in the body of multiline
        macros.  Note that these operators only work with the
        parameters of the multiline macros and not with ordinary
        preprocessor definitions. This is consistent with the
        standard behavior of these operators in ordinary
        preprocessor macros.
           Gory details: the values of the macro parameters are
        normally macro-expanded before substituting them into the
        text of the macro. However this macro-expansion does not
        happen when the substitution occurs in the context of a
        stringification or token concatenation operator.  All this
        is consistent with the standard. The novelty is that this
        macro-expansion does not occur either when the parameter
        appears in a nested preprocessor directive or multiline
        macro.
           More gory details: the stringification operator only
        works when the next non-space token is a macro parameter.
        This provides a good way to distinguish a nested directive
        from a stringification operator appearing in the beginning
        of a line.

  String comparison in conditional expressions
    The standard specifies that the expressions following #if
    directives are expressions of C integer type. However this
    processor also handles strings. The only valid operations on
    strings are the equality and ordering comparisons.
      This is quite useful to special cases the parameters
    of a multiline macro:

      #defmacro DEFINE_DOT(TYPE)
        #if #TYPE == "double"
           // call blas code ...
        #else
           // direct implementation ...

  Negative comma in variadic macros
    Consider the following variadic macro

      #define macro(msg, ...)  printf(msg, __VA_ARGS__)

    The standard says that it is an error to call this
    macro with only one argument. Calling this macro with
    an empty second argument -- macro(msg,) -- leaves an
    annoying comma in the expansion -- printf(msg,) --
    and causes a compiler syntax error.
      This preprocessor accepts invocations of such a macro with a
    single argument. The value of parameter <__VA_ARGS__> is then a
    so-called negative comma, meaning that the preceding comma is
    eliminated when this parameter appears in the macro definition
    between a comma and a closing parenthesis.

  Recursive macros
    When a new invocation of the macro appears in the expansion of a
    macro, the standard specifies that the preprocessor must rescan
    the expansion but should not recursively expand the macro.
    Although this restriction is both wise and useful, there are
    rare cases where one would like to use recursive macros.  As an
    experiment, this recursion prevention feature is turned off when
    one defines a multiline macro with #defrecursivemacro instead of
    #defmacro. Note that this might prevent the preprocessor from
    terminating unless the macro eventually takes a conditional
    branch that does not recursively invoke the macro. 


 PREDEFINED MACROS
  The preprocessor always defines <__CPARSER__> to value 1.  The
  value of symbols <__STDC__>, <__STDC_VERSION__>, <__GNUC__>,
  <__GNUC_MINOR__> are either obtained using option <-Zcppdef> or
  dependent on the C dialect selection option <-std=xxx>.
  See function <initialDefines(options)> for all the details.

 ==================================================


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
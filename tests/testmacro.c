/* Copyright (c) Facebook, Inc. and its affiliates.
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cblas.h>
#include <complex.h>

#defmacro DEFINE_SVV(TNAME, TYPE, OPNAME, OPTEXT)

  #define OPER(a,b) OPTEXT

  TYPE TNAME##Vector_##OPNAME(TYPE *a, TYPE *b, int n)
  {
    /* try cblas */
  #if #TYPE == "float" && #OPNAME == "dot"
    return cblas_sdot(n, a, 1, b, 1);
  #elif #TYPE == "double" && #OPNAME == "dot"
    return cblas_ddot(n, a, 1, b, 1);
  #else
    int i;
    TYPE s = 0;
  # pragma unroll(i)
    for(i=0;i<n;i++)
      s += OPER(a[i],b[i]);
    return s;
  #endif
  }

#endmacro

#defmacro FORALLTYPES(macro,...)
  macro(Int, int, __VA_ARGS__)
  macro(Long, long, __VA_ARGS__)
  macro(Float, float, __VA_ARGS__)
  macro(Double, double, __VA_ARGS__)
  macro(Complex, complex double, __VA_ARGS__)
#endmacro

FORALLTYPES(DEFINE_SVV,dot,(a)*(b));
FORALLTYPES(DEFINE_SVV,sqrdist,((a)-(b))*((a)-(b)))


#define DEFINE_TYPEINFO(TNAME,TYPE) { #TNAME, #TYPE },
struct { const char *tname, *type; } typeInfo[] = {
  FORALLTYPES(DEFINE_TYPEINFO)
};


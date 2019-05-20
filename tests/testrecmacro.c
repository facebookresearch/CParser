/* Copyright (c) Facebook, Inc. and its affiliates.
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#defmacro notseq(n)
# if n > 0 
    n, notseq(n-1)
# else
    0
# endif
#endmacro



#defrecursivemacro sequence(n)
# if n > 0 
    n, sequence(n-1)
# else
    0
# endif
#endmacro

#defrecursivemacro infinite(x)
    infinite(x)
#endmacro

int a[] = { notseq(10) };
      
int a[] = { sequence(10) };

#ifdef TESTLOOP
infinite(0);
#endif

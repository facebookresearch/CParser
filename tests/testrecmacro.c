/* Copyright (c) Facebook, Inc. and its affiliates. All Rights Reserved */

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

/* Copyright (c) Facebook, Inc. and its affiliates. All Rights Reserved */

/* Note: This test file is an excerpt of the C99 standard
   http://www.open-std.org/jtc1/sc22/WG14/www/docs/n1256.pdf */

#define hash_hash # ## #
#define mkstr(a) # a
#define in_between(a) mkstr(a)
#define join(c, d) in_between(c hash_hash d)
char p[] = join(x, y); // equivalent to
// char p[] = "x ## y";

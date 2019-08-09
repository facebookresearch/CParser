/* Copyright (c) Facebook, Inc. and its affiliates.
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* Note: This test case was originally contributed by 
   Javier <nanoterrain@yahoo.com> in git commit c6cf0b6fa31
   with the name tst7.c and licensed under the facebook CLA. */

typedef enum
{
  ValueOne,
  ValueTwo = 1,
  ValueThree,
  ValueFour = ValueOne
} SomeEnumeration;

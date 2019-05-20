/* Copyright (c) Facebook, Inc. and its affiliates.
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* Note: This test file is an excerpt of the C99 standard
   http://www.open-std.org/jtc1/sc22/WG14/www/docs/n1256.pdf */

#define OBJ_LIKE (1-1)
#define OBJ_LIKE /* white space */ (1-1) /* other */
#define FUNC_LIKE(a) ( a )
#define FUNC_LIKE( a )( /* note the white space */ \
a /* other stuff on this line 
*/ )


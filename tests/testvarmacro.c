/* Copyright (c) Facebook, Inc. and its affiliates.
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define F_MACRO1(arg1, arg2, ...) \
    do_f(arg1, arg2, __VA_ARGS__)
#define F_MACRO2(arg1, arg2, args...) \
    do_f(arg1, arg2, args)
#define F_MACRO3(arg1, arg2, args...) \
    do_f(arg1, arg2, ##args)


extern void do_f(int,...);

int main(int argc, char **argv)
{
	F_MACRO1(argc,argv[0],argv[1],argv[2],argv[3]);
	F_MACRO1(argc,argv[0]);
	F_MACRO2(argc,argv[0],argv[1],argv[2],argv[3]);
	F_MACRO2(argc,argv[0]);
	F_MACRO3(argc,argv[0],argv[1],argv[2],argv[3]);
	F_MACRO3(argc,argv[0]);
}

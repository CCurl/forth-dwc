// A ColorForth inspired system, MIT license

#ifndef __DWC_H__

#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#define IS_WINDOWS 1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>

#define VERSION         20250606
#define CODE_SZ           0x8000
#define VARS_SZ         0x400000
#define STK_SZ                63
#define NAME_LEN              25
#define IMMED               0x80
#define LIT_MASK      0x40000000
#define LIT_BITS      0x3FFFFFFF
#define CELL_SZ                4
#define byte             uint8_t
#define cell             int32_t
#define ucell           uint32_t
#define btwi(n,l,h)   ((l<=n) && (n<=h))
#define TOS           dstk[dsp]
#define NOS           dstk[dsp-1]
#define L0            lstk[lsp]
#define L1            lstk[lsp-1]

enum { COMPILE=1, DEFINE, INTERPRET, COMMENT };
typedef struct { ucell xt; byte sz; byte fl; byte ln; char nm[NAME_LEN+1]; } DE_T;
typedef struct { char *name; ucell value; } NVP_T;

// These are defined by dwc-vm.c
extern void inner(ucell start);
extern void outer(const char *src);
extern void dwcInit();
extern cell state, outputFp;
extern byte vars[];

// dwc-vm.c needs these to be defined
extern void zType(const char *str);
extern void emit(const char ch);
extern void ttyMode(int isRaw);
extern int  key();
extern int  qKey();
extern cell timer();
extern void ms(cell sleepForMS);
extern cell fOpen(cell name, cell mode);
extern void fClose(cell fh);
extern cell fRead(cell buf, cell sz, cell fh);
extern cell fWrite(cell buf, cell sz, cell fh);
extern cell fSeek(cell fh, cell offset);

#endif //  __DWC_H__

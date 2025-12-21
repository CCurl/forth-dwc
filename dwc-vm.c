#include "dwc-vm.h"

char mem[MEM_SZ], *toIn, wd[32];
ucell *code, dsp, rsp, lsp;
cell dstk[STK_SZ+1], rstk[STK_SZ+1], lstk[STK_SZ+1];
cell here, last, vhere, base, state, outputFp;
DE_T tmpWords[10];

#define PRIMS \
	/* DWC primitives */ \
	X(EXIT,   "exit",     pc = (ucell)rpop(); if (pc==0) { return; } ) \
	X(LIT,    "lit",      push(code[pc++]); ) \
	X(JMP,    "jmp",      pc = code[pc]; ) \
	X(JMPZ,   "jmpz",     if (pop()==0) { pc = code[pc]; } else { pc++; } ) \
	X(JMPNZ,  "jmpnz",    if (pop()) { pc = code[pc]; } else { pc++; } ) \
	X(NJMPZ,  "njmpz",    if (TOS==0) { pc = code[pc]; } else { pc++; } ) \
	X(NJMPNZ, "njmpnz",   if (TOS) { pc = code[pc]; } else { pc++; } ) \
	X(DUP,    "dup",      push(TOS); ) \
	X(DROP,   "drop",     pop(); ) \
	X(SWAP,   "swap",     t = TOS; TOS = NOS; NOS = t; ) \
	X(OVER,   "over",     push(NOS); ) \
	X(STO,    "!",        t = pop(); n = pop(); *(cell*)t = n; ) \
	X(FET,    "@",        TOS = *(cell*)TOS; ) \
	X(CSTO,   "c!",       t = pop(); n = pop(); *(byte*)t = (byte)n; ) \
	X(CFET,   "c@",       TOS = *(byte*)TOS; ) \
	X(RTO,    ">r",       rpush(pop()); ) \
	X(RAT,    "r@",       push(rstk[rsp]); ) \
	X(RFROM,  "r>",       push(rpop()); ) \
	X(MULT,   "*",        t = pop(); TOS *= t; ) \
	X(ADD,    "+",        t = pop(); TOS += t; ) \
	X(SUB,    "-",        t = pop(); TOS -= t; ) \
	X(SLMOD,  "/mod",     t = TOS; n = NOS; TOS = n/t; NOS = n%t; ) \
	X(LT,     "<",        t = pop(); TOS = (TOS  < t) ? 1 : 0; ) \
	X(EQ,     "=",        t = pop(); TOS = (TOS == t) ? 1 : 0; ) \
	X(GT,     ">",        t = pop(); TOS = (TOS  > t) ? 1 : 0; ) \
	X(PLSTO,  "+!",       t = pop(); n = pop(); *(cell *)t += n; ) \
	X(FIND,   "'",        push((cell)findInDict((char *)0)); ) \
	X(FOR,    "for",      lsp += 2; L0 = pop(); L1 = pc; ) \
	X(NEXT,   "next",     if (0 < --L0) { pc = (ucell)L1; } else { lsp = (1<lsp) ? lsp-2: 0; } ) \
	X(AND ,   "and",      t = pop(); TOS &= t; ) \
	X(OR,     "or",       t = pop(); TOS |= t; ) \
	X(XOR,    "xor",      t = pop(); TOS ^= t; ) \
	/* System primitives */ \
	X(KEY,    "key",      push(key()); ) \
	X(QKEY,   "key?",     push(qKey()); ) \
	X(EMIT,   "emit",     emit(pop()); ) \
	X(ZTYPE,  "ztype",    zType((const char*)pop()); ) \
	X(fOPEN,  "fopen",    t = pop(); TOS = fOpen(TOS, t); ) \
	X(fCLOSE, "fclose",   fClose(pop()); ) \
	X(fREAD,  "fread",    t = pop(); n = pop(); TOS = fRead(TOS, n, t); ) \
	X(fWRITE, "fwrite",   t = pop(); n = pop(); TOS = fWrite(TOS, n, t); ) \
	X(MS,     "ms",       ms(pop()); ) \
	X(TIMER,  "timer",    push(timer()); ) \
	X(ADDW,   "add-word", addToDict(0); ) \
	X(OUTER,  "outer",    t = pop(); outer((char*)t); ) \
	X(LASTOP, "system",   system((char*)pop()); )

#define X(op, name, code) op,
enum { PRIMS };

DE_T *addToDict(const char *w);
void outer(const char *src);
void compileNum(cell n);
void push(cell v) { if (dsp < STK_SZ) { dstk[++dsp] = v; } }
cell pop() { return (0 < dsp) ? dstk[dsp--] : 0; }
void rpush(cell v) { if (rsp < STK_SZ) { rstk[++rsp] = v; } }
cell rpop() { return (0 < rsp) ? rstk[rsp--] : 0; }
void comma(ucell val) { code[here++] = val; }
int  changeState(int st) { state = st; return st; }
void addPrim(const char *nm, ucell op) { DE_T *dp = addToDict(nm); if (dp) { dp->xt = op; } }
int  lower(int c) { return btwi(c, 'A', 'Z') ? c+32 : c; }
int  isTmpW(const char *w) { return (w[0]=='t') && btwi(w[1],'0','9') && (w[2]==0) ? 1 : 0; }
void addLit(const char *name, cell val) {
	DE_T *dp = addToDict(name); compileNum(val); comma(EXIT);
	if (btwi(val,0,LIT_BITS)) { dp->fl = INLINE; }
}

int strEqI(const char *src, const char *dst) {
	while (lower(*src) == lower(*dst)) {
		if (*src == 0) { return 1; }
		src++; dst++;
	}
	return 0;
}

void compileNum(cell n) {
	if (btwi(n,0,LIT_BITS)) { comma((ucell)(n | LIT_MASK)); }
	else { comma(LIT); comma(n); }
}

int nextWord() {
	int ln = 0;
	while (*toIn && (*toIn < 33)) {
		if (btwi(*toIn, COMPILE, COMMENT)) { changeState(*toIn); }
		++toIn;
	}
	while (*toIn > 32) { wd[ln++] = *(toIn++); }
	wd[ln] = 0;
	return ln;
}

int isNum(const char *w, cell b) {
	cell n = 0, isNeg = 0;
	if ((w[0] == 39) && (w[2] == 39) && (w[3] == 0)) { push(w[1]); return 1; }
	if (w[0] == '%') { b = 2; ++w; }
	if (w[0] == '#') { b = 10; ++w; }
	if (w[0] == '$') { b = 16; ++w; }
	if ((b == 10) && (w[0] == '-')) { isNeg = 1; ++w; }
	if (w[0] == 0) { return 0; }
	while (*w) {
		char c = lower(*(w++));
		if (btwi(c,'0','9') && btwi(c,'0','0'+b-1)) { n = (n*b)+(c-'0'); }
		else if (btwi(c,'a','a'+b-11)) { n = (n*b)+(c-'a'+10); }
		else return 0;
	}
	push(isNeg ? -n : n);
	return 1;
}

DE_T *addToDict(const char *w) {
	if (!w) {
		if (!nextWord()) return (DE_T*)0;
		w = &wd[0];
	}
	if (isTmpW(w)) { DE_T *x = &tmpWords[w[1]-'0']; x->xt = here; return x; }
	int ln = strlen(w);
	if (ln == 0) { return (DE_T*)0; }
	byte sz = CELL_SZ + 3 + ln + 1;
	while (sz & 0x03) { ++sz; }
	last -= sz;
	if (last < vhere) { last += sz; return (DE_T*)0; }
	DE_T *dp = (DE_T*)last;
	dp->xt = (ucell)here;
	dp->sz = sz;
	dp->fl = 0;
	dp->ln = ln;
	strcpy(dp->nm, w);
	return dp;
}

DE_T *findInDict(char *w) {
	if (!w) {
		if (!nextWord()) return (DE_T*)0;
		w = &wd[0];
	}
	if (isTmpW(w)) { return &tmpWords[w[1]-'0']; }
	cell cw = last, ln = strlen(w);
	while (cw < (cell)&mem[MEM_SZ]) {
		DE_T *dp = (DE_T *)cw;
		if ((dp->ln == ln) && (strEqI(dp->nm, w))) { return dp; }
		cw += dp->sz;
	}
	return (DE_T *)0;
}

#undef X
#define X(op, name, code) case op: code goto next;

void inner(ucell pc) {
	ucell ir;
	cell n, t;
next:
	ir = code[pc++];
	switch (ir)	{
		PRIMS
	default:
		if (LIT_BITS <= ir) { push(ir & LIT_BITS); goto next; }
		if (code[pc] != EXIT) { rpush(pc); }
		pc = ir;
		goto next;
	}
}

int isStateChange(const char *w) {
	if (state == COMMENT) { return 1; }
	if (strEqI(w, "]"))  { return changeState(COMPILE); }
	if (strEqI(w, ":"))  { return changeState(DEFINE); }
	if (strEqI(w, ";"))  { comma(EXIT); return changeState(INTERPRET); }
	if (strEqI(w, "["))  { return changeState(INTERPRET); }
	if (strEqI(w, "("))  {
		while (nextWord() && !strEqI(wd, ")")) { }
		return 1;
	}
	return 0;
}

void outer(const char *src) {
	char *svIn = toIn;
	toIn = (char *)src;
	while (nextWord()) {
		if (isStateChange(wd)) { continue; }
		if (state == DEFINE) { addToDict(wd); changeState(COMPILE); continue; }
		if (isNum(wd, base)) {
			if (state == COMPILE) { compileNum(pop()); }
			continue;
		}
		DE_T *dp = findInDict(wd);
		if (!dp) {
			zType("\n-word:["); zType(wd); zType("]?-\n");
			state = INTERPRET;
			break;
		}
		if ((state == INTERPRET) || (dp->fl & IMMED)) {
			code[10] = dp->xt;
			code[11] = EXIT;
			inner(10);
		}
		else {
			if (dp->fl & INLINE) {
				ucell x = dp->xt;
				while (code[x] != EXIT) { comma(code[x++]); }
			} else { comma(dp->xt); }
		}
	}
	toIn = svIn;
}

#undef X
#define X(op, name, code) { name, op },

void dwcInit() {
	code = (ucell *)&mem[0];
	last = (cell)&mem[MEM_SZ];
	vhere = (cell)&mem[0];
	here = LASTOP+1;
	base = 10;
	state = INTERPRET;
	NVP_T prims[] = { PRIMS { 0, 0 } };
	for (int i = 0; prims[i].name; i++) { addPrim(prims[i].name, prims[i].value); }
	NVP_T nv[] = {
		{ "version", VERSION },        { "(vh)",      (ucell)&vhere },
		{ "(h)",     (cell)&here },    { "(l)",       (cell)&last },
		{ "(sp)",    (cell)&dsp },     { "(stk)",     (cell)&dstk[0] },
		{ "state",   (cell)&state },   { "base",      (cell)&base },
		{ "mem",     (cell)&mem[0] },  { "mem-sz",    (cell)MEM_SZ },
		{ ">in",     (cell)&toIn},     { "output-fp", (cell)&outputFp },
		{ 0, 0 }
	};
	for (int i = 0; nv[i].name; i++) { addLit(nv[i].name, nv[i].value); }
}

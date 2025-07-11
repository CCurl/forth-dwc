// A ColorForth inspired system, MIT license

#include "dwc-vm.h"

#ifdef IS_WINDOWS

#include <windows.h>
#include <conio.h>
int qKey() { return _kbhit(); }
int key() { return _getch(); }
void ttyMode(int isRaw) {}
void ms(cell sleepForMS) { Sleep((DWORD)sleepForMS); }

#endif

// Support for Linux, OpenBSD, FreeBSD
#if defined(__linux__) || defined(__OpenBSD__) || defined(__FreeBSD__)

#include <termios.h>
#include <unistd.h>
#include <sys/time.h>

void ttyMode(int isRaw) {
	static struct termios origt, rawt;
	static int curMode = -1;
	if (curMode == -1) {
		curMode = 0;
		tcgetattr( STDIN_FILENO, &origt);
		cfmakeraw(&rawt);
	}
	if (isRaw != curMode) {
		if (isRaw) {
			tcsetattr( STDIN_FILENO, TCSANOW, &rawt);
		} else {
			tcsetattr( STDIN_FILENO, TCSANOW, &origt);
		}
		curMode = isRaw;
	}
}
int qKey() {
	struct timeval tv;
	fd_set rdfs;
	ttyMode(1);
	tv.tv_sec = 0;
	tv.tv_usec = 0;
	FD_ZERO(&rdfs);
	FD_SET(STDIN_FILENO, &rdfs);
	select(STDIN_FILENO+1, &rdfs, NULL, NULL, &tv);
	int x = FD_ISSET(STDIN_FILENO, &rdfs);
	// ttyMode(0);
	return x;
}
int key() {
	ttyMode(1);
	int x = fgetc(stdin);
	// ttyMode(0);
	return x;
}
void ms(cell sleepForMS) {
	while (sleepForMS > 1000) {
		usleep(500000);
		usleep(500000);
		sleepForMS -= 1000;
	}
	if (sleepForMS > 0) { usleep(sleepForMS * 1000); }
}

#endif // Linux, OpenBSD, FreeBSD

cell timer() { return (cell)clock(); }
void zType(const char *str) { fputs(str, outputFp ? (FILE*)outputFp : stdout); }
void emit(const char ch) { fputc(ch, outputFp ? (FILE*)outputFp : stdout); }

cell fOpen(cell name, cell mode) { return (cell)fopen((char*)name, (char*)mode); }
void fClose(cell fh) { fclose((FILE*)fh); }
cell fRead(cell buf, cell sz, cell fh) { return (cell)fread((char*)buf, 1, sz, (FILE*)fh); }
cell fWrite(cell buf, cell sz, cell fh) { return (cell)fwrite((char*)buf, 1, sz, (FILE*)fh); }
cell fSeek(cell fh, cell offset) { return (cell)fseek((FILE*)fh, (long)offset, SEEK_SET); }

void repl() {
	char tib[128];
	ttyMode(0);
	if (state != COMPILE) { state = INTERPRET; }
	zType((state == COMPILE) ? " ... "  : " ok\n");
	if (fgets(tib, 128, stdin) != tib) { exit(0); }
	outer(tib);
}

void boot(const char *fn) {
	if (!fn) { fn = "boot.fth"; }
	cell fp = fOpen((cell)fn, (cell)"rb");
	if (fp) {
		char *tib = (char*)&mem[10000];
		fRead((cell)tib, 99999, fp);
		fClose(fp);
		outer(tib);
	} else {
		zType("WARNING: unable to open source file!\n");
		zType("If no filename is provided, the default is 'boot.fth'\n");
	}
}

int main(int argc, char *argv[]) {
	dwcInit();
	boot((1<argc) ? argv[1] : 0);
	while (state != 999) { repl(); }
	ttyMode(0);
	return 0;
}

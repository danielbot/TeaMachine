#define LICENSE \
	"TeaMachine (c) Daniel Phillips 2019 is dual licensed under GPLv3 " \
	"plus this text and DDWTFv1. Email ddwtf@phunq.net for details. " \
	"Special shoutout to OGAWA Hirofumi and Daye Dancer. In source and " \
	"binary derivatives of this work this license text must be preserved " \
	"as is. Never be evil. That's it, enjoy"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include <dlfcn.h>
#include <string>
#include <vector>
extern "C" {
#include "hexdump.c"
#include "debug.h"
}

#define trace trace_off

#ifndef DEBUG
#define DEBUG 0
#endif

enum {longbits = 64, longshift = 3, dictalign = longshift, maxplug = 1000};

typedef int s32;
typedef unsigned u32;
typedef unsigned char byte;
typedef long teacode;
typedef	int (native)(int, long *);

template<typename Type>static byte *align(Type p, unsigned bits)
{
	unsigned mask = (1 << bits) - 1; // should compile to constant
	return (Type)p + (-(long)p & mask);
}

extern "C" void error_exit(unsigned exitcode, const char *reason, ...)
{
	va_list arglist;
	va_start(arglist, reason);
	vfprintf(stderr, reason, arglist);
	va_end(arglist);
	fprintf(stderr, "!\n");
	if (DEBUG)
		BREAK;
	exit(exitcode);
}

/* Priority queue for codewalker */
template <typename Type, long LOW = 0> struct minheap
{
	std::vector<Type> vec;

	Type least() { return vec[0]; } // root
	unsigned left(unsigned i) { return ((i << 1) + 1); }
	unsigned right(unsigned i) { return ((i << 1) + 2); }
	unsigned parent(unsigned i) { return (i - 1) >> 1; }
	unsigned size() { return vec.size(); }
	void swap(unsigned i, unsigned j) { std::swap(vec[i], vec[j]); }

	void heapify(unsigned i)
	{
		while (1) {
			/* Heapify node i assuming left and right already heapified */
			unsigned l = left(i), r = right(i), j = i, n = size();
			if (l < n && vec[l] < vec[i])
				j = l;
			if (r < n && vec[r] < vec[j])
				j = r;
			if (i == j)
				break;
			swap(i, j);
			i = j;
		}
	}

	/* Insert unordered at end then reheapify */
	void insert(Type key) { vec.push_back(key); restore(size() - 1); }

	/* Reduce node i to minus infinity then extract */
	void remove(unsigned i) { reduce(i, LOW); extract(); }

	/* Reduce node i to val where val is less than node i */
	void reduce(unsigned i, Type val) { vec[i] = val; restore(i); }

	/* Remove and return minimum element (root) */
	Type extract() {
		assert(size());
		Type root = vec[0];
		vec[0] = vec.back();
		vec.pop_back();
		heapify(0);
		return root;
	}

	/* Restore heap property if violated */
	void restore(unsigned i) {
		for (unsigned j = parent(i); i && vec[i] < vec[j]; i = j)
			swap(i, j);
	}
};

/*
 * Use high bit to mark primitive vs threaded tokens so execute can do the
 * right thing. This bit is available as a flag only if linux never maps
 * virtual addresses way up there and the C compiler never generates label
 * values in that range, which for machine architecture reasons is the
 * case today and for the foreseeable future for 64 bit systems. This bit
 * must be stripped off any token before depositing into a threaded code
 * body or the interpreter will handle it by jumping into oblivion.
 */
const long highbit = 1L << (longbits - 1);

#define PACKED __attribute__((packed))

extern "C" int foo(int argc, long *argv)
{
	printf("I am foo!");
	for (int i = 0; i < argc; i++)
		printf(" %li", argv[i]);
	printf("\n");
	return 0; // return zero results
}

extern "C" int foobar(int argc, char **argv)
{
	printf("foobar");
	for (int i = 0; i < argc; i++)
		printf(" %s", argv[i]);
	printf("\n");
	return 0; // return zero results
}

extern "C" int nada(int argc, long *argv)
{
	trace("I am Nothing!");
	return -1; // abort
}

void *tealib; // one dlopen handle to rule them all

static native *resolve(const void *name, int len)
{
	char silly[len + 1];
	silly[len] = 0;
	memcpy(silly, name, len);
	native *fn = (native *)dlsym(tealib, silly);
	char *error = dlerror();
	if (fn && error)
		error_exit(2, "%s!\n", error);
	if (0)
		printf("%.*s -> %p\n", len, (char *)name, fn);
	if (0 && !fn)
		printf("resolve %.*s failed!\n", len, (char *)name);
	return fn ? fn : nada;
}

int compare(const byte *s1, unsigned n1, const byte *s2, unsigned n2)
{
	unsigned head = std::min(n1, n2);
	int order = memcmp(s1, s2, head);
	trace_off("%.*s %.*s %i %i", head, s1, head, s2, order, (n1 < n2 ? -1 : n1 > n2 ? 1 : 0));
	return !order ? (n1 < n2 ? -1 : n1 > n2 ? 1 : 0) : (order < 0 ? -1 : order > 0 ? 1 : 0);
}

static struct teacodes { teacode
	n, s, cs,
	dup, maydup, over, drop, swap, rot, pick, verso,
	minusone, zero, one, two, three, four, five, six, seven, eight, nine, ten,
	add, sub, mul, div, mod,
	and_, or_, xor_,
	not_, inc, dec, abs, negate, invert,
	eq, neq, less, more, lessq, moreq, zless, compare,
	here, allot, align,
	bytecom, halfcom, comma, compile, comcall,
	fetch, store, cfetch, cstore, count, outside,
	rpop, rpush, jump, if__, unless__, times, do__, loop, i, exit, trap,
	call, execute, native, natex,
	running, comstart, comstop, literal, sep,
	abort, nop, insert4, lookup4, remove4,
	get4, put4, add4, add4x,
	query, rquery, in, out, output, dot, hexdump,
	vocab, to_vocab, find, words,
	start_, finish_, if_, unless_, else_, end_, begin_, do_, while_, loop_, until_, native_,
	run, bye;
} _;

#include <vector>
#include "Shardmap/size.h"
#include "Shardmap/utility.h"
//#include "Shardmap/recops.cc"
#include "Shardmap/shardmap.h"

typedef long dictstar; // dict relative pointer

struct teamachine {

	struct entry {
		dictstar link; byte flags, len, name[0];
		teacode *here() { return (teacode *)align(name + len, dictalign); }
		teacode token() { return *((teacode *)this - 1); }
		dictstar fixlist() { return *((teacode *)this - 2); }
	} PACKED;

	struct symbol {
		teacode token;
		struct entry entry;
	} PACKED;

	struct fixup {
		native *fn;
		dictstar link, unused; // argc was here
		struct entry entry;
	} PACKED;

	struct header {
		char license[sizeof LICENSE];
	} PACKED;

	struct dbreg {
		struct keymap *tab;
		rec_t *rec;
	};

	std::vector<teacode *> body{maxplug};
	std::vector<struct dbreg> db;
	byte *dict, *wall, *here;
	dictstar vocab = 0, fixlist = 0;
	bool mode;

	enum {stacksize = 100, rstacksize = 100, spill = 20, dictsize = 1000};
	long stackbase[stacksize], *stackhome = stackbase + stacksize - spill, *stack = stackhome;
	long rstackbase[rstacksize], *rstackhome = rstackbase + rstacksize, *rstack = rstackhome;

	teamachine(byte *dict = 0, long size = 0, byte *here = 0) :
		dict(dict), wall(dict + size), here(dict), mode(1)
	{
		assert(!(highbit & _.query));

		if (0) {
			native *fn = (native *)dlsym(tealib, "foo");
			char *error = dlerror();
			if (error) {
				printf("%s!\n", error);
				exit(1);
			}
			long args[] = {1, 2, 3}, n = 3;
			printf("fn(%li, argv) (%li) -> %i\n", n, (long)fn, fn(n, args));
			//exit(0);
		}

		struct header *header = (struct header *)(here = dict);
		*header = (struct header){{LICENSE}};
		grow(sizeof *header);
	}

	/* dictionary relative addressing */

	template <typename Type> Type outside(dictstar star)
	{
		return (Type)(star + dict);
	}

	byte *outside(const long relative)
	{
		return relative + dict;
	}

	long *thread(const long relative)
	{
		return (long *)outside(relative);
	}

	dictstar inside(const void *absolute)
	{
		return (byte *)absolute - dict;
	}

	template <typename Type>void comma(Type what)
	{
		*(Type *)here = what;
		here += sizeof what;
	}

	void halfcom(u32 what)
	{
		comma(what);
	}

	void textcom(void *text, unsigned bytes)
	{
		memcpy(here, text, bytes);
		grow(bytes);
	}

	void map_fixups(void (fn)(struct fixup *fixup))
	{
		dictstar star = fixlist;
		while (star) {
			struct fixup *fixup = outside<struct fixup *>(star);
			fn(fixup);
			star = fixup->link;
		}
	}

	void do_fixups()
	{
		map_fixups([](struct fixup *fixup){
			byte *name = fixup->entry.name;
			unsigned len = fixup->entry.len;
			trace_on("fixup %.*s\n", len, name);
			fixup->fn = resolve(fixup->entry.name, fixup->entry.len);
		});
	}

	void list_fixups()
	{
		map_fixups([](struct fixup *fixup){
			printf("%.*s ", fixup->entry.len, fixup->entry.name);
		});
		printf("\n");
	}

	void list_words(dictstar star)
	{
		for (; star; star = outside<struct entry *>(star)->link) {
			struct entry *entry = outside<struct entry *>(star);
			printf("%.*s ", entry->len, entry->name);
		}
		printf("\n");
	}

	struct entry *probe(dictstar star, byte *name, byte len)
	{
		for (; star; star = outside<struct entry *>(star)->link) {
			struct entry *entry = outside<struct entry *>(star);
			if (0)
				hexdump(entry->name, entry->len);
			if (entry->len == len && !memcmp(entry->name, name, len))
				return entry;
		}
		return NULL;
	}

	struct entry *symcom(const byte *name, byte len, byte flags)
	{
		trace("%s, flags %i", name, flags);
		struct entry *entry = (struct entry *)here;
		*entry = {vocab, flags, len};
		memcpy(entry->name, name, len);
		grow(sizeof(struct entry) + len);
		vocab = inside(entry);
		return entry;
	};

	struct fixup *natcom(const byte *name, byte len)
	{
		native *fn = resolve(name, len);
		struct fixup *fixup = (struct fixup *)here;
		grow(sizeof(struct fixup) - sizeof(struct entry));
		symcom(name, len, 2);
		fixup->unused = 0; // never stomp name until after resolve
		fixup->link = (u32)fixlist;
		fixup->fn = fn;
		fixlist = inside(fixup);
		return fixup;
	};

	int run(teacode *boot);
	void grow(unsigned bytes) { here = align(here + bytes, dictalign); }
};

struct teacom
{
	enum {immedflag = 0x80, embedflag = 0x40};
	enum {basekind = 1}; // kind zero illegal for now
	enum {immed = immedflag | basekind, kindmask = 0xf}; // two bits unused
	enum {stacksize = 100, spill = 0};

	struct control
	{
		u32 what, nest;
		s32 source, target; // relative to body start
		control(unsigned what, int source = 0, int target = 0) : what(what), nest(0), source(source), target(target) {}
		control(){}

		void push(long *&stack) { *--*(control **)&stack = *this; }
		struct control pop(long *&stack) { return *(*(control **)&stack)++; }
		static void error() { throw std::string("Syntax"); }
	};

	struct teamachine &vm;
	long *stackhome, *&stack;
	byte *&here;
	long *where;

	teacom(struct teamachine &vm, long *&stack, long *stackbase) :
		vm(vm), stackhome(stack), stack(stack),
		here(vm.here), where((long *)here) {}

	unsigned if_magic = *(u32*)(const char []){"if.."};
	unsigned else_magic = *(u32*)(const char []){"else"};
	unsigned do_magic = *(u32*)(const char []){"do.."};
	unsigned begin_magic = *(u32*)(const char []){"begi"};
	unsigned while_magic = *(u32*)(const char []){"whil"};
	unsigned loop_magic = *(u32*)(const char []){"loop"};
	unsigned nest_magic = *(u32*)(const char []){"nest"};

	long at() { return (long *)here - where; }
	void out(teacode op) { *(long *)here = op; here += sizeof(long); }
	void arg(teacode op) { out(op); }
	void op(teacode op) { out(op); }
	void lit(long value) { out(_.n); out(value); }
	void call(teacode body) { op(_.call); arg(body); }
	void text(const u8 *text, unsigned len) { op(_.cs); *here++ = len; memcpy(here, text, len); vm.grow(len); }
	void cstring(const char *s) { text((const u8 *)s, strlen(s)); }
	void dumpstack() { hexdump(stack, (char *)stackhome - (char *)stack); }

	control pop() {
		if (stack >= stackhome)
			throw std::string("Context");
		return ((control *)stack)->pop(stack);
	};

	dictstar colon(const byte *name, byte len, teacode *body, unsigned bytes, byte flags)
	{
		struct teamachine::entry *entry = vm.symcom((const byte *)name, len, flags | embedflag);
		vm.textcom(body, bytes);
		return vm.inside(entry->here());
	};

	/* wrappers for oldschool zstrings */

	dictstar colon(const char *name, teacode *body = 0, unsigned bytes = 0, byte flags = basekind)
	{ return colon((const byte *)name, strlen(name), body, bytes, flags); }

	struct teamachine::fixup *natcom(const char *name)
	{ return vm.natcom((const byte *)name, strlen(name)); };

	void start_()
	{
		where = (long *)here;
		control(nest_magic).push(stack);
	}

	void finish_()
	{
		if (((control *)stack)->pop(stack).what != nest_magic)
			control::error();
		op(_.exit);
		if (0)
			hexdump(where, at() << longshift);
	}

	void if_(bool unless = 0)
	{
		trace("if at %li", at());
		op(unless ? _.unless__ : _.if__);
		arg(0);
		control(if_magic, at()).push(stack);
	};

	void unless_()
	{
		if_(1);
	};

	void else_()
	{
		control was = pop();
		if (was.what != if_magic)
			control::error();

		trace("if from %i", was.source);
		op(_.jump);
		arg(0);
		(was.source + where)[-1] = (at()) - was.source; // forward patch
		control(else_magic, at()).push(stack);
	};

	void end_()
	{
		control was = pop();
		if (was.what != if_magic && was.what != else_magic)
			control::error();

		trace("if or else from %i", was.source);
		(was.source + where)[-1] = (at()) - was.source; // forward patch
	};

	void begin_()
	{
		trace("begin at %li", at());
		control(begin_magic, 0, at()).push(stack);
	};

	void do_(bool based = 1)
	{
		trace("do at %li", at());
		op(based ? _.do__: _.times);
		arg(0);
		control(do_magic, at(), at()).push(stack);
	};

	void while_()
	{
		control was = pop();
		if (was.what != begin_magic)
			control::error();

		trace("begin from %i", was.source);
		op(_.if__);
		arg(0);
		control(while_magic, at(), was.target).push(stack);
	};

	void loop_(bool until = 0)
	{
		control was = pop();

		trace("loop begin %i, loop at %li", was.target, at());

		if (was.what != begin_magic && was.what != while_magic && was.what != do_magic)
			control::error();

		if (was.what == do_magic && until) // maybe implement this later
			control::error();

		op(was.what == do_magic ? _.loop : until ? _.if__ : _.jump);
		long from = (at()) + 1;
		arg(was.target - from);

		if (was.what == while_magic || was.what == do_magic) {
			trace("while from %i", was.target);
			(was.source + where)[-1] = (at()) - was.source; // forward patch
		}
	};

	void until_()
	{
		loop_(1);
	};

	int bootstrap();
};

int teacom::bootstrap()
{
	enum {stacksize = 100, spill = 0};
	long stackbase[stacksize], *stackhome = stackbase + stacksize - spill, *stack = stackhome;
	vm.run(0); // discover labels

	struct { long token; const char *name; byte flags = basekind; } core[] = {
		{_.bye, "bye"},
		{_.query, "?"},
		{_.abort, "abort"},
		{_.here, "here"},
		{_.vocab, "vocab"},
		{_.to_vocab, "vocab!"},
		{_.allot, "allot"},
		{_.align, "align"},
		{_.execute, "execute"},
		{_.bytecom, "c,"},
		{_.halfcom, "half,"},
		{_.comma, ","},
		{_.fetch, "@"},
		{_.store, "!"},
		{_.cfetch, "c@"},
		{_.cstore, "c!"},
		{_.zero, "0"},
		{_.one, "1"},
		{_.two, "2"},
		{_.three, "3"},
		{_.dup, "dup"},
		{_.maydup, "?dup"},
		{_.over, "over"},
		{_.swap, "swap"},
		{_.drop, "drop"},
		{_.verso, "verso"},
		{_.dot, "."},
		{_.count, "count"},
		{_.outside, "outside"},
		{_.output, "output"},
		{_.hexdump, "hexdump"},
		{_.words, "words"},
		{_.not_, "not"},
		{_.less, "<"},
		{_.sub, "-"},
		{_.add, "+"},
		{_.mul, "*"},
		{_.div, "/"},
		{_.mod, "mod"},
		{_.and_, "and"},
		{_.or_, "or"},
		{_.xor_, "xor"},
		{_.eq, "="},
		{_.neq, "<>"},
		{_.less, "<"},
		{_.more, ">"},
		{_.lessq, "<="},
		{_.moreq, ">="},
		{_.zless, "0<"},
		{_.compare, "compare"},
		{_.comstart, "]"},
		{_.comstop, "[", immed},
		{_.start_, "nest", immed},
		{_.finish_, "back", immed},
		{_.if_, "if", immed},
		{_.unless_, "unless", immed},
		{_.else_, "else", immed},
		{_.end_, "then", immed},
		{_.end_, "end", immed},
		{_.begin_, "begin", immed},
		{_.do_, "do", immed},
		{_.while_, "while", immed},
		{_.loop_, "loop", immed},
		{_.until_, "until", immed},
	};

	enum {symbols = sizeof core / sizeof *core};

	/* create bootstrap vocabulary */

	for (unsigned i = 0; i < sizeof core / sizeof *core; i++)
		[this](const char *name, byte flags, teacode token)
		{
			vm.comma(token | highbit);
			vm.symcom((const byte *)name, strlen(name), flags);
		}
		(core[i].name, core[i].flags, core[i].token | highbit);

	if (0)
		printf("sizeof(struct control) %lu\n", sizeof(struct control));

	if (0) {
		/* learn about the memory map, because we abuse the address high bit */
		static const char hello[] = "hello";
		printf("%lx\n%p\n%p\n%p\n%p\n", _.if__, &hello, here, new control, &stackhome);
		return 0;
	}

	if (0) {
		colon("nop");
		start_();
		finish_();
	}

	long chop[] = {_.here, _.sub, _.allot, _.exit};
	long inskip[] = {_.zero, _.drop, _.in, _.dup, _.sep, _.unless__, -6, _.exit};
	long huh[] = {_.output, _.n, ' ', _.out, _.n, '?', _.out, _.n, '\n', _.out, _.exit};
	teacode huh_ = colon("??", huh, sizeof huh);
	teacode chop_ = colon("chop", chop, sizeof chop);
	teacode inskip_ = colon("inskip", inskip, sizeof inskip);

	teacode wordin_ = colon("wordin");
	start_();
	op(_.here);
		op(_.zero); op(_.bytecom);
		call(inskip_);
		begin_();
			op(_.bytecom);
			op(_.in);
			op(_.dup);
			op(_.sep);
		until_();
		op(_.drop);
	op(_.here); op(_.over); op(_.sub); op(_.dec); op(_.swap); op(_.cstore);
	finish_();

	teacode inword_ = colon("inword");
	start_();
	op(_.here); call(wordin_); op(_.dup); call(chop_);
	finish_();

	colon("'", 0, 0, immed);
	start_();
	call(inword_); op(_.find); op(_.maydup); if_();
		op(_.abs); op(_.dec); if_(); op(_.abort); end_();
		op(_.running); unless_(); op(_.compile); end_(); op(_.exit);
	end_();
	op(_.count), call(huh_); op(_.abort);
	finish_();

	teacode make_ = colon("make"); // ( flags -> )
	start_();
	op(_.vocab); op(_.comma); op(_.bytecom);
	call(wordin_); op(_.align);
	finish_();

	colon("native");
	start_();
	call(inword_);
	op(_.native_);
	finish_();

	colon(":", 0, 0, immed); // ( -> where )
	start_();
	op(_.here);
	lit(basekind | embedflag);
	call(make_);
	op(_.start_);
	op(_.comstart);
	finish_();

	colon(";", 0, 0, immed);
	start_();
	op(_.n);
	arg(_.exit);
	op(_.comma);
	op(_.comstop);
	op(_.finish_);
	op(_.to_vocab);
	finish_();

	teacode string_ = colon("\""); // not immediate for now (check ANS for guidance)
	start_();
	begin_();
		op(_.in);
		op(_.dup); lit('\\'); op(_.eq); if_(); // of!
			op(_.drop); op(_.in); op(_.one);
		else_();
			op(_.dup); lit('"'); op(_.neq);
		end_();
	while_();
		op(_.bytecom);
	loop_();
	op(_.drop);
	finish_();

	teacode cstring_ = colon("c\"", 0, 0, immed);
	start_();
	lit(_.cs);
	op(_.comma);
	op(_.here);
	op(_.zero); op(_.bytecom);
	call(string_);
	op(_.here); op(_.over); op(_.sub); op(_.dec); op(_.swap); op(_.cstore);
	op(_.align);
	finish_();

	colon(".\"", 0, 0, immed);
	start_();
	call(cstring_);
	lit(_.count);
	op(_.comma);
	lit(_.output);
	op(_.comma);
	finish_();

	teacode dotcr_ = colon(".cr");
	start_();
	lit('\n');
	op(_.out);
	finish_();

	struct teamachine::fixup *fixfoo = natcom("foo");
	struct teamachine::fixup *fixbar = natcom("bar");
	struct teamachine::fixup *fixfoobar = natcom("foobar");
	struct teamachine::fixup *native_tpcb = natcom("tpcb_main");

	teacode tcpb_ = colon("tpcb");
	start_();
	cstring("tea"); op(_.zero); op(_.bytecom); op(_.inc); op(_.outside); op(_.dup);
	op(_.two); op(_.rpush);
	begin_();
		op(_.here);
		call(wordin_); op(_.zero); op(_.bytecom);
		op(_.dup); op(_.count); cstring(";"); op(_.count); op(_.compare);
	while_();
		op(_.inc); op(_.outside);
		op(_.rpop); op(_.inc); op(_.rpush);
	loop_();
	op(_.drop);
	op(_.i); op(_.verso);
	op(_.rpop); op(_.native); arg(vm.inside(native_tpcb));

	finish_();

	colon(".cr");
	start_();
	lit('\n');
	op(_.out);
	finish_();

	if (0) {
		colon("test");
		start_();
		op(_.native); arg(vm.inside(fixfoo));
		finish_();
	}

	if (0)
		vm.list_fixups();

	if (0)
		vm.list_words(vm.vocab);

	return 0;
}

int teamachine::run(teacode *next)
{
	struct teacom tea{*this, stack, stackbase}; // optional

	if (next)
		goto **next++;

	/* boot the op table starting with convenience ops */
	_.hexdump = (long)&&hexdump;
	_.query = (long)&&query;
	_.rquery = (long)&&rquery;
	_.dot = (long)&&dot;

	/* io ops */
	_.in = (long)&&in;
	_.out = (long)&&out;
	_.output = (long)&&output;

	/* stack ops */
	_.dup = (long)&&dup;
	_.maydup = (long)&&maydup;
	_.over = (long)&&over;
	_.drop = (long)&&drop;
	_.swap = (long)&&swap;
	_.rot = (long)&&rot;
	_.pick = (long)&&pick;
	_.verso = (long)&&verso;
	_.rpop = (long)&&rpop;
	_.rpush = (long)&&rpush;

	/* literals */
	_.n = (long)&&n;
	_.s = (long)&&s;
	_.cs = (long)&&cs;
	_.minusone = (long)&&minusone;
	_.zero = (long)&&zero;
	_.one = (long)&&one;
	_.two = (long)&&two;
	_.three = (long)&&three;
	_.four = (long)&&four;
	_.five = (long)&&five;
	_.six = (long)&&six;
	_.seven = (long)&&seven;
	_.eight = (long)&&eight;
	_.nine = (long)&&nine;
	_.ten = (long)&&ten;

	/* arithmetic */
	_.add = (long)&&add;
	_.sub = (long)&&sub;
	_.mul = (long)&&mul;
	_.div = (long)&&div;
	_.mod = (long)&&mod;

	/* binary ops */
	_.and_ = (long)&&and_;
	_.or_ = (long)&&or_;
	_.xor_ = (long)&&xor_;

	/* unary ops */
	_.not_ = (long)&&not_;
	_.inc = (long)&&inc;
	_.dec = (long)&&dec;
	_.abs = (long)&&abs;
	_.invert = (long)&&invert;
	_.negate = (long)&&negate;

	/* predicates */
	_.eq = (long)&&eq;
	_.neq = (long)&&neq;
	_.less = (long)&&less;
	_.more = (long)&&more;
	_.lessq = (long)&&lessq;
	_.moreq = (long)&&moreq;
	_.zless = (long)&&zless;
	_.compare = (long)&&compare;

	/* storage */
	_.here = (long)&&here;
	_.allot = (long)&&allot;
	_.align = (long)&&align;
	_.bytecom = (long)&&bytecom;
	_.halfcom = (long)&&halfcom;
	_.comma = (long)&&comma;
	_.compile = (long)&&compile;
	_.comcall = (long)&&comcall;
	_.fetch = (long)&&fetch;
	_.store = (long)&&store;
	_.cfetch = (long)&&cfetch;
	_.cstore = (long)&&cstore;
	_.count = (long)&&count;
	_.outside = (long)&&outside;

	/* dictionary */
	_.vocab = (long)&&vocab;
	_.to_vocab = (long)&&to_vocab;
	_.find = (long)&&find;
	_.words = (long)&&words;

	/* control */
	_.if__ = (long)&&if__;
	_.unless__ = (long)&&unless__;
	_.jump = (long)&&jump;
	_.do__ = (long)&&do__;
	_.times = (long)&&times;
	_.loop = (long)&&loop;
	_.i = (long)&&i;
	_.exit = (long)&&exit;
	_.call = (long)&&call;
	_.execute = (long)&&execute;
	_.native = (long)&&native;
	_.natex = (long)&&natex;
	_.trap = (long)&&trap;

	/* boot compiler */
	_.sep = (long)&&sep;
	_.running = (long)&&running;
	_.comstart = (long)&&comstart;
	_.comstop = (long)&&comstop;
	_.literal = (long)&&literal;
	_.start_ = (long)&&start_;
	_.finish_ = (long)&&finish_;
	_.if_ = (long)&&if_;
	_.unless_ = (long)&&unless_;
	_.else_ = (long)&&else_;
	_.end_ = (long)&&end_;
	_.begin_ = (long)&&begin_;
	_.do_ = (long)&&do_;
	_.while_ = (long)&&while_;
	_.loop_ = (long)&&loop_;
	_.until_ = (long)&&until_;

	/* machinery */
	_.nop = (long)&&nop;
	_.abort = (long)&&abort;

	/* db */
	_.insert4 = (long)&&insert4;
	_.lookup4 = (long)&&lookup4;
	_.remove4 = (long)&&remove4;
	_.get4 = (long)&&get4;
	_.put4 = (long)&&put4;
	_.add4 = (long)&&add4;
	_.add4x = (long)&&add4x;

	/* externally definable primitives */
	_.native_ = (long)&&native_;
	_.run = (long)&&run;
	//#include "wlist.inc"
	_.bye = (long)&&bye;
bye: return 0;

/* creature comforts */

hexdump: {
	long n = *stack++;
	byte *p = outside(*stack++);
	hexdump(p, n); }
	goto **next++;

query: {
	printf("-> ");
	if (0)
		printf("(%li) ", stackhome - stack);
	for (long *p = stackhome; --p >= stack;)
		printf("%li ", *p);
	printf("\n"); }
	goto **next++;

rquery: {
	printf("=> ");
	for (long *p = rstackhome; --p >= rstack;)
		printf("%li ", *p);
	printf("\n"); }
	goto **next++;

dot: printf("%li ", *stack++); // ( a -> )
	goto **next++;

words: list_words(vocab);
	goto **next++;

/* stream io */

in:
	if (0) { // via simple syscall
		byte c;
		if (!read(0, &c, 1))
			goto bye;
		*--stack = c;
	} else // via libc stream
		if ((*--stack = getchar()) < 0)
			goto bye;
	goto **next++;

out:
	if (0) {
		char c = *stack++;
		if (write(0, &c, 1) < 1)
			goto bye;
	} else
		putchar(*stack++);
	goto **next++;

output: {
	long n = *stack++;
	byte *p = outside(*stack++);
	if (0) {
		if (write(0, p, n) < n)
			goto bye;
	} else
		while (n--)
			putchar(*p++); }
	goto **next++;

/* stack ops */

dup: { long a = stack[0]; *--stack = a; } // ( a -> a a )
	goto **next++;

maydup: { long a = stack[0]; if (a) *--stack = a; } // ( a -> a 1 | 0 )
	goto **next++;

over: { long a = stack[1]; *--stack = a; } // ( a b -> a b a )
	goto **next++;

drop: stack++; // ( a -> ) // empty???
	goto **next++;

swap: { long a = stack[0]; stack[0] = stack[1]; stack[1] = a; }; // ( a b -> b a )
	goto **next++;

rot: { long x = stack[0]; stack[0] = stack[2]; stack[2] = stack[1]; stack[1] = x; }
	goto **next++;

pick: stack[0] = stack[stack[0] + 1]; // ( i -> a )
	goto **next++;

verso:
	{
		unsigned n = *stack++, m = (n + 1) / 2;
		for (unsigned i = 0, j = i + n - 1; i < m; i++, j--)
			std::swap(stack[i], stack[j]);
	}
	goto **next++;

rpop: *--stack = *rstack++;
	goto **next++;

rpush: *--rstack = *stack++;
	goto **next++;

/* literals */

n: *--stack = *next++; // ( -> n )
	goto **next++;

s: { // ( -> text count )
	unsigned c = *(byte *)next;
	*--stack = inside(next + 1);
	*--stack = c;
	next = (long *)((byte *)next + 1 + c + (~c & 7)); }
	goto **next++;

cs: { // ( -> cstring )
	unsigned c = *(byte *)next;
	*--stack = inside(next);
	next = (long *)((byte *)next + 1 + c + (~c & 7)); }
	goto **next++;

minusone: *--stack = -1; goto **next++; // ( -> -1 )
zero: *--stack = 0; goto **next++; // ( -> 0 )
one: *--stack = 1; goto **next++; // ( -> 1 )
two: *--stack = 2; goto **next++; // ( -> 2 )
three: *--stack = 3; goto **next++; // ( -> 3 )
four: *--stack = 4; goto **next++; // ( -> 4 )
five: *--stack = 5; goto **next++; // ( -> 5 )
six: *--stack = 6; goto **next++; // ( -> 6 )
seven: *--stack = 7; goto **next++; // ( -> 7 )
eight: *--stack = 8; goto **next++; // ( -> 8 )
nine: *--stack = 9; goto **next++; // ( -> 9 )
ten: *--stack = 10; goto **next++; // ( -> 10 )

/* arithetic */

add: { long b = *stack++; stack[0] += b; }
	goto **next++;

sub: { long b = *stack++; stack[0] -= b; }
	goto **next++;

mul: { long b = *stack++; stack[0] *= b; }
	goto **next++;

div: { long b = *stack++; stack[0] = floor(float(stack[0]) / float(b)); }
	goto **next++;

mod: { long b = *stack++, a = stack[0], r = a % b;
	stack[0] = ((r < 0) != (b < 0)) && r ? r + b : r; }
	goto **next++;

and_: { long b = *stack++; stack[0] &= b; }
	goto **next++;

or_: { long b = *stack++; stack[0] |= b; }
	goto **next++;

xor_: { long b = *stack++; stack[0] ^= b; }
	goto **next++;

not_: stack[0] = !stack[0];
	goto **next++;

inc: stack[0]++;
	goto **next++;

dec: stack[0]--;
	goto **next++;

abs:
	if (stack[0] < 0)
		stack[0] = -stack[0];
	goto **next++;

negate: stack[0] = -stack[0];
	goto **next++;

invert: stack[0] = ~stack[0];
	goto **next++;

/* predicates */

eq: { long b = *stack++; stack[0] = stack[0] == b; }
	goto **next++;

neq: { long b = *stack++; stack[0] = stack[0] != b; }
	goto **next++;

less: { long b = *stack++, a = *stack++; *--stack = a < b; }
	goto **next++;

more: { long b = *stack++, a = *stack++; *--stack = a > b; }
	goto **next++;

lessq: { long b = *stack++, a = *stack++; *--stack = a <= b; }
	goto **next++;

moreq: { long b = *stack++, a = *stack++; *--stack = a >= b; }
	goto **next++;

zless: stack[0] = stack[0] < 0;
	goto **next++;

compare: { // ( text len text len -> n )
	unsigned n2 = *stack++;	byte *s2 = outside(*stack++);
	unsigned n1 = *stack++;	byte *s1 = outside(stack[0]);
	trace_off("%p %u %p %u", s1, n1, s2, n2);
	stack[0] = compare(s1, n1, s2, n2);	}
	goto **next++;

/* memory */

fetch:
	stack[0] = *(long *)outside(stack[0]); // ( p -> v )
	goto **next++;

store: *(long *)outside(stack[0]) = stack[1]; // ( v p -> )
	stack += 2;
	goto **next++;

cfetch: stack[0] = *outside(stack[0]); // ( p -> c )
	goto **next++;

cstore: *outside(stack[0]) = stack[1]; // ( c p -> )
	stack += 2;
	goto **next++;

count: { // ( text -> text+1 byte )
	unsigned c = *outside(stack[0]++);
	*--stack = c; }
	goto **next++;

outside: // ( p -> outside )
	stack[0] = (long)outside(stack[0]);
	goto **next++;

/* control */

jump: { long delta = *next++; next += delta; }
	goto **next++;

if__: { long delta = *next++; if (!*stack++) next += delta; } // ( ? -> )
	goto **next++;

unless__: { long delta = *next++; if (*stack++) next += delta; } // ( ? -> )
	goto **next++;

times: *--stack = 0; // then...
do__: {
	long delta = *next++;
	long start = *stack++, limit = *stack++;
	if (start >= limit) {
		next += delta;
		goto **next++;
	}
	*--rstack = limit;
	*--rstack = start; }
	goto **next++;

loop: {
	long delta = *next++;
	if (++rstack[0] < rstack[1]) {
		next += delta;
		goto **next++;
	}
	rstack += 2; }
	goto **next++;

i: *--stack = rstack[0];
	goto **next++;

call:
	*--rstack = (long)(next + 1);
	next = thread(*next);
	goto **next++;

execute: {
	long what = *stack++;
	if (what < 0)
		goto *(what & ~highbit);
	*--rstack = (long)next;
	next = thread(what); }
	goto **next++;

native: {
	const struct fixup *fixup = outside<struct fixup *>(*next++);
	unsigned int argc = *stack++;
	trace_off("(%p)(%i, %p)", fixup->fn, argc, stack);
	unsigned int keep = (fixup->fn)(argc, stack);
	trace_off("drop %i %i", argc, keep > argc ? argc : keep);
	stack += argc - (keep > argc ? argc : keep); }
	goto **next++;

natex: {
	const struct fixup *fixup = outside<struct fixup *>(*stack++); // the only difference vs native
	unsigned int argc = *stack++;
	trace_off("(%p)(%i, %p)", fixup->fn, argc, stack);
	unsigned int keep = (fixup->fn)(argc, stack);
	trace_off("drop %i %i", argc, keep > argc ? argc : keep);
	stack += argc - (keep > argc ? argc : keep); }
	goto **next++;

exit:
	next = (long *)*rstack++;
	goto **next++;

trap:
	BREAK;
	goto **next++;

/* dictionary */

here: *--stack = inside(here); // ( -> p )
	goto **next++;

allot: here += *stack++; // ( bytes -> )
	goto **next++;

align: {
	byte *where = (byte *)here;
	here = align(here, longshift);
	memset(where, 0, (byte *)here - where); }
	goto **next++;

bytecom: *here++ = *stack++; // ( c -> )
	goto **next++;

halfcom: halfcom(*stack++); // ( h -> )
	goto **next++;

comma: comma(*stack++); // ( n -> )
	goto **next++;

compile: // ( n -> )
	if (stack[0] >= 0)
		comma(_.call);
	comma(*stack++ & ~highbit);
	goto **next++;

comcall: { // ( p -> )
	if (0)
		hexdump(outside<struct fixup *>(stack[0]), sizeof(struct fixup));
	comma(_.native);
	comma(*stack++); }
	goto **next++;

/* compiler */

vocab: *--stack = vocab; // ( -> p )
	goto **next++;

to_vocab: vocab = *stack++; // ( p -> )
	goto **next++;

running: *--stack = mode;
	goto **next++;

comstart: mode = 0;
	goto **next++;

comstop: mode = 1;
	goto **next++;

find: { // ( cstring -> token 1 | token -1 | cstring 0 )
	byte *string = outside(stack[0]);
	struct entry *entry = probe(vocab, string + 1, string[0]);
	*--stack = 0;
	if (entry) {
		byte flags = entry->flags, kind = flags & teacom::kindmask;
		assert(kind);
		stack[1] = (flags & teacom::embedflag) ? inside(entry->here()) :
			kind == 2 ? inside((byte *)entry - offsetof(struct fixup, entry)) : entry->token();
		stack[0] = (flags & teacom::immedflag) | mode ? -kind : kind;
	} }
	goto **next++;

sep: stack[0] = !isgraph(stack[0]);
	goto **next++;

literal: {
	byte *string = outside(stack[1]), *limit = string + stack[0];
	long base = 10, n = 0;
	bool neg = 0;
	if (string == limit)
		goto badnum;
	if (string[0] == '-') {
		if (++string == limit)
			goto badnum;
		neg = 1;
	}
	// 0x handling goes here!
	while (string < limit) {
		char c = *string++;
		int d = c - '0' < 10 ? c - '0' : base == 0x10 && tolower(c) - 'a' < 6 ? tolower(c) - 'a' + 10 : -1;
		if (d < 0)
			goto badnum;
		n = n * base + d;
	}
	stack[1] = neg ? -n : n;
	stack[0] = 1;
	goto **next++;
badnum:
	*--stack = 0; }
	goto **next++;

start_: tea.start_();
	goto **next++;
finish_: tea.finish_();
	goto **next++;
if_: tea.if_(0);
	goto **next++;
unless_: tea.if_(1);
	goto **next++;
else_: tea.else_();
	goto **next++;
end_: tea.end_();
	goto **next++;
begin_: tea.begin_();
	goto **next++;
do_: tea.do_();
	goto **next++;
while_: tea.while_();
	goto **next++;
loop_: tea.loop_();
	goto **next++;
until_: tea.until_();
	goto **next++;

native_: { // ( cstring -> )
	byte *name = outside(*stack++);
	natcom(name + 1, name[0]); }
	goto **next++;

	/* db */

insert4: { // ( data key4 -> )
	u32 key = (u32)*stack++;
	int id = *next++, delta = *next++;
	if (!(db[id].rec = db[id].tab->insert((u8 *)&key, 4, outside(*stack++))))
		next += delta;
	printf("rec %p\n", db[id].rec);
	if (db[id].rec) hexdump(db[id].rec, db[id].tab->reclen); }
	goto **next++;

lookup4: { // ( key4 -> )
	u32 key = (u32)*stack++;
	int id = *next++, delta = *next++;
	if (!(db[id].rec = db[id].tab->lookup((u8 *)&key, 4)))
		next += delta;
	printf("rec %p\n", db[id].rec);
	if (db[id].rec) hexdump(db[id].rec, db[id].tab->reclen); }
	goto **next++;

remove4: { // ( key4 -> )
	u32 key = (u32)*stack++;
	int id = *next++, delta = *next++;
	if (!db[id].tab->remove((u8 *)&key, 4))
		next += delta;
	db[id].rec = 0; }
	goto **next++;

get4: { // ( -> u32 )
	printf("get %p\n", db[0].rec);
	hexdump(db[0].rec, db[0].tab->reclen);
	unsigned id = *next++, field = *next++;
	*--stack = *(u32 *)(db[id].rec + field); }
	goto **next++;

put4: { // ( u32 -> )
	unsigned id = *next++, field = *next++;
	*(u32 *)(db[id].rec + field) = *stack++; }
	goto **next++;

add4: { // ( u32 -> )
	unsigned id = *next++, field = *next++;
	*(u32 *)(db[id].rec + field) += *stack++; }
	goto **next++;

add4x: { // ( u32 -> u32 )
	unsigned id = *next++, field = *next++;
	stack[0] = *(u32 *)(db[id].rec + field) += stack[0]; }
	goto **next++;

abort:
	stack = stackhome;
	rstack = rstackhome;
	mode = 1;
	next = body[0];
	/*
	 * Interpret body[0], which should never come back, but what if it does?
	 * Return should therefore point at a cell containing bye
	 */
nop:
	goto **next++;

run: *--rstack = (long)next; next = body[0];
	goto **next++;
}

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

extern "C" int tpcb_main(int argc, const char *argv[]);

int main(int argc, const char *argv[])
{
	if (0) {
		printf("compare -> %i\n", compare((const byte *)"aa", 2, (const byte *)"ab", 2));
		return 0;
	}

	tealib = dlopen(NULL, RTLD_NOW); // _LAZY also works
	if (!tealib) {
		printf("%s!\n", dlerror());
		exit(1);
	}

	enum {dictsize = 10000};

	teamachine vm((byte *)malloc(dictsize), dictsize);
	struct teacom tea{vm, vm.stack, vm.stackbase};
	try { tea.bootstrap(); } catch (std::string why) { printf("%s!\n", why.c_str()); }

	/* convert absolute threaded code address to dict relative */
	auto inside = [&vm](long *body) { return vm.inside(body); };

	long hello[] = {_.cs, 0, 0, 0, _.count, _.output, _.exit};
	memcpy((char *)(hello + 1), "\22TeaMachine v0.0\n", 17);
	if (0)
		vm.body[0] = (long *)hello;
	assert(inside(hello) >= 0); // ensure body above dict to keep high bit clear

	long chop[] = {_.dup, _.here, _.sub, _.allot, _.exit};
	long countbang[] = {_.dec, _.over, _.cstore, _.exit};
	long inskip[] = {_.zero, _.drop, _.in, _.dup, _.sep, _.unless__, -6, _.exit};
	long inspan[] = {_.here, _.zero, _.bytecom, _.swap, _.bytecom, _.in, _.dup, _.sep, _.unless__, 2, _.jump, -8, _.drop, _.here, _.over, _.sub, _.exit};
	long inword[] = {_.call, inside(inskip), _.call, inside(inspan), _.call, inside(countbang), _.call, inside(chop), _.exit};
	long huh[] = {_.output, _.n, ' ', _.out, _.n, '?', _.out, _.n, '\n', _.out, _.exit};

	if (argc > 1 && !strcmp("tpcb", argv[1])) {
		return tpcb_main(argc, argv);
	}

	if (0) {
		struct header head = {
			.magic = {'t', 'e', 's', 't'},
			.version = 0,
			.blockbits = 14,
			.tablebits = 9,
			.maxtablebits = 19,
			.reshard = 1,
			.rehash = 2,
			.loadfactor = one_fixed8,
			.blocks = 0,

			.upper = {
				.mapbits = 0,
				.stridebits = 23,
				.locbits = 12,
				.sigbits = 50},

			.lower = {}
		};

		int fd = open("foo", O_CREAT|O_RDWR, 0644);
		struct keymap table1{head, fd, fixsize::recops, 8};
		vm.db.push_back((struct teamachine::dbreg){&table1});

		if (0) {
			long test[] = {
				_.cs, 0,
				_.six, _.insert4, 0, 0,
				_.six, _.lookup4, 0, 0,
				_.six, _.remove4, 0, 0,
				_.six, _.lookup4, 0, 0,
				_.bye
			};
			memcpy((char *)(test + 1), "\7foobar!", 8);
			return vm.run(test);
		}

		if (1) {
			dictstar test = vm.inside(vm.here);
			tea.cstring("foobar!");
			tea.op(_.six), tea.op(_.insert4), tea.arg(0), tea.arg(0);
			tea.op(_.six),
			tea.op(_.put4), tea.arg(0), tea.arg(4);
			tea.op(_.seven),
			tea.op(_.add4x), tea.arg(0), tea.arg(4);
		//	tea.op(_.get4), tea.arg(0), tea.arg(4);
			tea.op(_.query);
			tea.op(_.six), tea.op(_.lookup4), tea.arg(0), tea.arg(0);
			tea.op(_.six), tea.op(_.remove4), tea.arg(0), tea.arg(0);
			tea.op(_.six), tea.op(_.lookup4), tea.arg(0), tea.arg(0);
			tea.op(_.bye);
			return vm.run(vm.thread(test));
		}
	}

	if (0) {
		long boot[] = {_.call, inside(hello), _.bye};
		return vm.run(boot);
	}

	if (argc == 1) {
		dictstar teashell = vm.inside(vm.here);
		tea.start_();
		tea.call(inside(hello));
		tea.begin_();
			tea.call(inside(inword));
			tea.op(_.find);
			tea.op(_.maydup);
			tea.if_();
				tea.op(_.dup);
				tea.op(_.zless);
				tea.if_();
					tea.op(_.inc); tea.if_();
						tea.op(_.natex);
					tea.else_();
						tea.op(_.execute);
					tea.end_();
				tea.else_();
					tea.op(_.dec); tea.if_();
						tea.op(_.comcall);
					tea.else_();
						tea.op(_.compile);
					tea.end_();
				tea.end_();
			tea.else_();
				tea.op(_.count);
				tea.op(_.literal);
				tea.if_();
					tea.op(_.running);
					tea.unless_();
						tea.lit(_.n);
						tea.op(_.comma);
						tea.op(_.comma);
					tea.end_();
				tea.else_();
					tea.call(inside(huh));
					tea.op(_.abort);
				tea.end_();
			tea.end_();
		tea.loop_();
		tea.finish_();

		/*
		 * : shell
		 *   begin
		 *     word find ?dup if
		 *       0< running or unless
		 *         dup highbit and unless compile ;word then ,
		 *       else
		 *         execute
		 *       then
		 *     else
		 *       count literal if
		 *         running unless compile ;n , then
		 *       else
		 *         huh abort
		 *       then
		 *     then
		 *   again ;
		 */

		if (0)
			vm.do_fixups(); // necessary only at load/link time

		/* install shell as word zero so abort goes here */
		vm.body[0] = vm.thread(teashell);
		long boot[] = {_.run, /* shell should never exit, but... */ _.bye};
		int excode = 255; // goes back to shell!

		try { excode = vm.run(boot); } catch (std::string &why) { printf("%s!\n", why.c_str()); }

		return excode;
	}

	if (argc == 3) {
		long bench0[] = {_.n, 0, _.times, 2, _.loop, -2, _.bye};
		long bench1[] = {_.n, 0, _.times, 3, _.nop, _.loop, -3, _.bye};
		long bench2[] = {_.n, 0, _.times, 4, _.nop, _.nop, _.loop, -4, _.bye};
		long bench3[] = {_.n, 0, _.times, 5, _.nop, _.nop, _.nop, _.loop, -5, _.bye};
		long bench4[] = {_.n, 0, _.times, 6, _.nop, _.nop, _.nop, _.nop, _.loop, -6, _.bye};
		long bench5[] = {_.n, 0, _.times, 7, _.nop, _.nop, _.nop, _.nop, _.nop, _.loop, -7, _.bye};
		long bench6[] = {_.n, 0, _.times, 8, _.nop, _.nop, _.nop, _.nop, _.nop, _.nop, _.loop, -8, _.bye};
		long bench7[] = {_.n, 0, _.times, 9, _.nop, _.nop, _.nop, _.nop, _.nop, _.nop, _.nop, _.loop, -9, _.bye};
		long bench8[] = {_.n, 0, _.times, 10, _.nop, _.nop, _.nop, _.nop, _.nop, _.nop, _.nop, _.nop, _.loop, -10, _.bye};
		long bench9[] = {_.n, 0, _.times, 11, _.nop, _.nop, _.nop, _.nop, _.nop, _.nop, _.nop, _.nop, _.nop, _.loop, -11, _.bye};

		unsigned which = argc > 1 ? atoi(argv[1]) : 0;
		long *benches[] = {bench0, bench1, bench2, bench3, bench4, bench5, bench6, bench7, bench8, bench9};
		bench0[1] = bench1[1] = bench2[1] = bench3[1] = bench4[1] = bench5[1] = bench6[1] = bench7[1] = bench8[1] = bench9[1] = argc > 2 ? atoi(argv[2]) : 0;
		return which < 10 ? vm.run(benches[which]) : 0;
	}

	return 0;
}

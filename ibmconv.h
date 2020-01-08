#if defined _CRAY
#if defined _CRAYMPP
#define SHORT32
#define INT64
#define FLOAT32
#else
#define SHORT64
#define INT64
#define FLOAT64
#endif
#else
#define SHORT16
#define INT32
#define FLOAT32
#endif

#define DOUBLE64
#if defined _CRAY || defined __alpha || _MIPS_SZLONG == 64 || defined __64BIT__ || defined _LP64 || defined __LP64__ || __WORDSIZE == 64
#define LONG64
#else
#define LONG32
#define LONGLONG64
#endif

#ifdef SHORT16
typedef short int int16;
typedef unsigned short int uint16;
#endif
#ifdef INT32
typedef int int32;
typedef unsigned int uint32;
#endif
#ifdef SHORT32
typedef short int int32;
typedef unsigned short int uint32;
#endif
#ifdef FLOAT32
typedef float float32;
#endif
#ifdef DOUBLE64
typedef double float64;
#endif
#ifdef LONG64
typedef long int int64;
typedef unsigned long int uint64;
#else
typedef long long int int64;
typedef unsigned long long int uint64;
#endif

#define INTEGER int

#if defined __alpha || defined __i386 || defined __x86_64 || defined __ia64__ || defined __LITTLE_ENDIAN__
#define LITTLE__ENDIAN
#else
#define BIG__ENDIAN
#endif

#define I32_NAN    0x7fffffff
#define I32_INFP   0x7f800000
#define I32_INFN   0xff800000
#define I32_ZEROP  0x00000000
#define I32_ZERON  0x80000000

#ifdef LONG64
#define I64_NAN    0x7fffffffffffffffl
#define I64_INFP   0x7ff0000000000000l
#define I64_INFN   0xfff0000000000000l
#else
#define I64_NAN    0x7fffffffffffffffll
#define I64_INFP   0x7ff0000000000000ll
#define I64_INFN   0xfff0000000000000ll
#endif

#if defined _CRAY
#define swapbytes SWAPBYTES
#define ibmi4toi4 IBMI4TOI4
#define ibmi2toi4 IBMI2TOI4
#define ibmr4tor4 IBMR4TOR4
#define ibmi4toi8 IBMI4TOI8
#define ibmi2toi8 IBMI2TOI8
#define ibmr4tor8 IBMR4TOR8
#define ibmr8tor8 IBMR8TOR8
#elif defined __sun || defined __sgi || defined __osf__ || defined __uxpv__ || defined __linux || defined _SX || defined __MINGW32__ || defined __CYGWIN32__
#define swapbytes swapbytes_
#define ibmi4toi4 ibmi4toi4_
#define ibmi2toi4 ibmi2toi4_
#define ibmr4tor4 ibmr4tor4_
#define ibmi4toi8 ibmi4toi8_
#define ibmi2toi8 ibmi2toi8_
#define ibmr4tor8 ibmr4tor8_
#define ibmr8tor8 ibmr8tor8_
#endif

/* Routines callable from C */

void swap_bytes(void *, int, int);
int ibmi4_to_i4(void *, void *, int);
int ibmi2_to_i4(void *, void *, int, int);
int ibmr4_to_r4(void *, void *, int);
int ibmi4_to_i8(void *, void *, int, int);
int ibmi2_to_i8(void *, void *, int, int);
int ibmr4_to_r8(void *, void *, int, int);
int ibmr8_to_r8(void *, void *, int);

/* Routines callable from Fortran */

void swapbytes(void *, INTEGER *, INTEGER *);
void ibmi4toi4 (void *, void *, INTEGER *, INTEGER *);
void ibmi2toi4 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmr4tor4 (void *, void *, INTEGER *, INTEGER *);
void ibmi4toi8 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmi2toi8 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmr4tor8 (void *, void *, INTEGER *, INTEGER *, INTEGER *);
void ibmr8tor8 (void *, void *, INTEGER *, INTEGER *);

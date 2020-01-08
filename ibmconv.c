#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <errno.h>
#include "ibmconv.h"

/*  Routines callable from C */

/* 
 * buf is an array of n objects of size nbytes bytes.
 * reverse order of bytes in buf. buf will be changed by function.
 */

void swap_bytes(void *buf, int nbytes, int n)
{
   int i, j;
   unsigned char temp, *cbuf;

   cbuf = (unsigned char *) buf;

   for (i=0; i<n ; i++)
   {
      for (j=0; j<nbytes/2; j++)
      {
         temp = cbuf[j];
         cbuf[j] = cbuf[nbytes-j-1];
         cbuf[nbytes-j-1] = temp;
      }
      cbuf += nbytes;
   }

   return;
}

/* 
 * Translate IBM 32-bit integers to IEEE 32-bit integers
 *
 */

int ibmi4_to_i4(void *in, void *out, int n)
{
#ifndef _CRAY
  int32 *pin;
  int32 *pout;
  int err, i;

  pin = (int32 *) in;
  pout = (int32 *) out;

  err=0;

  for (i=0; i<n; i++)
     pout[i] = pin[i];

#ifdef LITTLE__ENDIAN
  swap_bytes(pout, 4, n);
#endif

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 2;
  bitoff = 0;
  stride = 1;
  intlen = 32;
  extlen = 32;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 16-bit integers to IEEE 32-bit integers
 *
 */

int ibmi2_to_i4(void *in, void *out, int n, int offset)
{
#ifndef _CRAY
  int16 *pin;
  int32 *pout;
  int err, i;

  err=0;
  pin = (int16 *) in;
  pout = (int32 *) out;

  pin += offset;

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 2, n);
#endif

  for (i=0; i<n; i++)
     pout[i] = pin[i];

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 2, n);
#endif

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 2;
  bitoff = offset*16;
  stride = 1;
  intlen = 32;
  extlen = 16;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 32-bit integers to IEEE 64-bit integers
 *
 */

int ibmi4_to_i8(void *in, void *out, int n, int offset)
{
#ifndef _CRAY
  int32 *pin;
  int64 *pout;
  int err, i;

  err=0;
  pin = (int32 *) in;
  pout = (int64 *) out;

  pin += offset;

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 4, n);
#endif

  for (i=0; i<n; i++)
     pout[i] = pin[i];

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 4, n);
#endif

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 2;
  bitoff = offset*32;
  stride = 1;
  intlen = 64;
  extlen = 32;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 16-bit integers to IEEE 64-bit integers
 *
 */

int ibmi2_to_i8(void *in, void *out, int n, int offset)
{
#ifndef _CRAY
  int16 *pin;
  int64 *pout;
  int err, i;

  err=0;
  pin = (int16 *) in;
  pout = (int64 *) out;

  pin += offset;

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 2, n);
#endif

  for (i=0; i<n; i++)
     pout[i] = pin[i];

#ifdef LITTLE__ENDIAN
  swap_bytes(pin, 2, n);
#endif

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 2;
  bitoff = offset*16;
  stride = 1;
  intlen = 64;
  extlen = 16;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 32-bit reals to IEEE 32-bit reals
 *
 */

int ibmr4_to_r4(void *in, void *out, int n)
{
#ifndef _CRAY
  unsigned char *pin;
  float32 *pout;
  unsigned long man;
  int err, i, exp, sign;
  double d;
  uint32 i32;

  pin = (unsigned char *) in;
  pout = (float32 *) out;

  err=0;

  for (i=0; i<n; i++)
  {
     sign = pin[0] & 0x80;
     exp = pin[0] & 0x7f;
     man = ((unsigned long) pin[1] << 16) | 
           ((unsigned long) pin[2] << 8) | (unsigned long) pin[3];

     d = ldexp((double) man ,4*(exp-64-6));

     if (d > (double) FLT_MAX || errno == ERANGE)
     {
        i32 = (sign ? (int32) I32_INFN : (int32) I32_INFP);
        *pout = *(float32 *) &i32;
        err=1;
     }
     else if (d < (double) FLT_MIN) 
     {
        i32 = (sign ? (int32) I32_ZERON : (int32) I32_ZEROP);
        *pout = *(float32 *) &i32;
     }
     else
        *pout = (sign ? -d : d);

     pout++;
     pin += 4;     
  }

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 3;
  bitoff = 0;
  stride = 1;
  intlen = 32;
  extlen = 32;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 32-bit reals to IEEE 64-bit reals
 *
 */

int ibmr4_to_r8(void *in, void *out, int n, int offset)
{
#ifndef _CRAY
  unsigned char *pin;
  float64 *pout;
  unsigned long man;
  int err, i, exp, sign;
  double d;
  uint64 i64;

  pin = (unsigned char *) in;
  pout = (float64 *) out;

  for (i=0; i<4*offset; i++) pin++;

  err=0;

  for (i=0; i<n; i++)
  {
     sign = pin[0] & 0x80;
     exp = pin[0] & 0x7f;
     man = ((unsigned long) pin[1] << 16) | 
           ((unsigned long) pin[2] << 8) | (unsigned long) pin[3];

     d = ldexp((double) man ,4*(exp-64-6));

     if (d > (double) DBL_MAX || errno == ERANGE)
     {
        i64 = (sign ? (int64) I64_INFN : (int64) I64_INFP);
        *pout = *(float64 *) &i64;
        err=1;
     }
     else
        *pout = (sign ? -d : d);

     pout++;
     pin += 4;     
  }

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 3;
  bitoff = offset*32;
  stride = 1;
  intlen = 64;
  extlen = 32;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/* 
 * Translate IBM 64-bit reals to IEEE 64-bit reals
 *
 */

int ibmr8_to_r8(void *in, void *out, int n)
{
#ifndef _CRAY
  unsigned char *pin;
  float64 *pout;
  unsigned long man;
  int err, i, exp, sign;
  double d;
  uint64 i64;

  pin = (unsigned char *) in;
  pout = (float64 *) out;

  err=0;

  for (i=0; i<n; i++)
  {
     sign = pin[0] & 0x80;
     exp = pin[0] & 0x7f;
     man = ((unsigned long) pin[1] << 16) | 
           ((unsigned long) pin[2] << 8) | (unsigned long) pin[3];
     d = (double) man;
     d = 65536.0*d + (((unsigned long) pin[4] << 8) + (unsigned long) pin[5]);
     d = 65536.0*d + (((unsigned long) pin[6] << 8) + (unsigned long) pin[7]);

     d = ldexp(d ,4*(exp-64-14));

     if (d > (double) DBL_MAX || errno == ERANGE)
     {
        i64 = (sign ? (int64) I64_INFN : (int64) I64_INFP);
        *pout = *(float *) &i64;
        err=1;
     }
     else
        *pout = (sign ? -d : d);

     pout++;
     pin += 8;     
  }

  return err;
#else
  int err, type, bitoff, stride, intlen, extlen;

  type = 3;
  bitoff = 0;
  stride = 1;
  intlen = 64;
  extlen = 64;

  err = IBM2CRI(&type, &n, in, &bitoff, out, &stride, &intlen, &extlen);

  return err;
#endif
}

/*  Routines callable from Fortran */

void swapbytes(void *buf, INTEGER *nbytes, INTEGER *n)
{
   swap_bytes(buf, *nbytes, *n);

   return;
}

void ibmi4toi4 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert IBM 32 bit integers to Ieee 32 bit integers */

   *iret = ibmi4_to_i4(in, out, *n);

   return;
}

void ibmi2toi4 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 16 bit integers to Ieee 32 bit integers */

   *iret = ibmi2_to_i4(in, out, *n, *offset);

   return;
}

void ibmr4tor4 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert IBM 32 bit floats to Ieee 32 bit floats */

   *iret = ibmr4_to_r4(in, out, *n);

   return;
}

void ibmi4toi8 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 32 bit integers to Ieee 64 bit integers */

   *iret = ibmi4_to_i8(in, out, *n, *offset);

   return;
}

void ibmi2toi8 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 16 bit integers to Ieee 64 bit integers */

   *iret = ibmi2_to_i8(in, out, *n, *offset);

   return;
}

void ibmr4tor8 (void *in, void *out, INTEGER *n, INTEGER *offset, INTEGER *iret)
{

   /* Convert IBM 32 bit floats to Ieee 64 bit floats */

   *iret = ibmr4_to_r8(in, out, *n, *offset);

   return;
}

void ibmr8tor8 (void *in, void *out, INTEGER *n, INTEGER *iret)
{

   /* Convert IBM 64 bit floats to Ieee 64 bit floats */

   *iret = ibmr8_to_r8(in, out, *n);

   return;
}

/*---------------------------------------------------------------------------
   Copyright (c) 2022 The pixeldrift programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/fail.h>

#include "vendor/lodepng.h"

#define OCAML_LODEPNG_RAISE_SYS_ERROR(ERR)                      \
  do { caml_raise_sys_error (caml_copy_string(ERR)); }          \
  while (0)

CAMLprim value ocaml_lodepng_decode (value ba)
{
  CAMLparam1(ba);
  CAMLlocal3(size, p, ret);
  unsigned w;
  unsigned h;
  unsigned char *pixels;

  if (lodepng_decode32 (&pixels, &w, &h, Caml_ba_data_val (ba),
                        Caml_ba_array_val (ba)->dim[0]))
  {
    OCAML_LODEPNG_RAISE_SYS_ERROR ("PNG decode error");
  }

  p = caml_ba_alloc_dims (CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_MANAGED,
                          1, pixels, w * h * 4);

  size = caml_alloc_tuple (2);
  Store_field (size, 0, Val_int (w));
  Store_field (size, 1, Val_int (h));

  ret = caml_alloc_tuple (2);
  Store_field (ret, 0, size);
  Store_field (ret, 1, p);

  CAMLreturn (ret);
}

CAMLprim value ocaml_lodepng_encode (value w, value h, value p)
{
  CAMLparam3(w, h, p);
  CAMLlocal1(b);
  unsigned char *out;
  size_t outsize;

  if (lodepng_encode32 (&out, &outsize, Caml_ba_data_val (p),
                        Int_val (w), Int_val (h)))
  {
    OCAML_LODEPNG_RAISE_SYS_ERROR ("PNG encode error");
  }

  b = caml_ba_alloc_dims (CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_MANAGED,
                          1, out, outsize);
  CAMLreturn (b);
}

/*---------------------------------------------------------------------------
   Copyright (c) 2022 The pixeldrift programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/

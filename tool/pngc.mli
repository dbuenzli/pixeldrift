(*---------------------------------------------------------------------------
   Copyright (c) 2022 The pixeldrift programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** PNG image codec.

    {b Note.} No color management is performed. Assuming or providing
    sRGB is the best bet. *)

type bigbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** The type for bigarrays of bytes. *)

type pixels = bigbytes
(** The type for pixel data. Pixels are stored in a linear buffer,
    line-by-line, from left to right and top to bottom in RGBA order
    with one byte per component. *)

type size = int * int
(** The type for image sizes. *)

val decode : bigbytes -> (size * pixels, string) result
(** [decode b] decodes a PNG image from the bytes [b]. *)

val encode : size -> pixels -> (bigbytes, string) result
(** [encode size p] encodes pixels [p] as an RGBA image of the given size.

    @raises Invalid_argument if [size] does not match the length of pixels. *)

(*---------------------------------------------------------------------------
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
  ---------------------------------------------------------------------------*)

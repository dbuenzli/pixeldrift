(*---------------------------------------------------------------------------
   Copyright (c) 2022 The pixeldrift programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type bigbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type pixels = bigbytes
type size = int * int

external _decode : bigbytes -> (int * int) * pixels = "ocaml_lodepng_decode"
external _encode : int -> int -> bigbytes -> bigbytes = "ocaml_lodepng_encode"

let decode b = try Ok (_decode b) with Sys_error e -> Error e
let encode (w, h) p =
  try
    let sc = w * h * 4 and ac = Bigarray.Array1.dim p in
    if sc = ac then Ok (_encode w h p) else
    invalid_arg @@
    Printf.sprintf "pixels size mimatch, expected %d bytes found %d" sc ac
  with
  Sys_error e -> Error e

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

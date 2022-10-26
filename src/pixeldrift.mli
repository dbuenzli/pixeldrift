(*---------------------------------------------------------------------------
   Copyright (c) 2022 The pixeldrift programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Find perceptible pixel differences.

    This module finds the number of perceptually differing pixels
    between two equally sized sRGB images with an alpha component. It
    is mostly suitable for comparing synthetic images: diagrams, user
    interface screenshots, rendering algorithms outputs, etc.

    To take into account the alpha component images are first blended
    against an opaque white background before testing their pixels for
    difference using a perceptual color metric.

    Anti-aliased pixels can be detected and optionally ignored by the
    comparison.

    {b References.} The algorithm is based on the
    {{:https://github.com/mapbox/pixelmatch}pixelmatch} JavaScript
    library. In contrast to the latter it performs alpha blending
    computations in linear space and uses a simplified anti-aliasing
    pixel detection algorithm which, for now, acts more like an edge
    detection filter. The color metric is described in this paper:
    {ul
    {- Yuriy Kotsarenko et al.
    {{:http://riaa.uaem.mx/handle/20.500.12055/91}
      {e Measuring perceived color difference using YIQ NTSC transmission
         color space in mobile applications}}. 2010}} *)

(** {1:pixels Pixels} *)

type uint32 = int32
(** The type for unsigned 32-bit integers. *)

(** Bigarrays of bytes. *)
module Bigbytes : sig
  type t =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    (** The type for bigarrays of bytes. *)

  val create : len:int -> init:int -> t
  (** [create ~len ~init] is a bigarray of bytes of length [len] filled
      with byte [init]. *)

  val length : t -> int
  (** [length b] is the length of [b]. *)

  val get : t -> int -> int
  (** [get b i] is the byte at index [i] of [b]. *)

  val set : t -> int -> int -> unit
  (** [set b i v] sets the byte at index [i] of [b] to [v]. *)

  val get_uint32_be : t -> int -> uint32
  (** [get b i] is the big endian unsigned 32-bit integer at byte index [i]
      of [b]. *)

  val set_uint32_be : t -> int -> uint32 -> unit
  (** [set b i v] sets the big endian unsigned 32-bit integer at byte index [i]
      of [b] to [v]. *)

  val clear : t -> unit
  (** [clear b] zeroes [b]. *)
end

type srgb_pixels = Bigbytes.t
(** The type for pixel data in sRGB space with an alpha component.

    Pixels are stored in a linear buffer, line-by-line, from left to
    right and top to bottom in RGBA order with one byte per
    component. *)

(** {1:map Difference map} *)

type diff_map
(** The type for specifying an image containing a color coded difference map.
    See {!val-diff_map} for details. *)

val diff_map :
  ?luma_alpha:float -> ?diff:uint32 -> ?diff_darken:uint32 ->
  ?aa:uint32 -> srgb_pixels -> diff_map
(** [diff_map ~luma_alpha ~diff ~diff_darken ~aa_color pixels]
    specifies a pixel difference image in [pixels] as follows:
    {ol
    {- First the luma component of the reference image is blended
       against an opaque white background using the image's alpha
       multiplied by [luma_alpha] (defaults to [0.1]). If [luma_alpha] is
       [0.] then no blending occurs and the difference map background is
       a black transparent color.}
    {- Each pixel where a difference is detected is set to [diff]
       (defaults to [0xD7_19_1C_FFl])}
    {- Each pixel where a darkening difference is detected is set to
       [diff_darken] (defaults equal to [diff]).}
    {- Each pixel where an anti-aliasing pixel is detected (if applicable)
       is set to [aa] (defaults to [0xFF_FF_99_FFl]).}} *)

val mask_diff_map :
  ?diff:uint32 -> ?diff_darken:uint32 -> srgb_pixels -> diff_map
(** [mask_diff_map] is [diff_map ~luma_alpha:0 ~aa:0l].
    With these parameters only differing pixels are rendered
    over a black transparent background. *)

val diff_map_pixels : diff_map -> srgb_pixels
(** [diff_map_pixels m] are the pixels of [m]. *)

(** {1:diff Difference} *)

val count :
  ?tol:float -> ignore_aa:bool -> w:int -> h:int -> ref:srgb_pixels ->
  test:srgb_pixels -> diff_map:diff_map option -> unit -> int
(** [count ~tol ~ignore_aa ~color_space ~w ~h ~ref ~test ~diff_map ()] is
    the number of pixels differing when comparing the [w]⨯[h] pixels
    of [ref] to [test] with:
    {ul
    {- [ignore_aa] determines whether anti-aliasing pixels should be detected
       and ignored for the sake of comparison.}
    {- [color_space] is the color space in which {e all} pixels
       are assumed to live in (including the [diff_map] if specified).}
    {- [diff_map] if specified, writes an image of the differences
       as specified by the {!val-diff_map} value.}
    {- [tol] is a matching tolerance the smaller the more sensitive the
       comparison is. Ranges from [0.] to [1.], defaults to [0.1].}}

    Raises [Invalid_argument] if [ref], [test] and [diff_map] do not have
    exactly [w]⨯[h] pixels. *)

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

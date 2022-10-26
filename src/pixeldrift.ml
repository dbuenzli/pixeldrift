(*---------------------------------------------------------------------------
   Copyright (c) 2022 The pixeldrift programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Pixels *)

type uint32 = int32

module Bigbytes = struct
  type t = (int,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t

  let _create len = Bigarray.(Array1.create int8_unsigned c_layout len)
  let create ~len ~init:v = let a = _create len in Bigarray.Array1.fill a v; a
  let length b = Bigarray.Array1.dim b

  let[@inline] get b i = (Bigarray.Array1.get : t -> int -> int) b i
  let[@inline] set b i v = (Bigarray.Array1.set : t -> int -> int -> unit) b i v

  external swap_32 : int32 -> int32 = "%bswap_int32"
  external get_uint32_ne : t -> int -> uint32 = "%caml_bigstring_get32"
  external set_uint32_ne : t -> int -> uint32 -> unit = "%caml_bigstring_set32"

  let[@inline] get_uint32_be b i =
    if Sys.big_endian
    then get_uint32_ne b i
    else swap_32 (get_uint32_ne b i)

  let[@inline] set_uint32_be b i v =
    if Sys.big_endian
    then set_uint32_ne b i v
    else set_uint32_ne b i (swap_32 v)

  let resize b ~len = Bigarray.Array1.sub b 0 len
  let clear b = Bigarray.Array1.fill b 0
end

type srgb_pixels = Bigbytes.t

let chan_of_byte = 1. /. 255.
let[@inline] chan_get_raw i k = float (Bigbytes.get i k) *. chan_of_byte
let[@inline] chan_to_byte v = Float.to_int (Float.round (v *. 255.))
let[@inline] chan_blend_over_white v ~a = (v -. 1.) *. a +. 1.

(* Note we use the convention to prime non-linear RGB variables. *)

module Srgb = struct

  (* sRGB equations from IEC 61966-2-1:1999 *)

  let c0 = 0.04045
  let c1 = 1. /. 12.92
  let c2 = 0.055
  let c3 = 1. /. 1.055
  let c4 = 2.4
  let[@inline] to_linear v' =
    if v' <= c0 then c1 *. v' else (c3 *. (v' +. c2)) ** c4

  let to_linear_lut = Array.init 256 (fun v' -> to_linear (float v' /. 255.))
  let[@inline] chan_get_linear i k = to_linear_lut.(Bigbytes.get i k)

  let c0 = 0.0031308
  let c1 = 12.92
  let c2 = 1.055
  let c3 = 1. /. 2.4
  let c4 = 0.055
  let[@inline] of_linear v =
    if v <= c0 then c1 *. v else c2 *. (v ** c3) -. c4

  (* YIQ as described in Kotsarenko et al. Measuring perceived color
     difference using YIQ NTSC transmission color space in mobile
     applications. 2010. *)

  let[@inline] to_y r' g' b' =
    0.29889531 *. r' +. 0.58662247 *. g' +. 0.11448223 *. b'

  let[@inline] to_i r' g' b' =
    0.59597799 *. r' -. 0.27417610 *. g' -. 0.32180189 *. b'

  let[@inline] to_q r' g' b' =
    0.21147017 *. r' -. 0.52261711 *. g' +. 0.31114694 *. b'

  (* XXX for now we disable the linear conversions, it's a bit unclear
     whether they bring anything for the comparison itself. Review the
     maths. It obviously has an impact for the alpha blending of the
     luma in the diff map but it's just that it looks lighter. *)

  let to_linear = Fun.id
  let[@inline] chan_get_linear i k = chan_get_raw i k
  let of_linear = Fun.id
end

(* Difference map *)

type diff_map =
  { luma_alpha : float; (* The blending factor for the luma component. *)
    diff_lighten : uint32; (* The color for differing lightening pixels. *)
    diff_darken : uint32; (* The color for differing darkening pixels. *)
    aa : uint32; (* The color for antialised pixels. *)
    show_aa : bool; (* Whether the alpha [aa] is not equal to [0]. *)
    pixels : srgb_pixels (* The pixels. *) }

let diff_map_pixels m = m.pixels
let diff_map
    ?(luma_alpha = 0.1) ?(diff = 0xD7_19_1C_FFl) ?(diff_darken = diff)
    ?(aa = 0xFF_FF_99_FFl) pixels
  =
  let show_aa = Int32.(logand aa 0xFFl <> 0l) in
  { luma_alpha; diff_lighten = diff; diff_darken; aa; show_aa; pixels }

let nil_diff_map = diff_map (Bigbytes.create ~len:0 ~init:0)

let mask_diff_map ?diff ?diff_darken pixels =
  diff_map ~luma_alpha:0. ?diff ?diff_darken ~aa:0l pixels

(* Difference *)

let color_delta_signed_square ~ref:r ~test:t ~k =
  let rr = Srgb.chan_get_linear r (k    ) in
  let rg = Srgb.chan_get_linear r (k + 1) in
  let rb = Srgb.chan_get_linear r (k + 2) in
  let ra = chan_get_raw         r (k + 3) in
  let tr = Srgb.chan_get_linear t (k    ) in
  let tg = Srgb.chan_get_linear t (k + 1) in
  let tb = Srgb.chan_get_linear t (k + 2) in
  let ta = chan_get_raw         t (k + 3) in
  if rr = tr && rg = tg && rb = tb && ra = ta then 0. else
  let rr' = Srgb.of_linear (chan_blend_over_white rr ~a:ra) in
  let rg' = Srgb.of_linear (chan_blend_over_white rg ~a:ra) in
  let rb' = Srgb.of_linear (chan_blend_over_white rb ~a:ra) in
  let tr' = Srgb.of_linear (chan_blend_over_white tr ~a:ta) in
  let tg' = Srgb.of_linear (chan_blend_over_white tg ~a:ta) in
  let tb' = Srgb.of_linear (chan_blend_over_white tb ~a:ta) in
  let dy = Srgb.to_y rr' rg' rb' -. Srgb.to_y tr' tg' tb' in
  let di = Srgb.to_i rr' rg' rb' -. Srgb.to_i tr' tg' tb' in
  let dq = Srgb.to_q rr' rg' rb' -. Srgb.to_q tr' tg' tb' in
  (* Kotsarenko et al. equation (35). It's a bit unfortunate no good intuition
     is given about the numbers, it would be nice to give a sensitive
     idea like just-noficeable difference to end-users *)
  let d = (0.5023 *. dy *. dy) +. (0.299 *. di *. di) +. (0.1954 *. dq *. dq) in
  if dy > 0. then -. d (* test darkens *) else d

let is_antialias ~w ~h ~x:xc ~y:yc i =
  let xmin = let m = xc - 1 in if m < 0 then 0 else m in
  let xmax = let m = xc + 1 in if m < w then m else w - 1 in
  let ymin = let m = yc - 1 in if m < 0 then 0 else m in
  let ymax = let m = yc + 1 in if m < h then m else h - 1 in
  let k = (yc * w + xc) * 4 in
  let cr = Srgb.chan_get_linear i (k    ) in
  let cg = Srgb.chan_get_linear i (k + 1) in
  let cb = Srgb.chan_get_linear i (k + 2) in
  let ca = chan_get_raw         i (k + 3) in
  let cr' = Srgb.of_linear (chan_blend_over_white cr ~a:ca) in
  let cg' = Srgb.of_linear (chan_blend_over_white cg ~a:ca) in
  let cb' = Srgb.of_linear (chan_blend_over_white cb ~a:ca) in
  let cy = Srgb.to_y cr' cg' cb' in
  let darker = ref 0 in
  let lighter = ref 0 in
  let eq = ref 0 in
  try
    (* Look at the moore neighborhood. If we have more than two exactly equal
       pixels this is not antialising. Otherwise count the surrounding darker
       or lighter pixels, if there's more than four of these, this is not
       antialiasing. XXX try to count them in adjacency order. *)
    for y = ymin to ymax do
      for x = xmin to xmax do
        if x = xc && y = yc then () else
        let k = (y * w + x) * 4 in
        let r = Srgb.chan_get_linear i (k    ) in
        let g = Srgb.chan_get_linear i (k + 1) in
        let b = Srgb.chan_get_linear i (k + 2) in
        let a = chan_get_raw         i (k + 3) in
        if r = cr && g = cg && b = cb && a = ca
        then (incr eq; if !eq > 2 then raise Exit) else
        let r' = Srgb.of_linear (chan_blend_over_white r ~a:a) in
        let g' = Srgb.of_linear (chan_blend_over_white g ~a:a) in
        let b' = Srgb.of_linear (chan_blend_over_white b ~a:a) in
        let y = Srgb.to_y r' g' b' in
        if cy -. y < 0. then incr lighter else incr darker
      done
    done;
    if !darker > 4 || !lighter > 4 then false else true
  with
  | Exit -> false

let blend_luma ~ref ~diff_map:m ~k =
  if m.luma_alpha = 0. then () else
  let r' = chan_get_raw ref (k    ) in
  let g' = chan_get_raw ref (k + 1) in
  let b' = chan_get_raw ref (k + 2) in
  let a  = chan_get_raw ref (k + 3) in
  let y = Srgb.to_linear (Srgb.to_y r' g' b') in
  let y = Srgb.of_linear (chan_blend_over_white y ~a:(m.luma_alpha *. a)) in
  let y = chan_to_byte y in
  let p = m.pixels in
  Bigbytes.set p (k    ) y;
  Bigbytes.set p (k + 1) y;
  Bigbytes.set p (k + 2) y;
  Bigbytes.set p (k + 3) 0xFF

let check_size kind ~exp ~fnd =
  if exp = fnd then () else
  invalid_arg @@
  Printf.sprintf "%s size mismatch, expected %d bytes found %d." kind exp fnd

let prepare_diff_map ~exp = function
| None -> nil_diff_map
| Some m ->
    check_size "Diff map image" ~exp ~fnd:(Bigbytes.length m.pixels);
    (if m.luma_alpha = 0. then Bigbytes.clear m.pixels); m

let return_equal ~w ~h ~ref ~diff_map =
  if diff_map != nil_diff_map && diff_map.luma_alpha <> 0.
  then (for i = 0 to w * h - 1 do blend_luma ~ref ~diff_map ~k:(i * 4) done);
  0

let count ?(tol = 0.1) ~ignore_aa ~w ~h ~ref ~test ~diff_map () =
  let exp = w * h * 4 in
  check_size "Reference image" ~exp ~fnd:(Bigbytes.length ref);
  check_size "Test image" ~exp ~fnd:(Bigbytes.length test);
  let m = prepare_diff_map ~exp diff_map in
  if ref = test then return_equal ~w ~h ~ref ~diff_map:m else
  let tol = 0.626 *. tol *. tol in
  let count = Stdlib.ref 0 in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let k = (y * w + x) * 4 in
      let d = color_delta_signed_square ~ref ~test ~k in
      if Float.abs d <= tol
      then (if m != nil_diff_map then blend_luma ~ref ~diff_map:m ~k)
      else begin
        if ignore_aa &&
           (is_antialias ~w ~h ~x ~y ref || is_antialias ~w ~h ~x ~y test)
        then begin
          if m == nil_diff_map then () else
          if m.show_aa then Bigbytes.set_uint32_be m.pixels k m.aa else
          blend_luma ~ref ~diff_map:m ~k
        end else begin
          incr count;
          if m == nil_diff_map then () else
          if d < 0.
          then Bigbytes.set_uint32_be m.pixels k m.diff_darken
          else Bigbytes.set_uint32_be m.pixels k m.diff_lighten
        end
      end
    done;
  done;
  !count

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

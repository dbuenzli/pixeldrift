(*---------------------------------------------------------------------------
   Copyright (c) 2022 The pixeldrift programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Bigfile = struct
  type fpath = string

  type bigbytes =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let map_bytes ?(len = -1) ~write fd =
    let t = Bigarray.int8_unsigned and l = Bigarray.C_layout in
    Bigarray.array1_of_genarray (Unix.map_file fd t l write [|len|])

  let error f e = Error (Printf.sprintf "%s: %s" f (Unix.error_message e))
  let read file =
    try
      let fd = Unix.openfile file Unix.[O_RDONLY] 0x600 in
      let finally () = try Unix.close fd with Unix.Unix_error _ -> () in
      Fun.protect ~finally @@ fun () -> Ok (map_bytes ~write:false fd)
    with
    | Unix.Unix_error (e, _, _) -> error file e

  let write file bytes =
    try
      let fd = Unix.openfile file Unix.[O_CREAT; O_RDWR; O_TRUNC] 0o644 in
      let finally () = try Unix.close fd with Unix.Unix_error _ -> () in
      Fun.protect ~finally @@ fun () ->
      let dst = map_bytes ~len:(Bigarray.Array1.dim bytes) ~write:true fd in
      Ok (Bigarray.Array1.blit bytes dst)
    with
    | Unix.Unix_error (e, _, _) -> error file e
end

let ( let* ) = Result.bind
let exec = Filename.basename Sys.executable_name
let log fmt = Format.printf ("@[" ^^ fmt ^^ "@]@.")
let log_err fmt = Format.eprintf ("%s: @[" ^^ fmt ^^ "@]@.") exec
let log_if_error ~use = function Ok v -> v | Error e -> log_err "%s" e; use

let err_size = 1
let err_failure = 2

let load_rgba_png file =
  let* bytes = Bigfile.read file in
  Result.map_error (Printf.sprintf "%s: %s" file) @@
  Pngc.decode bytes

let write_diff_map ~w ~h outfile map = match outfile with
| None -> Ok ()
| Some file ->
    let pixels = Pixeldrift.diff_map_pixels (Option.get map) in
    let* png = Pngc.encode (w, h) pixels in
    Bigfile.write file png

let output_diff_ratio ~quiet ~pixel_ratio diff_count ~total =
  if quiet then () else
  if pixel_ratio
  then log "%d/%d" diff_count total
  else log "%.3g%%" ((float diff_count /. (float total) *. 100.))

let pixeldrift
    ref test outfile pixel_ratio ignore_aa luma_alpha tol fail contrast quiet
  =
  log_if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* (w, h), ref_bytes = load_rgba_png ref in
  let* (tw, th), test_bytes = load_rgba_png test in
  if w <> tw || h <> th then begin
    log_err "@[<v>Image size mismatch:@,%s: %d⨯%d@,%s: %d⨯%d@]"
      ref w h test tw th;
    Ok err_size
  end else begin
    let total = w * h in
    let diff_map = match outfile with
    | None -> None
    | Some _ ->
        let pixels = Pixeldrift.Bigbytes.create ~len:(total * 4) ~init:0 in
        let diff_darken = if contrast then Some 0x1A_96_41_FFl else None in
        let aa = if luma_alpha = 0. then Some 0l else None in
        Some (Pixeldrift.diff_map ?diff_darken ?aa ~luma_alpha pixels)
    in
    let diff_count = Pixeldrift.count
        ~tol ~ignore_aa ~w ~h ~ref:ref_bytes ~test:test_bytes ~diff_map ()
    in
    output_diff_ratio ~quiet ~pixel_ratio diff_count ~total;
    let* () = write_diff_map ~w ~h outfile diff_map in
    match fail with
    | None -> Ok 0
    | Some f when pixel_ratio ->
        if diff_count >= truncate f then Ok err_failure else Ok 0
    | Some f ->
        let pct = (float diff_count /. (float total)) *. 100. in
        if pct >= f then Ok err_failure else Ok 0
  end

open Cmdliner

let exits =
  Cmd.Exit.info err_size ~doc:"on image size mismatch." ::
  Cmd.Exit.info err_failure
    ~doc:"on difference failure (see option $(b,--fail))." ::
  Cmd.Exit.defaults

let reference =
  let doc = "Reference image file." and docv = "REF.png" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

let test =
  let doc = "Test image file." and docv = "TEST.png" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv)

let outfile =
  let doc = "Output file for the difference map image. Not generated \
             if unspecified."
  in
  let docv = "DIFF.png" in
  Arg.(value & pos 2 (some string) None & info [] ~doc ~docv)

let ratio =
  let doc = "Output the pixel ratio rather than the percentage of differing \
             pixels."
  in
  Arg.(value & flag & info ["p"; "pixel-ratio"] ~doc)

let ignore_aa =
  let doc = "Detect and do not take into account anti-aliased pixels. Unless \
             $(b,--luma-alpha) is 0, these pixels are shown in yellow in \
             the difference map image."
  in
  Arg.(value & flag & info ["i"; "ignore-aa"] ~doc)

let luma_alpha =
  let doc = "$(docv) is the alpha value used to blend the luma channel \
             of the reference image in the difference map image. \
             Use 0 to suppress it and create an output mask of the differences."
  in
  Arg.(value & opt float 0.1 & info ["luma-alpha"] ~doc ~docv:"ALPHA")

let contrast =
  let doc =
    "Differentiate between regions becoming darker (green) and lighter \
     (red) in the test image. For dark on light text, respectively represents
     additions and deletions. If unspecified all differing pixels are in red."
  in
  Arg.(value & flag & info ["c"; "contrast"] ~doc)

let tol =
  let doc = "Tolerance for the color difference. The smaller the value \
             the more sensitive the comparison is."
  in
  Arg.(value & opt float 0.1 & info ["t"; "tol"] ~doc ~docv:"TOL")

let failure =
  let doc = "Failure threshold. Percentage or number of different pixels
            (if $(b,--pixel-ratio) is specified) necessary to make the \
             program exit with code 2. If unspecified, never does."
  in
  Arg.(value & opt (some float) None & info ["f"; "fail"] ~doc ~docv:"NUM")

let quiet =
  let doc = "Be quiet, do not output difference ratio." in
  Arg.(value & flag & info ["q"; "quiet"] ~doc)

let cmd =
  let doc = "Find perceptible pixel differences" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) finds perceptible pixel differences between two equally \
        sized sRGB images with an alpha component. It is mostly suitable \
        for comparing synthetic images: diagrams, user interface screenshots, \
        rendering algorithms outputs, etc.";
    `P "The tool reports the percentage or pixel ratio \
        (option $(b,--pixel-ratio)) of differing pixels on $(b,stdout). \
        An exit failure threshold can be specified with the option \
        $(b,--fail).";
    `P "Anti-aliasing pixels can be detected and ignored for \
        the comparison by using the $(b,--ignore-aa) option.";
    `P "If an output file $(i,DIFF.png) is specified, a color coded map of \
        the differences is written to it. The luma of the reference image
        is lightly blended on it (see option $(b,--luma-alpha)) and differing \
        pixels are set to red. Darkening pixels can be set to green \
        with option $(b,--contrast). When option $(b,--ignore-aa) is used \
        differing anti-aliasing pixels are set to yellow.";
    `P "The tool does not perform color management, the pixels in the PNG
        images are converted to 32-bit RGBA and assumed to be in sRGB space.";
    `S Manpage.s_bugs;
    `P "This program is distributed with the pixeldrift OCaml library. \
        See $(i,https://erratique.ch/software/pixeldrift) for contact \
        information.";]
  in
  Cmd.v
    (Cmd.info "pixeldrift" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man)
    Term.(const pixeldrift $ reference $ test $ outfile $ ratio $ ignore_aa $
          luma_alpha $ tol $ failure $ contrast $ quiet)

let () = if !Sys.interactive then () else exit (Cmd.eval' cmd)

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

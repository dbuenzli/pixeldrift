#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "pixeldrift" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/pixeldrift.mllib";
       Pkg.bin ~cond:cmdliner "tool/tool" ~dst:"pixeldrift";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld"; ]

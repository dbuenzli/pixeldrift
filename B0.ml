open B0_kit.V000

(* OCaml library names *)

(* FIXME this is needed for now because of
   https://github.com/ocaml/ocaml/issues/10833 *)
let bigarray = B0_ocaml.libname "bigarray"

let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"
let pixeldrift = B0_ocaml.libname "pixeldrift"

(* Libraries *)

let mod_srcs m =
  let mli = Fmt.str "src/%s.mli" m and ml = Fmt.str "src/%s.ml" m in
  Fpath.[ `File (v mli); `File (v ml) ]

let pixeldrift_lib =
  let srcs = Fpath.[`Dir (v "src")] in
  B0_ocaml.lib ~name:"pixeldrift-lib" pixeldrift ~doc:"pixeldrift library" ~srcs

(* Tool *)

let pixeldirft_tool =
  let srcs = Fpath.[`Dir_rec (v "tool")] in
  let requires = [bigarray; unix; cmdliner; pixeldrift] in
  B0_ocaml.exe "pixeldrift" ~doc:"pixeldrift tool" ~srcs ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The pixeldrift programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/pixeldrift"
    |> add online_doc "https://erratique.ch/software/pixeldrift/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/pixeldrift.git"
    |> add issues "https://github.com/dbuenzli/pixeldrift/issues"
    |> add description_tags
      ["graphics"; "color"; "testing";"org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-cmdliner" "%{cmdliner:installed}%"]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depopts ["cmdliner", ""]
    |> add B0_opam.Meta.conflicts [ "cmdliner", {|< "1.1.0"|}]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
  in
  B0_pack.v "default" ~doc:"pixeldrift package" ~meta ~locked:true @@
  B0_unit.list ()

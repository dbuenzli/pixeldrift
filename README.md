pixeldrift — Find perceptible pixel differences with OCaml
==========================================================
%%VERSION%%

Pixeldrift is an OCaml module to find perceptible pixel differences
between two equally sized sRGB images with an alpha component. It is
mostly suitable for comparing synthetic images: diagrams, user
interface screenshots, rendering algorithms outputs, etc.

Pixeldrift has no dependencies. It is distributed under the ISC
license. The command line tool depends on [cmdliner].

[cmdliner]: https://erratique.ch/software/cmdliner

# Installation

Pixeldrift can be installed with `opam`:

    opam install pixeldrift
    opam install cmdliner pixeldrift  # For the command line tool

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Documentation

The documentation can be consulted [online][doc] or via `odig doc pixeldrift`.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

For the command line tool consult `pixeldrift --help`.

[doc]: https://erratique.ch/software/pixeldrift/doc
[ocaml-forum]: https://discuss.ocaml.org/

# Acknowledgments

* Pixeldrift is based on the JavaScript [pixelmatch] library. See the 
  module documentation for more information.
* The perceptual color metric is defined in Kotsareno et al. 
  [Measuring perceived color difference using YIQ NTSC transmission color
  space in mobile applications][metric]. 2010.
* The vendored C PNG codec used by the command line tool is courtesy of 
  [LodePNG].

[pixelmatch]: https://github.com/mapbox/pixelmatch
[metric]: http://riaa.uaem.mx/handle/20.500.12055/91
[LodePNG]: https://lodev.org/lodepng/






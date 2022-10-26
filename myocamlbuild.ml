open Ocamlbuild_plugin
let () =
  dispatch begin function
  | After_rules ->
      dep ["c"; "compile"] ["tool/vendor/lodepng.h"];
      pdep ["link"] "linkit" (fun param -> [param]);
  | _ -> ()
  end

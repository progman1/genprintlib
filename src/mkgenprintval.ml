let _=
  let v=Sys.ocaml_version in
  if v < "4.03.0" then(
    Unix.rename "genprintval.ml-2" "genprintval.ml";
    Unix.rename "genprintval.mli-2" "genprintval.mli"
  )else
  if v < "4.04.0" then(
    Unix.rename "genprintval.ml-3" "genprintval.ml";
    Unix.rename "genprintval.mli-3" "genprintval.mli"
  )else
  if v < "4.05.0" then(
    Unix.rename "genprintval.ml-4" "genprintval.ml";
    Unix.rename "genprintval.mli-4" "genprintval.mli"
  )else
  if v < "4.06.0" then(
    Unix.rename "genprintval.ml-5" "genprintval.ml";
    Unix.rename "genprintval.mli-5" "genprintval.mli"
  )else
  if v < "4.07.0" then(
    Unix.rename "genprintval.ml-6" "genprintval.ml";
    Unix.rename "genprintval.mli-6" "genprintval.mli"
  )else
  if v < "4.08.0" then(
    Unix.rename "genprintval.ml-7" "genprintval.ml";
    Unix.rename "genprintval.mli-7" "genprintval.mli"
  )else(
    Unix.rename "genprintval.ml-8" "genprintval.ml";
    Unix.rename "genprintval.mli-8" "genprintval.mli"
  )

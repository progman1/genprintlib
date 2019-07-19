let _=
  let v=Sys.ocaml_version in
  if v < "4.08.0" then(
    Unix.rename "genprintval.ml-epoc1" "genprintval.ml";
    Unix.rename "genprintval.mli-epoc1" "genprintval.mli"
  )else(
    Unix.rename "genprintval.ml-epoc2" "genprintval.ml";
    Unix.rename "genprintval.mli-epoc2" "genprintval.mli"
  )

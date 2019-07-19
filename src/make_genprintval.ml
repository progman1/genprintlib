let () =
  let v = Sys.ocaml_version in
  if v < "4.08.0" then begin
    Unix.rename "genprintval.ml-epoc1" "genprintval.ml";
    Unix.rename "genprintval.mli-epoc1" "genprintval.mli";
  end else begin
    Unix.rename "genprintval.ml-epoc2" "genprintval.ml";
    Unix.rename "genprintval.mli-epoc2" "genprintval.mli";
  end

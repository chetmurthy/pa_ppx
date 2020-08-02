
open Pa_ppx_testutils
open Papr_util

module NormLocations = struct
  let loc = Ploc.dummy
  let str_item = (Reloc.str_item (fun _ -> loc) 0)
  let sig_item = (Reloc.sig_item (fun _ -> loc) 0)
  let structure sil = List.map (fun (si, loc) -> (str_item si, loc)) sil
  let signature sil = List.map (fun (si, loc) -> (sig_item si, loc)) sil
end

let official_implem_file f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Official.Implem.pa
  |> Official.Implem.pr |> print_string

let pappx_implem_file f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> PAPR.Implem.pa1 ~input_file:f
  |> NormLocations.structure |> List.map fst |> Ast2pt.implem f
  |> Official.Implem.pr |> print_string

let official_implem_string s =
  s |> Official.Implem.pa
  |> Official.Implem.pr |> print_string

let pappx_implem_string s =
  let f = "file://stashed" in
  Pa_ppx_dock.Pa_dock.stashed_file_contents := s ;
  s |> PAPR.Implem.pa1 ~input_file:f
  |> NormLocations.structure |> List.map fst |> Ast2pt.implem f
  |> Official.Implem.pr |> print_string

let official_interf_file f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Official.Interf.pa
  |> Official.Interf.pr |> print_string

let pappx_interf_file f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> PAPR.Interf.pa1 ~input_file:f
  |> NormLocations.signature |> List.map fst |> Ast2pt.interf f
  |> Official.Interf.pr |> print_string

let official_interf_string s =
  s |> Official.Interf.pa
  |> Official.Interf.pr |> print_string

let pappx_interf_string s =
  let f = "file://stashed" in
  Pa_ppx_dock.Pa_dock.stashed_file_contents := s ;
  s |> PAPR.Interf.pa1 ~input_file:f
  |> NormLocations.signature |> List.map fst |> Ast2pt.interf f
  |> Official.Interf.pr |> print_string

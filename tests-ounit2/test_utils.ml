(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open OUnitTest
open OUnit2
open Pa_ppx_utils
open Std
open Std2
open Coll

let std2_tests = "std2 tests" >:::
  [
    "hash_union0" >::
    (fun ctxt ->
       assert_equal (hash_union ["a"] ["a"]) ["a"]
    )
  ; "hash_union-stable-1" >::
    (fun ctxt ->
       assert_equal (hash_union ["a"; "b"] ["a"; "c"]) ["a"; "b"; "c"]
    )
  ]

let tsort_tests = "tsort tests" >:::
  [
    "simple" >::
    (fun ctxt ->
       let adj = ["a", ["b"]; "b", ["c"]] in
       let sorted = Tsort.tsort  (fun v l -> v::l) adj [] in
       assert_equal sorted ["c"; "b"; "a"] 
    )
  ; "simple2" >::
    (fun ctxt ->
       let adj = ["a", []; "b", []] in
       let sorted = Tsort.tsort  (fun v l -> v::l) adj [] in
       assert_equal sorted ["b"; "a"] 
    )
  ; "merge_adj" >::
    (fun ctxt ->
       let adj1 = ["a", ["b"; "c"]; "b", []; "e", []] in
       let adj2 = ["a", ["d"]; "b", ["c"]] in
       assert_equal (Tsort.merge_adj adj1 adj2)  ["a", ["b"; "c";"d"]; "b", ["c"]; "e", []; "c", []; "d", []]
    )
  ; "diamond" >::
    (fun ctxt ->
       let adj = ["a", ["b"; "c"]; "b", ["d"]; "c", ["d"]] in
       let sorted = Tsort.tsort  (fun v l -> v::l) adj [] in
       assert_equal sorted ["d"; "b"; "c"; "a"] 
    )
  ]

(* Run the tests in test suite *)
let _ =
if invoked_with "test_utils" then begin
  run_test_tt_main ("all_tests" >::: [
        tsort_tests ;
    ])
end
;;

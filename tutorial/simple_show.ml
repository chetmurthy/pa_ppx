
type a1 = int * int [@@deriving show]

let _ =
  print_string ([%show: a1] (5,6))

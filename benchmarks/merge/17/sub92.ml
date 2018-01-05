let rec merge (a, b) =
  match a with
  | [] -> b
  | hd1 :: tl1 ->
      (match b with
      | [] -> a
      | hd2 :: tl2 ->
          (if hd1 > hd2
          then hd1 :: merge (tl1, b)
          else hd2 :: merge (a, tl2)))
(*
let rec print_list l =
  match l with
  |[] -> print_newline()
  |hd::tl -> print_int hd; print_string " " ; print_list tl

let test1 = merge ([4; 3; 1], [5; 2])

let _ = print_list test1

let a11 = merge ([7; 2; 1], [5; 4; 3])
let a12 = merge ([], [])
let a13 = merge ([9; 2], [])
let a14 = merge ([], [7; 3])
let a15 = merge ([5; 4; 3], [5; 4; 3])
let a16 = merge ([5; 3; 1], [8; 6; 4; 2; 0])
let _ = print_list a11; print_list a12; print_list a13; print_list a14;
print_list a15; print_list a16
*)

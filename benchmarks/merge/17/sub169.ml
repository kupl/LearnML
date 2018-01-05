(*
 * 2017 - 09 - 11
 * PL Homework 1-2
 * Joonmo Yang
*)

let rec merge (x, y) =
  match x, y with
  | [], [] -> []
  | [], y -> y
  | x, [] -> x
  | a::lst1, b::lst2 -> 
    if a > b then a::merge(lst1,y)
    else b::merge(x,lst2)

(* tests
let lst1 = [10; 8; 5; 2]
let lst2 = [9; 8; 4; 2]
let lst3 = merge (lst1, lst2)

let _ = List.iter print_int lst3
*)

(* 2014 - 18474 kim ju hyeon  *)
(* 2017 fall PL 1_1 *)
let rec merge : int list * int list -> int list = function
  | list, []
  | [], list -> list
  | h1::t1, h2::t2 ->
    if h1 > h2 then
      h1 :: merge (t1, h2::t2)
    else
      h2 :: merge (h1::t1, t2)
       
(*
let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l
            
let x = merge ([101;77;1], [5;3;2])
let _ = print_list x
 *)
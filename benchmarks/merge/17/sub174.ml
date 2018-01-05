let rec merge : int list * int list -> int list = fun (l1, l2) ->
  match l1, l2 with
  | _, ([]: int list) -> l1
  | ([]: int list), _ -> l2
  | h1 :: t1, h2 :: t2 ->
    if h1 > h2 then h1 :: (merge (t1,l2)) else h2 :: (merge (l1,t2))

(* TESTING FIELD BELOW *)

let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let l = merge ([5;3;2;1],[6;4;0])

let _ = print_list l

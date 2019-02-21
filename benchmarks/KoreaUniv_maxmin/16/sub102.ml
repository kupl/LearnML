(*********************)
(*     Problem 1     *)
(*********************)
let rec num_max a b =
if a>b then a
else b

let rec max : int list -> int
=fun lst -> match lst with
| [] -> raise (Failure "list is too short")
| [x] -> x
| x :: tl -> num_max x (max tl)


let rec num_min a b =
if a<b then a
else b

let rec min : int list -> int
= fun lst -> match lst with
| [] -> raise (Failure "list is too short")
| [x] -> x
| x :: tl -> num_min x (min tl)

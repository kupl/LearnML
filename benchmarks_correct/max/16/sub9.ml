let rec fold f lst base
= match lst with
| [] -> base
| [i] -> i
| hd::tl -> f hd (fold f tl base)

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> fold (fun a b -> if a > b then a else b) lst 0
 
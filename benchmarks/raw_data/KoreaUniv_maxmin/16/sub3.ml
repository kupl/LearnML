(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l =
  match l with
    | [] -> raise(Failure "list size is 0")
    | h::[] -> h
    | h::t -> f h (fold f t);;

let cmp_b a b =
  if a>=b then a
  else b;;
 
let rec max l = fold (cmp_b) l;;
 
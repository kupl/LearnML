(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a=b then f a else (f a)*product f (a+1) b

(* problem 5*)

let rec dfact : int -> int
= fun n -> match n with
 | 1 -> 1
 | 2 -> 2
 | _ -> (product (fun x->x) 1 n) / dfact (n-1)
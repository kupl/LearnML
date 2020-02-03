let rec fold f l a =
	match l with
	[] -> a
	| hd::tl -> f hd (fold f tl a)

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> fold (fun n1 n2 -> if n1 > n2 then n1 else n2) lst min_int (* TODO *)
 
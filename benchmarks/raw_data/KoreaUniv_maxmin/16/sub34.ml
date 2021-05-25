(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	begin 
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a)
	end;;

let rec max : int list -> int
= fun lst -> fold (fun x y -> if x>y then x else y) lst 0;;  (* TODO *)
 
(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 0 (* TODO *)

let rec fold f l a=
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a);;

let sum l = fold (+) l 0;;
let prod l = fold (fun x y -> x * y)l 1;;

let max lst = fold(fun x y -> if(x>=y) then x else y) lst 0;;

let rec min : int list -> int
= fun lst -> 0 (* TODO *)

let min lst = fold(fun x y -> if(x<=y) then x else y) lst (max lst);;

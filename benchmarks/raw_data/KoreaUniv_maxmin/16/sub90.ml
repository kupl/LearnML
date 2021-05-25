(*********************)
(*     Problem 1     *)
(*********************)

let rec max : int list -> int
= fun lst -> let rec fold f l = match l with | a::[]->a | hd::tl -> f hd (fold f tl) in fold(fun x y -> if x>=y then x else y) lst;;
 
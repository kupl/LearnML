(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l =
match l with
[a] -> a 
|[a; b] -> f a b
|hd::tl -> fold f (hd::fold f tl::[]);; 

let rec max : int list -> int
= fun lst -> fold (fun x y -> if x>y then x else y) lst;; (* TODO *)

let rec min : int list -> int
= fun lst -> fold (fun x y -> if x<y then x else y) lst;; (* TODO *)
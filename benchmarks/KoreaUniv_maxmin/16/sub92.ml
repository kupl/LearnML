(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a = match l with 
|[]->a
|hd::tl-> f hd (fold f tl a);;


let rec max : int list -> int
= fun lst ->match lst with
	|[] -> 0
	|hd::tl ->  fold (fun x y -> if x>y then x else if x=y then x else y ) lst hd;; 

let rec min : int list -> int
= fun lst ->match lst with
|[]->0
|hd::tl->  fold (fun x y -> if x<y then x else if x=y then x else y ) lst hd;; 

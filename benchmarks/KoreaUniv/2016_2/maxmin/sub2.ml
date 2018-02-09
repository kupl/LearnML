(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> let a = match lst with
|[]->raise (Failure "list should not be empty")
|hd::tl->hd
in let rec fold f l a = match l with
|[]->a
|hd::tl-> f hd (fold f tl a) in
fold (fun x y -> if x>y then x else y) lst a;;

let rec min : int list -> int
= fun lst -> let a = match lst with
|[]->raise (Failure "list should not be empty")
|hd::tl-> hd 
in let rec fold f l a = match l with
|[]->a
|hd::tl -> f hd (fold f tl a) in
fold (fun x y -> if x<y then x else y) lst a;;

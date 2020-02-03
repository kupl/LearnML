(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
match l with
|[]->a
|hd::tl -> f hd (fold f tl a)

let rec last l = 
match l with
|[a]->a
|_::tl->last tl
|[]->0

let rec max : int list -> int
= fun lst -> fold (fun x y ->if x>y then x else y) lst (last lst)
 
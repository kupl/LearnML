(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> let h =
match lst with 
|[]->0
|hd::tl -> hd in
let rec fold f l b =
match l with 
|[]->h
|hd::tl -> f hd (fold f tl h) in
fold(fun x y -> if(x>y) then x else y) lst h;;
 
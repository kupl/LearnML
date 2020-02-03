(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l=
match l with
|[] -> 0
|hd :: [] -> hd
|hd :: tl -> f hd (fold f tl);;

let rec max : int list -> int
= fun lst -> fold(fun x y -> if(x>y) then x else y) lst;;
 
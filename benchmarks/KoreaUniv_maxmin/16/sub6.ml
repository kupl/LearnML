(*********************)
(*     Problem 1     *)
(*********************)
exception Empty_list

let rec fold f l a =
match l with 
|[] -> a
|hd::tl -> f hd (fold f tl a)

let rec fold1 f l = 
match l with 
|[] -> raise Empty_list
|hd::tl -> fold f tl hd

let rec max : int list -> int
= fun lst -> fold1 (fun x y -> if x > y then x else y) lst 

let rec min : int list -> int
= fun lst -> fold1 (fun x y -> if x < y then x else y) lst 

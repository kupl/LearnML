(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->  match lst with 
 | []->0
 | h::t -> let rec fun2 : int list -> int -> int
 = fun lst1 a -> match lst1 with 
 | []-> a
 | hd::tl -> if hd>a then fun2 tl hd else fun2 tl a in fun2 t h
 
let rec min : int list -> int
= fun lst -> match lst with 
 | []->0
 | h::t -> let rec fun2 : int list -> int -> int
 = fun lst1 a -> match lst1 with 
 | []-> a
 | hd::tl -> if hd<a then fun2 tl hd else fun2 tl a in fun2 t h

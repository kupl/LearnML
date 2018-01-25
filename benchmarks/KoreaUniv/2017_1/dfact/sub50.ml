let is_even x = x mod 2 = 0;;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
if b - a = 0 then f a
else (f a)*(product f (a+1) b);;

(* problem 5*)
let even x =
if is_even x then x
else 1;;

let odd x =
if is_even x then 1
else x;;

let dfact : int -> int
= fun n -> 
if n = 0 then 1
else if is_even n then product even 1 n
else product odd 1 n;;
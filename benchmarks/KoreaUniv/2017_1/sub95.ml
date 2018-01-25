(*     2013210046 조형준    *)

 (* problem 1*) 
let rec fastexpt : int -> int -> int 
= fun b n -> 
if n = 0 then 1
else if n mod 2 = 0 then fastexpt b (n/2) * fastexpt b (n/2) 
else if n mod 2 = 1 then b * fastexpt b (n-1)   
else if n = 1 then b

(*TODO*) 

(* problem 2*) 
let rec smallest_divisor : int -> int 
= fun n -> 
(*TODO*) 
  
(* problem 3*) 

let rec iter : int * (int -> int) -> (int -> int) 
= fun (n,f) -> 
(*TODO*) 
  
(* problem 4*) 
let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if b > a then (product f a (b-1)) * f (b)  
else f a

 
(*TODO*) 
  
(* problem 5*) 
let rec dfact : int -> int 
= fun n ->
if n < 2 then 1
else n * (dfact n-2)
(*TODO*) 
  
(* problem 6*) 
  
let rec drop : 'a list -> int -> 'a list 
= fun l n -> 
match l with
|[] -> []
|hd::tl -> if n>1 then drop tl (n-1) else tl

(*TODO*) 
(* problem 7*) 
let unzip : ('a * 'b) list -> 'a list * 'b list 
= fun lst ->
List.split lst

(*TODO*) 
(* problem 8*) 

let rec change : int list -> int -> int 
= fun coins amount ->  

(*TODO*)  
(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->
        if y = 0 then 1
        else if x=y then 1
        else pascal (x-1,y-1) + pascal(x-1,y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
        if a = b then f a
        else f a + sigma f (a+1) b
        
        
(* Problem 3 *)        
let rec min : int list -> int
=fun l -> match l with
|[] -> raise (Failure "error")
| [a] -> a
| a::b::tl -> if a< b then min (a::tl) 
else min (b::tl)

let rec max : int list -> int
= fun l -> match l with
|[] -> raise (Failure "error")
| [a] -> a
| a::b::tl -> if a>b then max (a::tl) else max (b::tl)

(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> match f with
|True -> true
|False -> false
| Neg f -> if (eval f) = true then false else true
| Or(f1,f2) -> (eval f1) || (eval f2)
| And(f1,f2) ->( eval f1) && (eval f2)
| Imply(f1,f2) -> 
                (if (eval f1) = true then false else true)  ||eval f2
|Equiv (f1,f2) ->
((if (eval f1) = true then false else true)  ||eval f2) &&
((if (eval f2) = true then false else true)  ||eval f1)





(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
        match n1 with
        | ZERO -> n2
        | SUCC(tl) -> SUCC(natadd tl n2)




let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 

        match n2 with
        | ZERO -> n2
        | SUCC(tl) -> natadd n1 (natmul n1 tl) 









(* Problem 1 *)let rec pascal : int * int -> int=fun (x,y) -> 
if x=y || y=0 then 1 else pascal(x-1,y-1) + pascal(x-1,y);;
(* Problem 2 *)let rec sigma : (int -> int) -> int -> int -> int=fun f a b -> 
if a>b then 0 else (f a) + (sigma f (a+1) b);;
(* Problem 3 *)let rec max : int list -> int=fun l ->
match l with | [] -> 0 
| hd::tl -> if tl = [] then hd else let big = max tl in if hd>big then hd else big;;
let rec min : int list -> int=fun l -> 
match l with | [] -> 0 
| hd::tl -> if tl = [] then hd else let small = min tl in if hd>small then small else hd;;
(* Problem 4 *)type formula = 
True | False  | Neg of formula  | Or of formula * formula  
| And of formula * formula  | Imply of formula * formula
| Equiv of formula * formula
let rec eval : formula -> bool=fun f -> match f with 
 True -> true 
|False -> false
|Neg x -> if x = True then eval False else eval True
|Or (x, y) -> if x = True then eval True else if y = True then eval True else eval False 
|And (x, y) -> if x = True && y = True then eval True else eval False 
|Imply (x, y) -> if x = True && y = True then eval True else if x = False && y = False then eval True else eval False 
|Equiv (x, y) -> if x = y then eval True else eval False;;
(* Problem 5 *)type nat = ZERO | SUCC of nat 
let rec natadd : nat -> nat -> nat=fun n1 n2 -> 
match n2 with
ZERO -> n1
|SUCC n2 -> natadd (SUCC n1) n2;; 
let rec natmul : nat -> nat -> nat=fun n1 n2 -> 
match n2 with
ZERO -> ZERO
| SUCC y -> natadd (natmul n1 y) n1;;
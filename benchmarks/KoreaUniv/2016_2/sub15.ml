(*********************)
(*     Problem 1     *)
(*********************)

let rec max  : int list -> int
= fun lst -> 
    match lst with
    |[] -> 0
    |hd::tl -> if hd > (max tl) then hd else (max tl) ;;

(* TODO *)

max [1;2;3;4;5] ;;

let rec min : int list -> int
= fun lst ->  
    match lst with
    |[] -> raise(Failure "No empty list can be accepted")
    |[a] -> a 
    |hd::tl -> if hd < (min tl) then hd else (min tl);; 
(* TODO *)

min [2;3;1;5;4] ;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst  
  =  match lst with
    |[] -> raise(Failure "NO item")
    |[a] -> if pred a then [a] else [] 
    |hd::tl -> if  pred hd then hd :: filter pred tl else filter pred tl;;

(* TODO *)
filter (fun x-> x mod 2 = 0) [1;2;3;4;5;];;
filter (fun x-> x>0) [5;-1;9;2;-9] ;;
filter (fun x-> x*x> 25) [1;2;3;4;5;6;7;8] ;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =  
    f (f a) ;;   

(* TODO *)

let inc x = x+1 ;;
let mul x = x*2 ;;

(double inc) 1;;
((double double) inc) 0;;
((double (double double)) inc) 5;;
(double mul) 1 ;;
(double double) mul 2 ;;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree 
-> match tree with
    |Empty -> false
    |Node (a,b,c) -> if a = n then true else if mem n b then true else mem n c ;;


(* TODO *)
let t1 = Node (1, Empty, Empty)
let t2 = Node (1, Node(2, Empty, Empty), Node (3, Empty, Empty));;
mem 1 t1;;
mem 4 t2;;
mem 3 t2;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let addition a b  
= SUCC(b);;
    
let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
 match n1 with
 |ZERO -> n2 
 |SUCC(nat) -> natadd nat (addition nat n2) ;; 

let rec natmul : nat -> nat -> nat
=fun n1 n2  ->
 match n1 with
 |ZERO -> ZERO
 |SUCC(ZERO) -> n2
 |SUCC(nat) -> natadd n2 (natmul nat n2) ;; 
 (* TODO *)

let two = SUCC (SUCC ZERO) ;;
let three = SUCC (SUCC (SUCC ZERO)) ;;
let one = SUCC ZERO ;;
let four = SUCC(SUCC(SUCC(SUCC ZERO))) ;;

natadd ZERO one ;;
natadd two one ;;
natadd two three;; 

natmul ZERO one ;;
natmul two one ;;
natmul two three;; 
natmul three two ;; 
natmul four three;;

(*********;************)
(*     Problem 6     *)
(*********************)
type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula (* first argue is false, then false*) 
	| OrElse of formula * formula (* any argue in the line is true ,then true *)
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec eval : formula -> bool
    = fun f ->
    match f with
    |True -> true
    |False -> false
    |Not(b)->if b = True then eval True else eval False 
    |AndAlso(a,b) ->
            if a = False then eval False 
                    else if b = False then eval False else eval True
    |OrElse (a,b) -> if a = True then eval True 
                    else if b = True then eval True else eval False 
    |Imply(a,b) -> if a = False then eval True
                    else if b = True then eval True else eval False
    |Equal(a,b) -> if a = b then eval True else eval False ;;
                
 (* TODO *) 
eval (Imply (Imply (True,False), True)) ;;
eval (Equal (Num 1, Plus (Num 1, Num 2))) ;;

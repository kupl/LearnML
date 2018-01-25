(*********************)
(*     Problem 1     *)
(*********************)  
let rec max: int list -> int
= fun lst -> List.fold_right (fun x y -> if (x > y) then x else y) lst (List.hd lst)

let rec min : int list -> int
= fun lst -> List.fold_right (fun x y -> if (x > y) then y else x) lst (List.hd lst)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
			| [] -> []
			| h::t -> if pred h then h::(filter pred t) else (filter pred t)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f(a))

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec treeGetnumber: btree -> int
= fun tree -> match tree with
	|Empty -> raise (Failure "is empty")
	|Node(n, child1, child2) -> n

let rec treeGetchild1: btree -> btree
= fun tree -> match tree with
	|Empty -> raise (Failure "is empty")
        |Node(n, child1, child2) -> child1

let rec treeGetchild2: btree -> btree
= fun tree -> match tree with
	|Empty -> raise (Failure "is empty")
        |Node(n, child1, child2) -> child2

let rec mem : int -> btree -> bool
= fun n tree -> 
if tree = Empty then false 
else if  n = (treeGetnumber tree) then true 
else (mem n (treeGetchild1 tree)) || (mem n (treeGetchild2 tree))


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec getPrevious : nat -> nat -> nat
= fun n1 n2 -> 
if (SUCC(n1) = n2) then n1 
else getPrevious (SUCC(n1)) n2 


let rec natadd : nat -> nat -> nat
= fun n1 n2 ->	
if n2 = ZERO then  n1 
else natadd (SUCC(n1))  (getPrevious ZERO n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
if n2 = ZERO then ZERO 
else if n1 = ZERO then ZERO 
else if n1 = SUCC(ZERO) then n2  
else if n2 = SUCC(ZERO) then n1 
else natadd n1 (natmul n1 (getPrevious ZERO n2))

(*********************)
(*     Problem 6     *)
(*********************)
type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 
	
let rec getInt : exp -> int
= fun number -> match number with
	| Num (x) -> x
	| Plus (a, b) -> getInt(a) + getInt(b)
	| Minus (a, b) -> getInt(a) - getInt(b)
	
let rec getExpression : exp -> exp
= fun f -> match f with
			|Num (x)-> Num(x)
			|Plus (a, b) -> Num(getInt(Plus(a, b)))
			|Minus (a, b) -> Num(getInt(Minus(a, b)))
			
let rec getValue : formula -> formula
= fun f -> match f with
	| True -> True
	| False -> False
	| Not(x) -> if x = True then False else if x = False then True else (getValue(Not(getValue(x))))
	| AndAlso (x, y) -> if x = True && y = True then True else if x = False || y = False then False else (getValue(AndAlso(getValue(x), getValue(y))))
	| OrElse (x, y) -> if x = True && y = False then True else if x = False && y = True then True else if x = False && y = False then False else if x = True && y = True then False else (getValue(OrElse(getValue(x), getValue(y))))
	| Imply (x, y) -> if x = False then True else if x = True && y = True then True else if x = True && y = False then False else (getValue(Imply (getValue(x), getValue(y))))
	| Equal (ex1, ex2) -> if getExpression (ex1) = getExpression (ex2) then True else False
 			

let rec eval : formula -> bool
= fun f ->
if f = True then true
else if f = False then false
else eval(getValue(f))


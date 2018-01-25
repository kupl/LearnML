(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
match t with 
Empty -> Empty
|Node (int, tl, tr) -> Node (int, (mirror tr), (mirror tl))

(*problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
match n1 with
ZERO -> n2 
|SUCC ZERO -> SUCC n2
|SUCC (nat) -> SUCC (natadd nat n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
match n1 with
ZERO -> ZERO
|SUCC ZERO -> n2
|SUCC (nat) -> natadd (natmul nat n2) n2

let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
match n2 with
ZERO -> SUCC ZERO
|SUCC ZERO -> n1
|SUCC (nat) -> natmul n1 (natexp n1 nat)

(*problem 3 - Var*)
type formula =
	True
	|False
	|Var of string
 	|Neg of formula
	|And of formula * formula
	|Or of formula * formula
	|Imply of formula * formula
	|Iff of formula * formula

let rec sat : formula -> bool
=fun f -> 
match f with
True -> true
|False -> false
|Var _ -> true
|Neg True -> false
|Neg False -> true
|Neg Var _ -> true
|Neg formula -> not(sat formula)
|And (True, True) -> true
|And (True, Var _) -> true
|And (Var _, True) -> true
|And (formula, False) -> false
|And (False, formula) -> false
|And (formula1, formula2) -> (sat formula1) && (sat formula2)
|Or (False, False) -> false
|Or (formula, True) -> true
|Or (True, formula) -> true
|Or (formula1, formula2) -> (sat formula1) || (sat formula2)
|Imply (True, False) -> false
|Imply (True, True) -> true
|Imply (True, Var _) -> true
|Imply (Var _, True) -> true
|Imply (True, formula) -> sat(formula)
|Imply (False, formula) -> true
|Imply (formula, True) -> true
|Imply (formula, False) -> not(sat formula)
|Imply (formula1, formula2) -> (((sat formula1) == false) || ((sat formula2) == true))
|Iff (formula1, formula2) -> ((sat formula1) == (sat formula2))



(*Problem 4*)
type aexp =
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> 
	match e with
	|Const n -> Const 0;
	|Var _ -> Const 1;
	|Power ( _ , 2 ) -> Times [ Const 2 ; Var "x" ]
	|Power ( _ , n ) -> Times [ Const n ; Power ( "x" , (n-1)) ]
	|Times [] -> raise (Failure "list is too short")  
	|Times [ Const n ; Var _ ] -> Const n
	|Times [ Const n ; Power ( _ , 2 ) ] -> Times [ Const ( 2 * n ) ; Var "x" ]
	|Times [ Const n1 ; Power ( _ , n2 ) ] -> Times [ Const ( n1 * n2 ) ; Power ( "x" , (n2 - 1))]
	|Times al -> Times al
	|Sum [] -> raise (Failure "list is too short")
	|Sum [aexp] -> diff(aexp, "x")	
	|Sum al -> Sum [diff((List.hd al), "x") ; diff((Sum (List.tl al)) , "x")]



(*problem 5*)
type exp = X
				| INT of int
				| ADD of exp * exp
			  | SUB of exp * exp
			  | MUL of exp * exp
			  | DIV of exp * exp
        | SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun e ->
    let rec sigma (n, m, f)    
		 = if n = m  then f n else (f n) + (sigma (n+1, m, f))
	      in let rec change_x : exp -> (int -> int)
           = fun changex -> match changex with
             |X -> (fun x -> x)
             |INT n -> (fun x -> n)
             |ADD (n, m) -> (fun x -> ((change_x n)x)+((change_x m)x))
             |SUB (n, m) -> (fun x -> ((change_x n)x)-((change_x m)x))
             |MUL (n, m) -> (fun x -> ((change_x n)x)*((change_x m)x))
             |DIV (n, m) -> (fun x -> ((change_x n)x)/((change_x m)x))
             |SIGMA (n, m, f) -> (fun x -> sigma((change_x n)x, (change_x m)x, (change_x f)))
in match e with
X -> raise (Failure "X is not int")
|INT n -> n
|ADD (n, m) -> (calculator n) + (calculator m)
|SUB (n, m) -> (calculator n) - (calculator m)
|MUL (n, m) -> (calculator n) * (calculator m)
|DIV (n, m) -> (calculator n) / (calculator m)
|SIGMA (n, m, exp) -> sigma (calculator n, calculator m, change_x exp)



					










(*problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
						|CompoundBranch of length * mobile
and length = int
and weight = int

let rec amount : mobile -> int
= fun m ->
match m with
(SimpleBranch (l1, w1), SimpleBranch (l2,w2)) -> w1 + w2
|(SimpleBranch (l1, w1), CompoundBranch (l2, (SimpleBranch (l3, w3), SimpleBranch (l4, w4)))) -> w1 + w3 + w4
|(SimpleBranch (l1, w1), CompoundBranch (l2, mobile)) -> w1 + (amount mobile)
|(CompoundBranch (l1, (SimpleBranch(l2, w2), SimpleBranch(l3, w3))), SimpleBranch(l4,w4)) -> w2 + w3 + w4
|(CompoundBranch (l1, mobile), SimpleBranch(l2, w2)) -> (amount mobile) + w2 
|(CompoundBranch (l1, (SimpleBranch(l2, w2), SimpleBranch(l3, w3))), CompoundBranch(l4, (SimpleBranch(l5, w5), SimpleBranch(l6, w6)))) -> w2 + w3 + w5 + w6
|(CompoundBranch (l1, mobile1), CompoundBranch (l2, mobile2)) -> (amount mobile1) + (amount mobile2)

let rec balanced : mobile -> bool
= fun m ->
match m with
(SimpleBranch (l1, w1) , SimpleBranch (l2, w2)) -> (l1 * w1) == (l2 * w2)
|(SimpleBranch (l1, w1) , CompoundBranch (l2, (SimpleBranch (l3, w3), SimpleBranch (l4, w4)))) -> ((l3 * w3) == (l4 * w4)) && ((l1*w1) == (l2 *(w3 + w4))) 
|(SimpleBranch (l1, w1), CompoundBranch (l2, mobile)) -> (balanced mobile) && ((l1 * w1) == (l2 * (amount mobile)))
|(CompoundBranch (l1, (SimpleBranch(l2, w2), SimpleBranch(l3, w3))), SimpleBranch(l4,w4)) -> ((l2*w2)==(l3*w3)) && ((l1 * (w2+w3))==(l4*w4))
|(CompoundBranch (l1, mobile), SimpleBranch(l2, w2)) -> (balanced mobile) && ((l1 * (amount mobile)) == (l2 * w2))
|(CompoundBranch (l1, (SimpleBranch(l2, w2), SimpleBranch(l3, w3))), CompoundBranch(l4, (SimpleBranch(l5, w5), SimpleBranch(l6, w6)))) -> ((l2 * w2) == (l3 * w3)) && ((l5 * w5) == (l6 * w6)) && ((l1 * (w2 + w3)) == (l4 * (w5 + w6)))
|(CompoundBranch (l1, mobile1), CompoundBranch (l2, mobile2)) -> (balanced mobile1) && (balanced mobile2) && ((l1 * (amount mobile1)) == (l2 * (amount mobile2)))


(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec pow2 : int -> int
= fun n -> if n = 0 then 1 else if n = 1 then 2 else 2 * (pow2 (n-1))

let rec b2t : bin -> int
= fun b ->
match (List.length b) with
0 -> raise (Failure "list is too short")
|1 -> 
	(match b with 
		|[ZERO] -> 0
		|[ONE] -> 1
		|_ -> raise (Failure "Type Error"))
|_ -> (pow2 ((List.length b) - 1)) * (b2t [List.hd b]) + b2t (List.tl b)

let rec t2b : int -> bin
= fun n -> if n = 0 then [ZERO] else if n = 1 then [ONE] else List.append (t2b (n/2)) (t2b (n mod 2))

let bmul : bin -> bin -> bin
= fun b1 b2 -> t2b ((b2t b1) * (b2t b2))

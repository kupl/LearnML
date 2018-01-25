(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
	        | Empty -> Empty
	        | Node(a,b,c) -> if b = Empty || c = Empty then Node(a,c,b)
			                    else Node(a, mirror c, mirror b)

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
	 	          | ZERO -> n2
		          | SUCC nat -> if nat = ZERO then SUCC n2
				                    else SUCC (natadd nat n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
		          | ZERO -> ZERO
		          | SUCC nat -> if nat = ZERO then n2
			      	            else natadd (natmul (SUCC ZERO) n2) (natmul nat n2)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
		          | ZERO -> SUCC ZERO
		          | SUCC nat -> if nat = ZERO then n1
				                    else natmul (natexp n1 (SUCC ZERO)) (natexp n1 nat)


(* problem 3
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec sat : formula -> bool
= fun f -> match f with
	| And (formula1, Neg formula1) -> false
	| Iff (formula1, formula2) -> match formula 1 with
					| (And 



let find : formula -> bool
	| And (formula1, Neg formula1) -> false
	| Or (formula1, Neg formula1) -> true
	| Imply (formula1, formula2) 
	| 
let rec sat : formula -> bool
= fun f -> match f with
	| True -> true
	| False -> false
	| Neg formula -> true
	| And (formula1, Neg formula1) -> false
	| Or (formula1, Neg formula1) -> true
	| Imply (formula1, formula2) -> 
	| _ -> true
	


	| Neg formula -> if (sat formula) = false then true else false
	| And (formula1, formula2) -> (sat formula1) && (sat formula2)
	| Or (formula1, formula2) -> (sat formula1) || (sat formula2)
	| Imply (formula1, formula2) -> if (sat formula1) = false then (sat formula2)
					else (sat formula2)
	| Iff (formula1, formula2) -> (sat formula1) = (sat formula2)	
	| Var _ -> true *)
(*
	| True -> true
	| Neg formula -> if formula = False then true else false
	| And (formula1,formula2) -> if formula1 = True && formula2 = True then true else false
	| Or (formula1,formula2) -> if formula1 = True || formula2 = True then true else false
	| Imply (formula1,formula2) -> if formula1 = False then true
					else if formula2 = True then true
					else false
	| Iff (formula1,formula2) -> if formula1 = formula2 then true
					else false
	| _ -> false
*)

(* problem 4*)

type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
		
let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
              | Const (n) -> Const 0
              | Var (str) -> if str=x then Const 1 else Const 0
              | Power (str,n) -> if n=0 then Const 0
				else if n=1 then Const 1
                               	else Times [Const n; Power (str,n-1)]
              | Times lst -> (match lst with
				| [] -> Const 0
                            	| hd::t1 -> (match t1 with
					| [] -> Sum [Times [diff ((hd,x)); Const 1]; Times [hd; diff (Times t1,x)]]
					| _ -> Sum [Times [diff (hd,x); Times t1]; Times [hd; diff (Times t1,x)]]))      
              | Sum lst -> (match lst with
			| [] -> Const 0
                        | hd::t1 -> Sum [diff (hd,x); diff (Sum t1,x)])
                            
              (* diff (Sum [Power ("x",2); Times [Const 2; Var "x"]; Const 1],"x") ;; *)


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec helpsigma
= fun h -> match h with
	| X -> (fun x -> x)
	| INT n -> (fun x -> n)
	| ADD (a,b) -> (fun x -> ((helpsigma a) x + (helpsigma b) x))
	| SUB (a,b) -> (fun x -> ((helpsigma a) x - (helpsigma b) x))
	| MUL (a,b) -> (fun x -> ((helpsigma a) x * (helpsigma b) x))
	| DIV (a,b) -> (fun x -> ((helpsigma a) x / (helpsigma b) x))
	| SIGMA (a,b,c) -> raise (Failure "error")

let rec operation
= fun opr -> match opr with
	| X -> raise (Failure "error")
	| INT n -> n
	| ADD (n1,n2) -> (operation n1) + (operation n2) 
	| SUB (n1,n2) -> (operation n1) - (operation n2) 
	| MUL (n1,n2) -> (operation n1) * (operation n2)
	| DIV (n1,n2) -> (operation n1) / (operation n2)
	| SIGMA (n1,n2,n3) -> let rec sigma
				= fun k1 k2 -> if k1>k2 then 0
						else if k1=k2 then (helpsigma n3) k1
						else (helpsigma n3) k1 + sigma (k1+1) k2
				in sigma (operation n1) (operation n2)
(*

	| DIV (e1,e2) -> if calculator e2 = 0 then raise (Failure "division by zero")
			else int_of_float(calculator e1 / calculator e2)
	| SIGMA (e1,e2,e3) ->  
			
*)	

let rec calculator : exp -> int
= fun e -> match e with
	| X -> 0
	| INT n -> n
	| ADD (e1,e2) -> operation (ADD (e1,e2))
	| SUB (e1,e2) -> operation (SUB (e1,e2))
	| MUL (e1,e2) -> operation (MUL (e1,e2))
	| DIV (e1,e2) -> operation (DIV (e1,e2))
	| SIGMA (e1,e2,e3) -> operation (SIGMA (e1,e2,e3))

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec calWeight : branch -> int
= fun b -> match b with
	| SimpleBranch (len,w) -> w
	| CompoundBranch (len, mobile) -> match mobile with
					| (a,b) -> calWeight a + calWeight b
let rec calTorque : branch -> int
= fun b -> match b with
	| SimpleBranch (len,w) -> len * w 
	| CompoundBranch (len,mobile) -> match mobile with
					| (a,b) -> if (calTorque a = calTorque b) then len * (calWeight a + calWeight b) else -1
(* not same torque -> not balanced*)
let balanced : mobile -> bool
= fun m -> match m with
	| (left,right) -> if (calTorque left) = (calTorque right) then true else false 


(* problem 7 *)
type digit = ZERO | ONE
type bin = digit list


let rec helpB2d : bin -> bin (*reverse*)
= fun b1 -> match b1 with
	| [] -> []
	| hd::t1 -> (helpB2d t1) @ [hd]

let rec b2d : bin -> int
= fun b1 -> match b1 with
	| [] -> 0
	| hd::t1 -> match hd with
			| ONE -> 1 + 2 * (b2d t1)
			| ZERO -> 2 * (b2d t1)
let rec d2b : int -> bin
= fun n -> match n with
	| 0 -> [ZERO]
	| 1 -> [ONE]
	| _ -> if (n mod 2 = 0) then (d2b (n/2))@[ZERO] else (d2b ((n-1)/2))@[ONE] 

let bmul : bin -> bin -> bin
= fun b1 b2 -> d2b ( (b2d (helpB2d b1)) * (b2d (helpB2d b2)) )

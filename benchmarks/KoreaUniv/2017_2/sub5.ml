(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> let rec swap f = match f with | Empty -> Empty | Node(a,x,y) -> Node(a, (swap y) ,(swap x)) in swap t;;

(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> let rec func n1 n2 = match n1 with | ZERO ->n2 | SUCC n1 -> SUCC(func n1 n2) in func n1 n2;; 

let two = SUCC(SUCC ZERO);;
let three = SUCC(SUCC(SUCC ZERO));;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> let funa n2 = match n2 with | ZERO -> ZERO | SUCC n2 -> n2 in let funb n2 = match n2 with | ZERO -> ZERO | SUCC n2 -> n2 in let k = funb n1 in let rec func n1 n2 = match n1 with | ZERO -> if (funa n2) != ZERO then SUCC(func k (funb n2)) else ZERO | SUCC n1 -> if n2 != ZERO then SUCC(func n1 n2) else ZERO in func n1 n2;;

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let funa n2 = match n2 with | ZERO -> ZERO | SUCC n2 -> n2 in let sum = n1 in let mult n1 = natmul sum n1 in let rec funb n1 n2 = match n2 with | ZERO -> SUCC(ZERO) | SUCC n2 -> if (funa n2) != ZERO then funb (mult n1) (funa n2) else mult n1 in funb n1 n2;;

(*
(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f -> (* TODO *)
*)

(*
(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> let funa p = match p with | [] -> [] | hd::tl -> hd in
		match e with | Const(a) -> Const 0
			    | Var(a) -> if a!=x then Const 0 else Const 1
			    | Power(a,b) -> if a!=x then Const 0 else Times (Const b :: Power(a ,(b-1)) :: [])
			    | Times k -> (match k with | hd::tl -> Times [hd::(diff (funa tl) x)])
 			    | Sum k -> (match k with | hd::tl -> Sum (hd :: Sum tl));;
*)

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let funb env = match env with | [] -> failwith "failure" | hd::tl -> hd
	   in let rec funa e env = match e with | X -> funb env
					        | INT a -> a
					        | ADD(a,b) -> (funa a env) + (funa b env) 
					        | SUB(a,b) -> (funa a env) - (funa b env) 
					        | MUL(a,b) -> (funa a env) * (funa b env)
					        | DIV(a,b) -> if (funa b env) != 0 then (funa a env) / (funa b env) else failwith "Divided by Zero"
					        | SIGMA(a,b,c) -> if (funa a env) = (funa b env) then (funa c ((funa b env)::env))
								else ((funa (SIGMA(b,b,c)) env) + (funa (SIGMA(a,SUB(b,INT 1),c)) env))
		in funa e [];;

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> let rec funl k = match k with (x,_) -> x in let rec funr k = match k with (_,x) -> x in 
			let sum1 = 0 in let sum2 = 0 in
			let rec funa k = match k with | (SimpleBranch(a,b), SimpleBranch(c,d)) -> let sum1 = sum1 + b + d in sum1
						      | (SimpleBranch(a,b), CompoundBranch(c,d)) -> let sum1 = sum1 + b + (funa d) in sum1
						      | (CompoundBranch(a,b), SimpleBranch(c,d)) -> let sum1 = sum1 + (funa b) + d in sum1
                                                      | (CompoundBranch(a,b), CompoundBranch(c,d)) -> let sum1 = sum1 + (funa b) + (funa d) in sum1 in
			let rec funb k = match k with | (SimpleBranch(a,b), SimpleBranch(c,d)) -> let sum2 = sum2 + b + d in sum2
						      | (SimpleBranch(a,b), CompoundBranch(c,d)) -> let sum2 = sum2 + b + (funb d) in sum2
						      | (CompoundBranch(a,b), SimpleBranch(c,d)) -> let sum2 = sum2 + (funb b) + d in sum2
                                                      | (CompoundBranch(a,b), CompoundBranch(c,d)) -> let sum2 = sum2 + (funb b) + (funb d) in sum2 in
				let func k = match k with | SimpleBranch(a,b) -> let sum1 = sum1 + b in sum1
							  | CompoundBranch(a,b) -> let sum1 = sum1 + (funa b) in sum1 in
				let fund k = match k with | SimpleBranch(a,b) -> let sum2 = sum2 + b in sum2
							  | CompoundBranch(a,b) -> let sum2 = sum2 + (funb b) in sum2 in
					let fune k = match k with | SimpleBranch(a,b) -> a
							          | CompoundBranch(a,b) -> a in 
					if ((func (funl m)) * (fune (funl m))) = ((fund (funr m)) * (fune (funr m))) then true else false;;

							    
	   
(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> let rec length l = match l with | [] -> 0 | hd::tl -> 1 + length tl in let rec exp k = if k = 0 then 1 else 2 * exp (k-1) in 
			let rec funa k = match k with | [] -> 0 | hd::tl -> if hd = ONE then (exp ((length k) - 1)) + funa tl else funa tl in
				let rec fund k = match k with | [] -> 0 | hd::tl -> if hd = ONE then (exp ((length k) - 1)) + fund tl else fund tl in
					let funb k = k mod 2 in
					let rec func k lis = if k = 1 then ONE::lis else if (funb k) = 1 then let lis = ONE::lis in func (k/2) lis else let lis = ZERO::lis in func (k/2) lis in  
						func (funa b1 * fund b2) [];;




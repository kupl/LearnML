(* problem 1*) 
type btree = 
	| Empty 
	| Node of int * btree * btree 
let rec mirror : btree -> btree 
= fun t -> 
match t with
	|Empty -> Empty 
	|Node(m,t1,t2) -> Node(m, mirror t2, mirror t1) 
(* TODO *) 
let t1 = Node(1,Empty,Empty);;
let t2 = Node(1,Node(2,Node(3,Empty,Empty),Empty),Node(4,Empty,Empty));;
mirror t1;;
mirror t2;;
(* problem 2*) 
type nat = ZERO 
	| SUCC of nat 

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
match n2 with
	|ZERO -> n1
	|SUCC m -> natadd (SUCC n1) m

(* TODO *) 
let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
match n2 with
 	|ZERO -> ZERO
 	|SUCC m -> natadd n1 (natmul n1 m)
(* TODO *) 
let natexp : nat -> nat -> nat 
= fun n1 n2 -> 
match n2 with
	|ZERO -> SUCC ZERO
	|SUCC m -> if m = ZERO then SUCC ZERO else  natmul n1 (natmul n1 m) 
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natadd two three;;
natmul two three;;

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
match f with
|True -> true
|False -> false
|Var v -> bool_of_string v
|Neg(f1) -> if (f1 = True) then false else true 
|And(f1,f2) -> if (f1 = False) then false else if (f2 = True) then true else false
|Or(f1,f2) -> if (f1 = True) then true else if (f2 = True) then true else false 
|Imply(f1,f2) -> if (f1 = True && f2 = False) then false else true 
|Iff(f1,f2) -> if (f1 = f2) then true else false

(* problem 4*) 

  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (e, v) ->
      match e with
      | Const a -> Const 0
      | Var s   -> if s=v then Const 1 else Const 0
      | Power (s,n) -> if s=v then Times[Const n; Power(s,n-1)] else Const 0     
      | Times lst -> (match lst with
                    | []    -> Const 1   
                    | h::[] -> diff(h,v)            
                    | h::t  -> (match h with
                                | Const 0 -> Const 0
                                | Const 1 -> diff (Times t,v)
                                | Const n -> Times[Const n; diff (Times t,v)]
                                | _ -> Sum [Times(diff(h,v)::t); Times[h;diff(Times t,v)]]))
      | Sum lst2 -> (match lst2 with
                    | []    -> Const 0
                    | h::[] -> diff(h,v)
                    | h::t  -> Sum [ (diff(h,v)) ; (diff(Sum t,v)) ])

(* problem 5*) 
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec sigma : int * int * (int->int) -> int
  = fun (n1, n2, func) ->
    if n1 > n2 then 0
    else if n1 = n2 then func n1
    else (func n1) + (sigma (n1+1,n2,func));; 

  let rec etof : exp -> (int -> int)
  = fun e ->
      match e with
  | X -> (fun x -> x)
  | INT n -> (fun x -> n)
  | ADD (exp1, exp2) -> (fun x -> ((etof exp1) x) + ((etof exp2) x))
  | SUB (exp1, exp2) -> (fun x -> ((etof exp1) x) - ((etof exp2) x))
  | MUL (exp1, exp2) -> (fun x -> ((etof exp1) x) * ((etof exp2) x))
  | DIV (exp1, exp2) -> (fun x -> ((etof exp1) x) / ((etof exp2) x))
  | SIGMA (exp1, exp2, exp3) -> (fun x -> sigma( (etof exp1) x, (etof exp2)x, (etof exp3)));;

  let rec calculator : exp -> int
  = fun e -> match e with
    | X -> raise (Failure "error")
    | INT a -> a
    | ADD (INT n1, INT n2) -> n1 + n2
    | ADD (exp1, exp2) -> (calculator exp1) + (calculator exp2)
    | SUB (INT n1, INT n2) -> n1 - n2
    | SUB (exp1, exp2) -> (calculator exp1) - (calculator exp2)
    | MUL (INT n1, INT n2) -> n1 * n2
    | MUL (exp1, exp2) -> (calculator exp1) * (calculator exp2)
    | DIV (INT n1, INT n2) -> if n2 = 0 then raise (Failure "DIVISION ERROR") else n1/n2
    | DIV (exp1, exp2) -> (calculator exp1) / (calculator exp2)
    | SIGMA (INT n1, INT n2, exp1) -> sigma (n1, n2, (etof exp1))
    | SIGMA (exp1, exp2, exp3) -> sigma (calculator exp1, calculator exp2, etof exp3);;

 
(* problem 6*) 
type mobile = branch * branch 
(* left and rigth branches *) 
	and branch = 
	SimpleBranch of length * weight 
	| CompoundBranch of length * mobile 
	and length = int 
	and weight = int 
	
	let rec bweight : branch -> weight
  = fun brc ->
      match brc with
      SimpleBranch (_,w) -> w
      |CompoundBranch (l,m) ->
      (match m with
	     (right,left) -> (bweight right)+ (bweight left));;
	
	let rec btorque : branch -> int
	= fun brc ->
	match brc with 
	SimpleBranch (l,w) -> l * w
	|CompoundBranch (l,m) -> 
		(match m with
			(right,left) -> l * (btorque right * btorque left));;
	let rec balanced : mobile -> bool 
= fun m -> 
match m with 
(left,right) ->
(match left,right with 
|SimpleBranch (l1,w1),SimpleBranch (l2,w2) ->
if (btorque left = btorque right) then true else false
|SimpleBranch (l1,w1),CompoundBranch (l2,m) ->
if (balanced m = true) && (btorque left = btorque right) then true else false
|CompoundBranch (l1,m), SimpleBranch (l2,w2) ->
if (balanced m = true) && (btorque left = btorque right) then true else false
|CompoundBranch(l1,m1), CompoundBranch(l2,m2) ->
if (balanced m1 = true) && (balanced m2 = true) &&(btorque left = btorque right)
then true else false
)

(* TODO *) 
(* problem 7*) 

(* TODO *) 

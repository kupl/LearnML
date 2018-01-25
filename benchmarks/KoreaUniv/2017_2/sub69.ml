(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
=fun t -> match t with Node(a,b,c)
-> if (b = Empty) && (c = Empty) then t
else if (b = Empty) then Node(a, (mirror c), Empty)
else if (c = Empty) then Node(a, Empty, (mirror b))
else Node(a, (mirror c), (mirror b))


(* problem 2*) 
type nat = ZERO | SUCC of nat 

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC(k) -> natadd k (SUCC (n2))

let natmul : nat -> nat -> nat 
= fun n1 n2 -> if (n1 = ZERO) || (n2 = ZERO) then ZERO 
else
let rec multi : nat -> nat -> nat -> nat
= fun n1 n2 n3 -> match n1 with
| SUCC ZERO -> n2
| SUCC(k) -> multi k (natadd n3 n2) n3 in multi n1 n2 n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let rec exp : nat -> nat -> nat -> nat
= fun n1 n2 n3 -> match n2 with
| ZERO -> SUCC ZERO
| SUCC(k) -> natmul (exp n1 k n3) n1 in
exp n1 n2 n1


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

let sat : formula -> bool = fun f
->
let rec findvar : formula -> (formula * bool)list -> (formula * bool)list -> (formula * bool)list = fun var l1 l2
->match l1 with
| [] -> [(var, true)]@l2
| (a, b)::tail -> if (a = var) then l2
        else findvar var tail l2 in

let rec findvar2 : formula -> (formula * bool) list -> bool = fun var l
-> match l with
| [] -> true
| (a, b)::tail -> if var = a then b
        else findvar2 var tail in


let rec mlist : formula -> (formula * bool) list -> (formula * bool) list = fun f l
-> match f with
| True -> []
| False -> []
| Var a -> findvar (Var a) [] []
| Neg a -> mlist a l
| And (a, b) -> (mlist a l)@(mlist b l)
| Or (a, b) -> (mlist a l)@(mlist b l)
| Imply (a, b) -> (mlist a l)@(mlist b l)
| Iff (a, b) -> (mlist a l)@(mlist b l) in

let onepos = mlist f [] in

let tablechange : (formula * bool) list -> (formula * bool) list = fun l
-> match l with
| [] -> []
| (a, b)::tail -> if b = true then ((a, false)::tail) else tail in

let rec exetable : formula -> (formula * bool) list -> bool = fun f table
-> match f with
| True -> true
| False -> false
| Var a -> findvar2 (Var a) table
| Neg a -> not(exetable a table)
| And (a, b) -> (exetable a table) && (exetable b table)
| Or (a, b) -> (exetable a table) || (exetable b table)
| Imply (a, b) -> if ((exetable a table) = true) && ((exetable b table) = false) then false else true
| Iff (a, b) -> if (exetable a table) = (exetable b table) then true else false in

let rec test : (formula * bool) list -> bool = fun l
-> match l with
| [] -> false
| (a,b)::tail -> b || (test tail) in

let rec exeall : formula -> (formula * bool) list -> bool = fun f l
-> if (test l) = false then exetable f l
else (exetable f l) || (exeall f (tablechange l)) in
exeall f onepos


(* problem 4*) 
type aexp = 
| Const of int 
| Var of string 
| Power of string * int 
| Times of aexp list 
| Sum of aexp list 

let rec diff : aexp * string -> aexp 
= fun (e, x) -> match e with
| Const b -> Const 0 
| Var a -> if a = x then Const 1 else Var a
| Power (a,b) -> if a = x then Times[Const b; Power(a, b-1)] else Power(a, b)
| Times l -> begin match l with 
 | [] -> Times l
 | head::tail -> Sum[Times([diff(head,x)]@tail); Times[head; diff(Times tail, x)]]
 end
| Sum l2 -> begin match l2 with
 | [] -> Sum l2
 | head::tail -> Sum[diff(head,x); diff((Sum tail), x)]
 end

 

(* problem 5*) 
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let rec calculator : exp -> int = fun e 
-> match e with
| INT a -> a
| ADD (a, b) -> (calculator a) + (calculator b)
| SUB (a, b) -> (calculator a) - (calculator b)
| MUL (a, b) -> (calculator a) * (calculator b)
| DIV (a, b) -> (calculator a) / (calculator b)
| SIGMA (a, b, c) -> if (calculator a)>(calculator b) then 0 
   else eval(c,a) + calculator (SIGMA ( ADD( a, INT 1),  b, c))
and eval (f, x) = match f with
| X -> calculator x
| INT n -> n
| ADD(a,b) -> eval(a, x) + eval (b,x)
| SUB(a,b) -> eval(a, x) - eval (b,x)
| MUL(a,b) -> eval(a, x) * eval (b,x)
| DIV(a,b) -> eval(a, x) / eval (b,x)

 


(* problem 6*) 
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool = fun m 
-> let rec tweight : mobile -> int = fun m 
-> match m with 
| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1+w2
| (CompoundBranch (l1, w1), SimpleBranch (l2, w2)) -> (tweight w1)+ w2
| (SimpleBranch (l1, w1), CompoundBranch(l2, w2)) -> w1+(tweight w2)
| (CompoundBranch (l1, w1), CompoundBranch(l2,w2)) -> (tweight w1) + (tweight w2)in 

let rec balance : mobile->bool->bool = fun m b 
-> if b=false then false 
else match m with
| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if (l1*w1) = (l2*w2) then true else false
| (CompoundBranch (l1, w1), SimpleBranch (l2, w2)) -> if (balance w1 true) = false then false 
       else if ((tweight w1)*l1) = (l2*w2) then true
       else false 
| (SimpleBranch (l1, w1), CompoundBranch(l2, w2)) -> if (balance w2 true) = false then false 
       else if ((tweight w2)*l2) = (l1*w1) then true
       else false 
| (CompoundBranch (l1, w1), CompoundBranch(l2,w2)) -> if (balance w2 true)&&(balance w1 true) = false then false 
       else if ((tweight w2)*l2) = ((tweight w1)*l1) then true
       else false in 
balance m true

 

(* problem 7*) 
type digit = ZERO | ONE 
type bin = digit list 

let bmul : bin -> bin -> bin = fun w1 w2 
-> let rec mlist : bin -> 'a list = fun b
-> match b with
| [] -> []
| head::tail -> if (head = ONE) then (mlist tail)@[1]
  else (mlist tail)@[0] in
let rec fastexpt : int -> int -> int = fun b n
-> if n = 0 then 1
 else if (n mod 2) = 0 then fastexpt b (n/2) * fastexpt b (n/2)
 else b * fastexpt b (n-1) in
let rec mdecimal : int -> 'a list -> int = fun n l
-> match l with
| [] -> 0
| head::tail -> (head * (fastexpt 2 n)) + (mdecimal (n+1) tail) in
let rec mbinary : int -> bin -> bin = fun n b
-> if n < 1 then b
else if (n mod 2) = 0 then mbinary (n/2) ([ZERO]@b)
else mbinary ((n-1)/2) ([ONE]@b) in
let v1 = mdecimal 0 (mlist(w1)) in
let v2 = mdecimal 0 (mlist(w2)) in
let v3 = v1 * v2 in
mbinary v3 []

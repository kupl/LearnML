(* problem 1 *)
type btree = Empty | Node of int * btree * btree
let rec mirror : btree -> btree = fun t ->
    match t with
       Empty -> Empty
      |Node(k, left, right) -> Node(k, mirror right, mirror left);;


(* problem 2 *)
type nat = ZERO | SUCC of nat
let rec natadd : nat -> nat -> nat = fun n1 n2 ->
     match n1 with
        ZERO -> n2
       |SUCC(k) -> SUCC(natadd k n2);;

let rec natmul : nat -> nat -> nat = fun n1 n2 ->
     match n1 with
        ZERO -> ZERO
       |SUCC(k) -> natadd (natmul k n2) n2;;

let rec natexp : nat -> nat -> nat = fun n1 n2 ->
     match n2 with
        ZERO -> SUCC(ZERO)
       |SUCC(k) -> natmul n1 (natexp n1 k);; 


(* problem 3*)
type  formula = True | False | Var of string | Neg of formula | And of formula * formula | Or of formula * formula | Imply of formula * formula | Iff of formula * formula
let rec sat : formula -> bool = fun f ->
     match f with
       True -> true
      |False -> false
      |Var p -> true
      |Neg p -> if sat p == true then false else true
      |And (p, q) -> sat p && sat q
      |Or (p, q) -> sat p || sat q    
      |Imply (p, q) -> if (sat p == true) && (sat q == false) then false else true
      |Iff (p, q) -> if sat p == sat q then true else false;;


(* problem 4 *)
type aexp = |Const of int | Var of string | Power of string * int | Times of aexp list | Sum of aexp list
let rec diff : aexp * string -> aexp = fun (e,x) ->
   match e with
     | Const i -> Const 0
     | Var s -> Const 1
     | Power(s,i) -> Times[Const i; Power(s, i-1)]
     | Times l ->(match l with
                  [] -> Const 0
		 |hd::tl -> Times[hd; diff(List.hd tl,x)])
     | Sum l -> match l with
                 [] -> Const 0
                |hd::tl -> Sum [diff(hd, x); diff(Sum tl,x)];;

(* problem 5*)
type exp = X | INT of int | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp
let rec sigma_help : exp*int -> exp = fun (e,i) ->
   match e with
     X -> INT i
    | INT n -> INT n
    | ADD(e1, e2) -> ADD(sigma_help(e1,i), sigma_help(e2,i))
    | SUB(e1, e2) -> SUB(sigma_help(e1,i), sigma_help(e2,i))
    | MUL(e1, e2) -> MUL(sigma_help(e1,i), sigma_help(e2,i))
    | DIV(e1, e2) -> DIV(sigma_help(e1,i), sigma_help(e2,i))
    | SIGMA(e1, e2, e3) -> sigma_help(e3, i);;

let rec calculator : exp -> int = fun e ->
   match e with 
    | INT i -> i
    | ADD (e1, e2) -> calculator(e1) + calculator(e2)
    | SUB (e1, e2) -> calculator(e1) - calculator(e2)
    | MUL (e1, e2) -> calculator(e1) * calculator(e2)
    | DIV (e1, e2) -> calculator(e1) / calculator(e2)
    | SIGMA (e1, e2, e3) -> if calculator(e1) > calculator(e2) then 0 else  calculator(sigma_help(SIGMA(e1, e2, e3),calculator(e1))) + calculator(SIGMA(ADD(e1, INT 1),e2,e3));;
	

(* problem 6*)
type mobile = branch * branch 
and branch = SimpleBranch of length * weight | CompoundBranch of length * mobile
and length = int
and weight = int
let rec calWeight : branch -> int = fun b ->
   match b with
   |SimpleBranch(len, wei) -> wei
   |CompoundBranch(len, mob) -> calWeight (fst mob) + calWeight (snd mob);;

let rec calLength : branch -> int = fun b ->
   match b with
   |SimpleBranch(len, wei) -> len
   |CompoundBranch(len, mob) -> len

let balanced : mobile -> bool = fun m ->
   if calLength (fst m) * calWeight (fst m) = calLength (snd m) * calWeight (snd m) then true
   else false;;


(* problem 7 *)
(*type digit = ZERO | ONE 
type bin = digit list
let bmul : bin -> bin -> bin = fun b1 b2 ->
*)

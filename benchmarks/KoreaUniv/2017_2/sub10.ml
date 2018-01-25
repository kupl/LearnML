(*
    Author: Jacob Pihl - 2017952019
    Date: 25 Nov 2017
    Description: Homework 2 for Programming Languages

    Known problems:
      *Problem 4 resulting aexp is not same as example but represents the same function.
*)

(* Helper Functions *)
let rec nth l n =
match l with
  [] -> 0
  |a::t -> if n=0 then a else nth t (n-1)

(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> 
match t with
  |Empty -> Empty
  |Node(a, left, right) -> Node(a, mirror right, mirror left)

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
  |ZERO -> n2
  |SUCC(nat) -> SUCC (natadd nat n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
match n2 with
  |ZERO -> n2
  |SUCC(nat) -> natadd n1 (natmul n1 nat)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
match n2 with
  |ZERO -> SUCC ZERO
  |SUCC(nat) -> natmul n1 (natexp n1 nat)

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

(* Helper functions problem 3 *)
let rec extractVariables vars exp = 
match exp with
  |True -> "True"::vars
  |False -> "False"::vars
  |Var(a) -> a::vars
  |Neg(form) -> extractVariables vars form
  |And(a,b) -> extractTwo a b vars
  |Or(a,b) -> extractTwo a b vars
  |Imply(a,b) -> extractTwo a b vars
  |Iff(a,b) -> extractTwo a b vars
and extractTwo expA expB vars = 
  let vars = extractVariables vars expA in
  extractVariables vars expB

let rec eval env exp =
match exp with
  |True -> true
  |False -> false
  |Var(a) -> List.assoc a env
  |Neg(form) -> not(eval env form)
  |And(a,b) -> eval env a && eval env b
  |Or(a,b) -> eval env a || eval env b
  |Imply(a,b) -> eval env (Or(Neg(a), b))
  |Iff(a,b) -> eval env (Imply(a, b)) && eval env (Imply(b, a))

let rec genTruthTable values variables expr =
match variables with
  |[] -> [(List.rev values, eval values expr)]
  |th :: tl -> genTruthTable ((th, true) :: values) tl expr @ genTruthTable ((th, false) :: values) tl expr

let rec compareListElems lst = 
match lst with
  |[] -> false
  |(a,b)::tl -> b || compareListElems tl
(* End of helper functions problem 3 *)

let sat : formula -> bool
= fun f ->
  compareListElems (genTruthTable [] (extractVariables [] f) f)

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) ->
match e with
  |Const a -> Const 0
  |Var a -> if a = x then Const 1 else Const 0
  |Power (base,exp) -> begin
    if base = x then Times[Const exp; Power(base, exp-1)] else Const 0
  end
  |Times l -> begin
    match l with
    |[] -> Const 0
    |_::[] -> Const 0
    |th::tm::tl -> Sum [Times [diff (th,x); tm]; Times[diff (tm, x); th]; diff (Times tl, x)]
  end
  |Sum l -> begin
    match l with
    |[] -> Const 0
    |_::[] -> Const 0
    |th::tm::tl -> Sum [diff (th,x); diff (Sum tl, x)]
  end

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->
let rec evalC env sum exp =
match exp with
  |X -> nth env 0
  |INT a -> a
  |ADD(a, b) -> evalC env sum a + evalC env sum b
  |SUB(a, b) -> evalC env sum a - evalC env sum b
  |MUL(a, b) -> evalC env sum a * evalC env sum b
  |DIV(a, b) -> evalC env sum a / evalC env sum b
  |SIGMA(a,b,c) -> if (evalC env sum a) > (evalC env sum b) then sum else evalC ((evalC env sum a)::env) (sum + (evalC ((evalC env sum a)::env) sum c)) (SIGMA(INT ((evalC env sum a)+1), b, c))
in evalC [] 0 e

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

(* Helper functions problem 6 *)
let rec branchWeight tWeight branch =
  match branch with
    |SimpleBranch(l, w) -> (tWeight + w)
    |CompoundBranch(l, nMob) -> begin
      match nMob with
        |(b1,b2) -> (branchWeight tWeight b1) + (branchWeight tWeight b2)
    end

let mobWeight mob =
match mob with
  |(left, right) -> (branchWeight 0 left) + (branchWeight 0 right)

let branchTorque tTorque branch =
  match branch with
    |SimpleBranch(l, w) -> (tTorque + (l*w))
    |CompoundBranch(l, nMob) -> (tTorque + (l*(mobWeight nMob)))
(* End of helper functions problem 6 *)

let rec balanced : mobile -> bool
= fun m ->
match m with
  |(left, right) -> if (branchTorque 0 left) = (branchTorque 0 right) then true
    &&
    begin
      match left with
        |SimpleBranch(l, w) -> true
        |CompoundBranch(l, nMob) -> balanced nMob
    end
    &&
    begin
      match right with
        |SimpleBranch(l, w) -> true
        |CompoundBranch(l, nMob) -> balanced nMob
    end else false

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

(* Helper functions for problem 7*)
let addChar s c = s ^ String.make 1 c

let divTwoRemainder dec = (dec - ((dec / 2)*2))

let rec bin_of_dec bin dec =
  if dec = 0 then bin
  else begin
    match (divTwoRemainder dec) with
      |0 -> bin_of_dec (ZERO::bin) (dec/2)
      |1 -> bin_of_dec (ONE::bin) (dec/2)
      |_ -> [ZERO]
    end

let char_of_digit digit =
  match digit with
    |ZERO -> '0'
    |ONE -> '1'

let rec string_of_digitlst str digitlst =
  match digitlst with
    |[] -> str
    |h::t -> string_of_digitlst (addChar str (char_of_digit h)) t
(* End of helper functions for problem 7 *)

let bmul : bin -> bin -> bin
= fun b1 b2 -> bin_of_dec [] ((int_of_string (string_of_digitlst "0b" b1))*(int_of_string (string_of_digitlst "0b" b2)))
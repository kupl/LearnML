(*Problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
  match t with
  | Node (n, t1, t2) -> Node (n, mirror t2, mirror t1)
  | Empty -> Empty

(*Problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
  match n2 with
  | ZERO -> n1
  | SUCC x -> SUCC (natadd n1 x)
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
  match n2 with
  | ZERO -> ZERO
  | SUCC x -> natadd n1 (natmul n1 x)
let rec natexp : nat -> nat -> nat
= fun n1 n2 -> 
  match n2 with
  | ZERO -> SUCC ZERO
  | SUCC x -> natmul n1 (natexp n1 x)

(*Problem 3*)
type formula =
  | True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f ->
  let rec union l1 l2 = 
    match l2 with
    | x::xs -> if List.mem x l1 
               then union l1 xs
               else x::(union l1 xs)
    | [] -> l1
  in
  let rec formulaToVar formula = 
    match formula with
    | True -> []
    | False -> []
    | Var x -> [x]
    | Neg f -> formulaToVar f
    | And (f1, f2) -> union (formulaToVar f1) (formulaToVar f2)
    | Or (f1, f2) -> union (formulaToVar f1) (formulaToVar f2)
    | Imply (f1, f2) -> union (formulaToVar f1) (formulaToVar f2)
    | Iff (f1, f2) -> union (formulaToVar f1) (formulaToVar f2)
  in
  let rec substitution formula1 var fbool = 
    match formula1 with
    | True -> True
    | False -> False  
    | Var x -> if x = var then fbool else Var x
    | Neg f -> Neg (substitution f var fbool)
    | And (f1,f2) ->
      And (((substitution f1 var fbool), (substitution f2 var fbool)))
    | Or (f1,f2) ->
      Or (((substitution f1 var fbool), (substitution f2 var fbool)))
    | Imply (f1,f2) ->
      Imply (((substitution f1 var fbool), (substitution f2 var fbool)))
    | Iff (f1,f2) ->
      Iff (((substitution f1 var fbool), (substitution f2 var fbool)))
  in
  let rec calc formula = 
    match formula with
    | Var x -> raise (Failure "cansnot calc variable ")
    | True -> True
    | False -> False
    | Neg x -> if (calc x) != True then True else False
    | And (x,y) -> if (calc x)=True && (calc y)=True then True else False
    | Or (x,y) -> if (calc x)=True || (calc y)=True then  True else False
    | Imply (x,y) -> if (calc x)=True && (calc y)=False then False else True
    | Iff (x,y) -> if  (calc x)=(calc y) then True else False
  in
  let rec satisfierWithVar formula vars =
    match vars with
    | x::xs -> if (satisfierWithVar (substitution formula x True) xs) ||
                  (satisfierWithVar (substitution formula x False) xs)
               then true 
               else false
    | [] -> if (calc formula) = True
            then true
            else false
  in
  satisfierWithVar f (formulaToVar f)

type aexp =
    | Const of int
    | Var of string
    | Power of string * int
    | Times of aexp list
    | Sum of aexp list

let rec diff : aexp * string -> aexp 
= fun (exp, var) ->
  match exp with
  | Const i -> Const 0
  | Var x -> if x=var then Const 1 else Const 0
  | Power (_,0) -> Const 0
  | Power (x,1) -> if x=var then Const 1 else Const 0
  | Power (x,i) ->
      if x=var then Times [Const i; Power (var, (i-1))] else Const 0
  | Times (x::xs) ->
    Sum [Times [diff (x, var); Times xs]; Times [x; diff((Times xs), var)]]
  | Times [] -> Const 0
  | Sum (x::xs) ->
    Sum ((diff (x,var))::((function|(Sum x)->x|_->[]) (diff ((Sum xs), var))))
  | Sum [] -> Const 0

type exp = X
    | INT of int
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp

let rec calculator : exp -> int 
= fun exp -> 
  let rec substitute exp value =
    match exp with
    | X -> INT value 
    | INT i -> INT i
    | ADD (exp1, exp2) ->
        ADD ((substitute exp1 value), (substitute exp2 value))
    | SUB (exp1, exp2) ->
        SUB ((substitute exp1 value), (substitute exp2 value))
    | MUL (exp1, exp2) ->
        MUL ((substitute exp1 value), (substitute exp2 value))
    | DIV (exp1, exp2) ->
        DIV ((substitute exp1 value), (substitute exp2 value))
    | SIGMA (exp1, exp2, exp3) -> SIGMA(exp1, exp2, exp3)
  in
  let rec sigma f a b =
    if b < a
    then 0
    else (f a) + (sigma f (a+1) b)
  in
  match exp with
  | X -> raise (Failure "cannot calculate exp with variable")
  | INT i -> i
  | ADD (exp1, exp2) -> (calculator exp1) + (calculator exp2)
  | SUB (exp1, exp2) -> (calculator exp1) - (calculator exp2)
  | MUL (exp1, exp2) -> (calculator exp1) * (calculator exp2)
  | DIV (exp1, exp2) -> (calculator exp1) / (calculator exp2)
  | SIGMA (exp1, exp2, exp3) ->
    let eval = (function x -> calculator (substitute exp3 x)) in           
    sigma eval (calculator exp1) (calculator exp2)

type mobile = branch * branch 
    and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
    and length = int
    and weight = int

let balanced : mobile -> bool
= fun m ->
  let rec balanced_ (branch1, branch2) = 
    let lengthOfBranch branch = 
      match branch with
      | SimpleBranch (l,_) -> l
      | CompoundBranch (l,_) -> l 
    in
    let rec weightOfBranch branch = 
      match branch with
      | SimpleBranch (_,w) -> w
      | CompoundBranch (_,m) -> weightOfMobile m 
    and weightOfMobile (branch1, branch2) = 
      (weightOfBranch branch1) +
      (weightOfBranch branch2) 
    in
    let torqueOfBranch branch = 
      (lengthOfBranch branch) *
      (weightOfBranch branch) 
    in
      (balancedBranch branch1) &&
      (balancedBranch branch2) && 
      (torqueOfBranch branch1) = (torqueOfBranch branch2)
  and balancedBranch branch =
    match branch with
    | SimpleBranch _ -> true
    | CompoundBranch (l,m) -> balanced_ m
  in 
  balanced_ m

type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin
= fun bin1 bin2 ->
  let rec last l = 
    match l with
    | [x] -> x
    | x::xs -> last xs
    | [] -> raise (Failure "empty list input on last<func>")
  in
  let rec init l = 
    match l with
    | [x] -> []
    | x::xs -> [x]@(init xs)
    | [] -> raise (Failure "empty list input on init<func>")
  in
  let rec length l = 
    match l with
    | _::xs -> 1 + (length xs)
    | _ -> 0
  in
  let addDigits x = 
    match x with
    | (ZERO, ZERO, ZERO) -> (ZERO, ZERO)
    | (ZERO, ZERO, ONE) -> (ONE, ZERO)
    | (ZERO, ONE, ZERO) -> (ONE, ZERO)
    | (ONE, ZERO, ZERO) -> (ONE, ZERO)
    | (ZERO, ONE, ONE) -> (ZERO, ONE)
    | (ONE, ZERO, ONE) -> (ZERO, ONE)
    | (ONE, ONE, ZERO) -> (ZERO, ONE)
    | (ONE, ONE, ONE) -> (ONE, ONE)
  in
  let lengthCheck l = if l=[] then false else true
  in
  let rec zeros n = if n=0 then [] else ZERO::(zeros (n-1))
  in
  let rec bsum bin1 bin2 x = 
    if (lengthCheck bin1) && (lengthCheck bin2) 
    then let k = addDigits (last bin1, last bin2, x)
         in (bsum (init bin1) (init bin2) (snd k))@[fst k]
    else if x=ONE
         then bsum (bin1@bin2) [ONE] ZERO
         else bin1@bin2
  in
  let rec bmul_ bin1 bin2 n bin3 = 
    match bin2 with
    | ONE::xs -> bmul_ bin1 xs (n-1) (bsum bin3 (bin1@(zeros (n-1))) ZERO)
    | ZERO::xs -> bmul_ bin1 xs (n-1) bin3
    | [] -> bin3
  in
  bmul_ bin1 bin2 (length bin2) [ZERO]

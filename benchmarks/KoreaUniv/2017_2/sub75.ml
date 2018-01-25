(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror 
= fun t ->
match t with
| Empty -> Empty
| Node(n,Empty,Empty) -> Node(n, Empty, Empty)
| Node(n,Empty,r) -> Node(n,(mirror r), Empty)
| Node(n,l,Empty) -> Node(n,Empty,(mirror l))
| Node(n,l,r) -> Node(n,(mirror r),(mirror l));;

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec num n = 
match n with
| ZERO -> 0
| SUCC ZERO -> 1
| SUCC a -> 1+(num a);;

let rec make n =
match n with
| 0 -> ZERO
| 1 -> SUCC ZERO
| _ -> SUCC(make (n-1));;

let rec exp n1 n2 = 
match n1 with
| 0 -> 0
| 1 -> 1
| _ ->
if n2 = 0 then 1 else if n2 = 1 then n1 else n1*(exp n1 (n2-1));;

let natadd : nat -> nat -> nat 
= fun n1 n2 -> make ((num n1)+(num n2));;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> make ((num n1)*(num n2));;

let natexp : nat -> nat -> nat 
= fun n1 n2 -> make (exp (num n1) (num n2));;

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
let rec testa f = 
match f with
| True -> True
| False -> False
| Var _ -> f
| Neg(a) -> (match a with
  | True -> False
  | False -> True
  | Var _ -> Neg(a)
  | _ -> Neg(testa a) )
| And(h,t) ->
let f1 = testa h in
let f2 = testa t in
(match f1, f2 with
    | True, True -> True
    | True, False -> False
    | True, f2 -> f2
    | False, _ -> False
    | f1, True -> f1
    | f1, False -> False
    | f1, f2 -> if f2 = Neg(f1) then False 
    else f1)
| Or(h,t) ->
let f1 = testa h in
let f2 = testa t in
(match f1, f2 with
 | True, _ -> True
 | False, True -> True
 | False, False -> False
 | False, f2 -> f2
 | f1, True -> True
 | f1, False -> f1
 | f1, f2 -> if f2 = Neg(f1) then True
 else f1)
| Imply(h,t) ->
let f1 = testa h in
let f2 = testa t in
(match f1, f2 with
 | True, True -> True
 | True, False -> False
 | True, f2 -> f2
 | False, _ -> True
 | f1, True -> True
 | f1, False -> f1
 | f1, f2 -> if f2 = f1 then True
 else f1)
| Iff(h,t) ->
let f1 = testa h in
let f2 = testa t in
(match f1, f2 with
 | True, True -> True
 | True, False -> False
 | True, f2 -> f2
 | False, True -> False
 | False, False -> True
 | False, f2 -> f2
 | f1, True -> f1
 | f1, False -> f1
 | f1, f2 -> if f2 = f1 then True else if f2 = Neg(f1) then False 
 else f1);;

let sat : formula -> bool
= fun f ->
match f with
| True | False | Var _ -> false
| _ -> 
let a = testa f in
(match a with
 | True | False -> false
 | _ -> true);;

(*
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
| Const i -> Times []
| Var s -> Const 1
| Power(s,i) -> 
if s = x then Times[Const i; Power (s,(i-1))]
else Power(s,i)
| Times l ->
(match l with
 | [] -> Times []
 | hd::tl -> 
 (match hd with
  | Const n -> hd
  | _ -> Times ([diff (hd,x)]@tl)))
| Sum l ->
(match l with
 | [] -> Const 0
 | hd::tl -> Sum ([diff (hd,x)]@([diff (Sum tl, x)])));;
*)

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec test a e
= match e with
| INT n -> n
| X -> a
| ADD(e1,e2) ->
(let v1 = test a e1 in
 let v2 = test a e2 in (v1+v2))
| SUB(e1,e2) ->
(let v1 = test a e1 in
 let v2 = test a e2 in (v1-v2))
| MUL(e1,e2) ->
(let v1 = test a e1 in
 let v2 = test a e2 in (v1*v2))
| DIV(e1,e2) ->
(let v1 = test a e1 in
 let v2 = test a e2 in (v1/v2))
| SIGMA(e1,e2,e3) ->
(let v1 = test a e1 in
 let v2 = test a e2 in
 (if v1 = v2 then test v1 e3
  else ((test v1 e3)+ (test a (SIGMA(INT(v1+1), INT v2, e3))))));;

let rec calculator
= fun e ->
match e with
| INT n -> n
| X -> raise (Failure "Not value")
| ADD(e1,e2) -> 
(match e1, e2 with
 | INT n1, INT n2 -> (n1+n2)
 | _, _ -> let v1 = calculator e1 in let v2 = calculator e2 in
 (v1+v2))
| SUB(e1,e2) -> 
(match e1, e2 with
 | INT n1, INT n2 ->(n1-n2)
 | _, _ -> let v1 = calculator e1 in let v2 = calculator e2 in
 (v1-v2))
| MUL(e1,e2) ->
(match e1, e2 with
 | INT n1, INT n2 ->(n1*n2)
 | _,_ -> let v1 = calculator e1 in let v2 = calculator e2 in
 (v1*v2))
| DIV(e1, e2) ->
(match e1, e2 with
 | INT n1, INT n2 ->(n1/n2)
 | _, _ -> let v1 = calculator e1 in let v2 = calculator e2 in
 (v1/v2))
| SIGMA(e1,e2,e3) ->
let v1 = calculator e1 in
(test v1 e);;

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec one b
= match b with
| SimpleBranch(l,w) -> w
| CompoundBranch(l,m) ->
(match m with
 | (b1,b2) -> (one b1)+(one b2));;

let rec two b
 = match b with
 | SimpleBranch(l,w) -> l
 | CompoundBranch(l,m) ->
 (match m with
  | (b1,b2) -> (two b1)+(two b2));;

let balanced : mobile -> bool
= fun m ->
match m with
| (b1,b2) -> 
if ((one b1)*(two b1)) = ((one b2)*(two b2)) then true else false

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec make a =
match a with
| 0 -> [ZERO]
| 1 -> [ONE]
| _ -> (make (a/2))@(make (a mod 2));;

let rec length l = 
match l with
| [] -> 0
| hd::tl -> 1+(length tl);;

let rec expt b n =
if n = 0 then 1
else b*(expt b (n-1));;

let rec solve lst n
= match lst with
| [] -> 0
| hd::tl ->
if hd = ONE then (expt 2 (n-1))+(solve tl (n-1))
  else (solve tl (n-1));;

let bmul : bin -> bin -> bin
= fun b1 b2 -> make((solve b1 (length b1))*(solve b2 (length b2)));;

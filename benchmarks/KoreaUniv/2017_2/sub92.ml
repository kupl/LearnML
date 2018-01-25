(*problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree = fun t ->
match t  with
|Empty -> Empty
|Node(n, btree1, btree2) -> Node(n, mirror btree2, mirror btree1);;


(*problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat = fun n1 n2 ->
match n1 with
|ZERO -> n2
|SUCC(nat) -> SUCC(natadd nat n2);;

let rec natmul : nat -> nat -> nat = fun n1 n2 ->
match n2 with
|ZERO -> ZERO
|SUCC(nat) -> natadd n1 (natmul n1 nat);;

let rec natexp : nat -> nat -> nat = fun n1 n2 ->
match n2 with
|ZERO -> SUCC ZERO
|SUCC(nat) -> natmul n1 (natexp n1 nat);;


(*problem 3*)
type formula = 
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec form : formula -> formula = fun f ->
match f with
|True -> True
|False -> False
|Var x -> Var x
|Neg f1 -> 
  (match f1 with
  |Neg f2 -> form f2
  |_ -> if (form f1 = True) then False else if (form f1 = False) then True else Neg (form f1))
|And (f1,f2) ->
  if (form f1 = form (Neg f2)) then False
  else if (form f1 = False && form f2 = False) then False
  else True
|Or (f1,f2) -> if (form f1 = False && form f2 = False) then False else True
|Imply (f1,f2) -> if (form f1 = True && form f2 = False) then False else True
|Iff (f1,f2) -> if (form f1 = form f2) then True else False;;
  
let rec sat : formula -> bool = fun f ->
match f with
|True -> true
|False -> false
|Var x -> true
|_ -> sat (form f);;

(*problem 4*)
type aexp =
| Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff : aexp * string -> aexp = fun (e,x) ->
match e with
|Const i -> Const 0
|Var s ->
  if x=s then Const 1 else Const 0
|Power (s,n) ->
  if x=s then match n with
  |1 -> Const 1
  |0 -> Const 0
  |_ -> Times[Const n; Power (x,n-1)]
  else Const 0
|Times(l) ->
  (match l with
  |[] -> Const 0
  |hd::tl -> Sum [Times ([diff(hd, x)] @ tl); Times (hd::[diff(Times tl, x)])])
|Sum l -> 
  (match l with
  |[] -> Const 0
  |hd::tl -> Sum[diff (hd,x);diff (Sum tl,x)])


(*problem 5*)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let e = SIGMA(INT 1, INT 5, ADD(X,X));;

let rec calculator : exp -> int = fun e ->
match e with
|X -> calculator X
|INT n -> n
|ADD (e1,e2) -> ((calculator e1) + (calculator e2))
|SUB (e1,e2) -> ((calculator e1) - (calculator e2))
|MUL (e1,e2) -> ((calculator e1) * (calculator e2))
|DIV (e1,e2) -> ((calculator e1) / (calculator e2))
|SIGMA (e1,e2,e3) -> 
  (match e1,e2, e3 with
  |INT n1, INT n2, _ -> 
    if(n1<n2+1) then 
      (match e3 with 
      |X -> n1 + calculator(SIGMA(INT (n1+1), e2, e3))
      |INT n -> n + calculator(SIGMA(INT (n1+1), e2, e3))
      |ADD (e4,e5) -> calculator(SIGMA(e1, e1, e4)) + calculator(SIGMA(e1, e1, e5)) + calculator(SIGMA(INT (n1+1), e2, e3))
      |SUB (e4,e5) -> calculator(SIGMA(e1, e1, e4)) - calculator(SIGMA(e1, e1, e5)) + calculator(SIGMA(INT (n1+1), e2, e3))
      |MUL (e4,e5) -> calculator(SIGMA(e1, e1, e4)) * calculator(SIGMA(e1, e1, e5)) + calculator(SIGMA(INT (n1+1), e2, e3))
      |DIV (e4,e5) -> calculator(SIGMA(e1, e1, e4)) / calculator(SIGMA(e1, e1, e5)) + calculator(SIGMA(INT (n1+1), e2, e3))
      |SIGMA (e4,e5,e6) -> raise (Failure ("type error")))
   else 0
  |_ -> raise (Failure ("type error")))



(*problem 6*)
type mobile = branch * branch  (* left and rigth branches *)
and branch = SimpleBranch of length * weight
            | CompoundBranch of length * mobile
and length = int
and weight = int

let rec weight : mobile -> int = fun m ->
match m with
|(SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> (w1 + w2)
|(SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> (w1 + (weight m2))
|(CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> (weight m1) + w2
|(CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> (weight m1) + (weight m2);;

let rec balanced : mobile -> bool = fun m ->
match m with
|(SimpleBranch (l1, w1), SimpleBranch(l2, w2)) -> if l1*w1 = l2*w2 then true else false
|(SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> if (l1*w1 = l2*(weight m2)) && (balanced m2) then true else false
|(CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> if (l2*w2 = l1*(weight m1)) && (balanced m1) then true else false
|(CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> if (l1*(weight m1) = l2*(weight m2)) && (balanced m1) && (balanced m2) then true else false


(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec length : bin -> int = fun b ->
match b with
|[] -> 0
|hd::tl -> 1 + (length tl);;

let rec power : int -> int = fun n ->
if n = 0 then 1
else 2 * power (n-1);;

let rec btod : bin -> int = fun b ->
match b with
|[] -> 0
|[ONE] -> 1
|[ZERO] -> 0
|ONE::tl -> (power ((length b)-1)) + (btod tl)
|ZERO::tl -> btod tl;;

let rec dtob : int -> bin = fun n ->
if n = 0 then [ZERO]
else if n = 1 then [ONE]
else if (n mod 2 = 1) then (dtob (n/2)) @ [ONE]
else (dtob (n/2)) @ [ZERO];;

let bmul : bin -> bin -> bin = fun b1 b2 ->
dtob ((btod b1) * (btod b2));;

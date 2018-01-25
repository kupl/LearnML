(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> let rec mr t = match t 
with Empty -> Empty
| Node(a, b, c)-> Node(a, mr c, mr b)
in mr t


(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> let rec add c1 c2 =
match c1 
with ZERO -> (match c2
	with ZERO -> ZERO 
	| SUCC(a) -> SUCC( add c1 a ))
| SUCC(b) -> SUCC( add b c2 )
in add n1 n2 

let natmul : nat -> nat -> nat 
= fun n1 n2 -> let rec mul c2 = 
match c2 
with ZERO -> ZERO
| SUCC( a) -> natadd n1 (mul a) 
in mul n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let rec exp c2 =
match c2
with ZERO -> SUCC(ZERO)
| SUCC( a) -> natmul n1 (exp a)
in exp n2

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
= fun f -> 
let l = 
let rec findVars f l = match f
with Var x -> if ( let rec findVar k = match k
with hd::tl -> if (hd = x) then true else findVar tl
| [] -> false
in findVar l ) then l 
else x::l
| Neg a -> findVars a l
| And (a,b) ->  findVars b (findVars a l)
| Or (a,b) ->  findVars b (findVars a l)
| Imply (a,b) ->  findVars b (findVars a l)
| Iff (a,b) -> findVars b (findVars a l)
| _ -> []
in findVars f []
in let rec logic f cl = match f
with Var x -> (let rec findVal cl = match cl
with hd::tl -> (match hd 
with (a,b) -> if (a = x) then b else findVal tl)
| [] -> false
in findVal cl)
| Neg x -> not (logic x cl)
| And (a, b) -> (logic a cl) && (logic b cl)
| Or (a, b) -> (logic a cl) || (logic b cl)
| Imply (a, b) -> (not (logic a cl)) || (logic b cl)
| Iff (a, b) -> let iff p q = (not p || q) && (not q || p) in iff (logic a cl)  (logic b cl)
| False -> false
| True -> true
in let rec table l cl = match l
with hd::tl -> (table tl ((hd, true)::cl)) || (table tl ((hd, false)::cl))
| [] -> logic f cl
in table l []

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> let rec d exp isTimesWithX = match exp 

with Sum l -> let rec iter tmp = match tmp
with [] -> []
| hd::tl -> (d hd false)::(iter tl)
in Sum (iter l)

| Times l -> let tmpBool =
let rec iter tmp = match tmp
with [] -> false
| hd::tl -> match hd with 
| Var a -> if (a = x) then true else iter tl
| Power (a,b) -> if (a = x) then (if (b = 0) then iter tl else true) else iter tl 
| _ -> iter tl
in iter l
in let rec iter tmp = match tmp
with [] -> []
| hd::tl -> (d hd tmpBool)::(iter tl)
in Times (iter l)

| Power (a,b) -> if (a = x) then 
if (b = 1) then (Const 1)
else if (b = 0) then if isTimesWithX then Const 1 else Const 0
else Times [(Const b); (Power (a,(b-1)))]
else if isTimesWithX then (Power (a,b))
else Const 0

| Var a -> if (a = x) then (Const 1)
else if isTimesWithX then Var a
else Const 0

| Const a -> if isTimesWithX then Const a
else Const 0

in d e false

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let rec realcal e c = match e
with X -> c
| INT x -> x
| ADD (x, y) -> (realcal x c) + (realcal y c)
| SUB (x, y) -> (realcal x c) - (realcal y c)
| MUL (x, y) -> (realcal x c) * (realcal y c)
| DIV (x, y) -> (realcal x c) / (realcal y c)
| SIGMA (x, y, z) -> let b = realcal x c 
in let t = realcal y c 
in let rec sigma counter = if counter > t then 0 else (realcal z counter) + sigma (counter + 1)
in sigma b
in realcal e 0

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> let rec sum mo = match mo
with (l,r) -> match l 
with SimpleBranch (xl, wl) -> ( match r 
with SimpleBranch (xr, wr) -> wl + wr
| CompoundBranch (lr, mr) -> wl + (sum mr))
| CompoundBranch (ll, ml) -> ( match r
with SimpleBranch (xr, wr) -> wr + (sum ml)
| CompoundBranch (lr, mr) -> (sum ml) + (sum mr))

in let rec bal m = match m
with (l,r) -> match l
with SimpleBranch (xl, wl) -> ( match r 
with SimpleBranch (xr, wr) -> xl * wl = xr * wr
| CompoundBranch (lr, mr) -> (xl * wl = lr * (sum mr)) && (bal mr))
| CompoundBranch (ll, ml) -> ( match r
with SimpleBranch (xr, wr) -> (xr * wr = ll * (sum ml)) && (bal ml)
| CompoundBranch (lr, mr) -> (bal ml) && (bal mr) && 
((sum ml)*ll ==  (sum mr)*lr) )

in bal m

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> if (b1 = [ZERO] || b2 = [ZERO]) then [ZERO]
else let rec bintodec b e = match b 
with hd::tl -> (match hd
with ZERO -> 0 * e + (bintodec tl e/2)
| ONE -> 1 * e + (bintodec tl e/2))
| [] -> 0
in let rec dectobin d =
if ( d = 0 ) then []
else (dectobin (d/2))@(if d mod 2 = 0 then [ZERO] else [ONE])
in dectobin ( (bintodec b1 (1 lsl ((List.length b1)-1)))*(bintodec b2 (1 lsl ((List.length b2)-1))) )

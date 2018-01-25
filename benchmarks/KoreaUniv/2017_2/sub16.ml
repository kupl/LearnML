(*Problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
  Empty -> Empty
  | Node(x, left, right) -> Node(x, mirror right, mirror left)

(*Problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
ZERO -> n2
| SUCC(x) -> SUCC( natadd x n2 )

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
ZERO -> ZERO
| SUCC(x) -> natadd n2 (natmul x n2) 

let rec natexp : nat -> nat -> nat
= fun n1 n2 -> match n2 with
ZERO -> SUCC(ZERO)
| SUCC(x) -> natmul n1 (natexp n1 x)

(*Problem 3*)
type formula =
  True
| False
| Var of string
| Neg of formula
| And of formula * formula
| Or of formula * formula
| Imply of formula * formula
| Iff of formula * formula

let rec ftbt : formula -> bool
 = fun f -> match f with
|True -> true
|False -> false
|Var string -> true
|Neg f -> not (ftbt f)
|And (f1, f2) -> (ftbt f1) && (ftbt f2)
|Or (f1, f2) -> (ftbt f1) || (ftbt f1)
|Imply (f1, f2) -> if ((ftbt f1) = false) then true else ftbt f2
|Iff (f1, f2) -> if ((ftbt f1) = (ftbt f2)) then true else false

let rec ftbf : formula -> bool
= fun f ->match f with
|True -> true
|False -> false
|Var string -> false
|Neg f -> not (ftbf f)
|And (f1, f2) -> (ftbf f1) && (ftbf f2)
|Or (f1, f2) -> (ftbf f1) || (ftbf f1)
|Imply (f1, f2) -> if ((ftbf f1) = false) then true else ftbf f2
|Iff (f1, f2) -> if ((ftbf f1) = (ftbf f2)) then true else false

let rec ftbtf : formula -> bool
= fun f ->match f with
|True -> true
|False -> false
|Var string -> true
|Neg f -> not (ftbtf f)
|And (f1, f2) -> (ftbt f1) && (ftbf f2)
|Or (f1, f2) -> (ftbt f1) || (ftbf f1)
|Imply (f1, f2) -> if ((ftbt f1) = false) then true else ftbf f2
|Iff (f1, f2) -> if ((ftbt f1) = (ftbf f2)) then true else false
 
let rec ftbft : formula -> bool
= fun f ->match f with
|True -> true
|False -> false
|Var string -> true
|Neg f -> not (ftbft f)
|And (f1, f2) -> (ftbf f1) && (ftbt f2)
|Or (f1, f2) -> (ftbf f1) || (ftbt f1)
|Imply (f1, f2) -> if ((ftbf f1) = false) then true else ftbf f2
|Iff (f1, f2) -> if ((ftbf f1) = (ftbt f2)) then true else false 

let rec getStringfront : formula -> string
= fun f ->match f with
|True -> "True"
|False -> "False"
|Var string -> string
|Neg f -> getStringfront f
|And (f1, f2) -> getStringfront f1
|Or (f1, f2) -> getStringfront f1
|Imply (f1, f2) -> getStringfront f1
|Iff (f1, f2) -> getStringfront f1

let rec getStringlast : formula -> string
 = fun f ->match f with
|True -> "True"
|False -> "False"
|Var string -> string
|Neg f -> getStringlast f
|And (f1, f2) -> getStringlast f2
|Or (f1, f2) -> getStringlast f2
|Imply (f1, f2) -> getStringlast f2
|Iff (f1, f2) -> getStringlast f2




let sat : formula -> bool
= fun f -> if(getStringfront f = getStringlast f) then (if (ftbt f = false && ftbf f = false) then false else true)
  else (if (ftbt f = false && ftbf f = false && ftbtf f = false && ftbft f = false) then false
else true)


(*Problem 4*)
type aexp =
|Const of int
|Var of string
|Power of string * int
|Times of aexp list
|Sum of aexp list


let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
|Sum (lst) -> (match lst with
  |[]->Const 0
  |hd::tl -> if(tl = []) then diff (hd,x) else Sum([diff (hd,x)] @ [diff (Sum tl,x)])
  )
|Const int -> Const 0
|Var string -> if(string = x) then Const 1 else Const 0
|Power(string, int) -> if(string = x) then Times [Const int; Power(x, (int-1))] 
else Const 0
|Times (lst) -> match lst with
  |[] -> Const 0
  |hd::tl -> Times([hd] @ [diff (Sum tl,x)])




(*Problem 5*)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp


let rec calc : exp -> int -> int
= fun e n -> match e with
|X -> n
|INT int -> int
|ADD(a, b) -> (calc a n) + (calc b n)
|SUB(a, b) -> (calc a n) - (calc b n)
|MUL(a, b) -> (calc a n) * (calc b n)
|DIV(a, b) -> (calc a n) / (calc b n)
|SIGMA(a, b, c) -> raise (Failure "Double SIGMA")

let rec calculator : exp -> int
= fun e -> match e with
|X -> raise (Failure "Wrong input")
|INT int -> int
|ADD(a,b) -> calculator a + calculator b
|SUB(a,b) -> calculator a - calculator b
|MUL(a,b) -> calculator a * calculator b
|DIV(a,b) -> calculator a / calculator b
|SIGMA(a, b, c) -> if((calculator a) > (calculator b)) then 0
else calc c (calculator a) + calculator(SIGMA(INT((calculator a)+1),b,c))


(*Problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
            |CompoundBranch of length * mobile
and length = int
and weight = int

let rec weight : mobile -> int
= fun m -> match m with
(lb,rb) ->
match lb with
|SimpleBranch(l1,w1) -> (match rb with
    |SimpleBranch(l2,w2)-> w1 + w2
    |CompoundBranch(l2,m2) -> w1 + (weight m2)
    )
|CompoundBranch(l3,m3) -> match rb with
    |SimpleBranch(l4,w4) -> (weight m3) + w4
    |CompoundBranch(l4,m4) -> (weight m3) + (weight m4)



let rec balanced : mobile -> bool
= fun m -> match m with
(lb,rb) -> 
match lb with
|SimpleBranch(l1,w1) -> (match rb with
    |SimpleBranch(l2,w2) -> if(l1*w1 = l2*w2) then true else false
    |CompoundBranch(l2,m2) -> if((balanced m2) = true) then (if(l1*w1 = l2*(weight m2)) then true else false) else false
    )

|CompoundBranch(l3,m3) -> if((balanced m3) = true) then (match rb with
    |SimpleBranch(l4,w4) -> if(l4*w4 = l3*(weight m3)) then true else false
    |CompoundBranch(l4,m4) -> if((balanced m4) = true) then (if(l3*(weight m3) = l4*(weight m4)) then true else false) else false
    )
  else false


(*Problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec expt : int -> int -> int
= fun a n -> if (n = 0) then 1
  else (if (n mod 2 = 0) then (expt a (n/2))*(expt a (n/2)) else (if (n=1) then a else a*(expt a (n-1))))


let rec bitode : bin -> int
= fun b-> let len = List.length(b) in
match b with
    |[]->0
    |hd::tl -> if(hd=ZERO) then bitode tl 
      else if(hd=ONE) then (expt 2 (len-1)) + bitode tl
      else 0
  

let rec detobi : int -> bin
= fun d -> if (d = 0) then [ZERO] else if (d = 1) then [ONE]
else if ( d mod 2 = 1 ) then (detobi (d/2))@[ONE]
else (detobi (d/2))@[ZERO]



let bmul : bin -> bin -> bin
= fun b1 b2 -> detobi (bitode b1 * bitode b2)


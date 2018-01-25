(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
|Node(i,x,y) -> Node(i, mirror y, mirror x)
|_ -> Empty


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
|SUCC(nat) -> SUCC(natadd nat n2)
|ZERO -> n2

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
|SUCC(nat) -> natadd n2 (natmul nat n2)
|ZERO -> ZERO

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
|SUCC(ZERO) -> n1
|ZERO -> ZERO
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

let rec sat : formula -> bool
= fun f -> let rec length lst = match lst with
|hd::tl -> length tl + 1 
|[] -> 0 in 
 let rec findvar f = match f with
 |True -> []
 |False -> []
 |Var(x) -> [x]
 |Neg(a) -> findvar(a)
 |And(a,b) -> findvar(a) @ findvar(b)
 |Or(a,b) -> findvar(a) @ findvar(b)
 |Imply(a,b) -> findvar(a) @ findvar(b)
 |Iff(a,b) -> findvar(a) @ findvar(b) in 
 let rec exp n = match n with
  |0 -> 1
  |1 ->2
  |_ -> 2 * exp(n-1) in
   let rec maketup lst n = match lst with
   |hd::tl -> if (n/2) = 0 then [(hd,true)] else
   (match (n mod 2) with
    |1 -> [(hd, true)] @ maketup tl (n/2)
    |0 -> [(hd, false)] @ maketup tl (n/2)
    |_ -> [(hd, false)])
   |_-> [] in
    let rec tup f lst = match lst with
    |hd::tl -> (match hd with
      |(a,b) -> match f with
       |Var(x) -> if a = x then b else tup f tl
       |_ -> tup f tl)
    |_ -> false in
     let rec fm f lst = match f with
     |True -> true
     |False -> false
     |Var(x) -> tup (Var(x)) lst
     |Neg(x) -> if (fm x lst) = true then false else true
     |And(a,b) -> (fm a lst) && (fm b lst)
     |Or(a,b) -> (fm a lst) || (fm b lst)
     |Imply(a,b) -> (fm (Neg(a)) lst) || (fm b lst)
     |Iff(a,b) -> if (fm a lst) = (fm b lst ) then true else false in
     let lst = findvar(f) in let n = exp(length(lst)) in
      if (let rec sum f n1 n2 = if fm f (maketup lst n2) = true then 1 else if (n1 = n2) then 0 else sum f n1 (n2+1) in sum f n 0) = 1 then true else false

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
|Const(i) -> Const(0)
|Var(a) -> if a = x then Const(1) else Const(0)
|Power(a,i) -> if a = x then (match i with
  |1 ->Const(i)
  |2 ->Times[Const(i);Var(a)]
  |_ ->Times[Const(i);Power(a,(i-1))]) else Const(0)
|Times list -> if(let rec search list x = match list with
  |hd::tl -> (match hd with
    |Power(a,i) -> 1 + search tl x
    |Var(a) -> if a = x then 1 else 0 + search tl x
    |_ -> search tl x)
  |_ -> 0 in search list x) = 0 then Const(0) else Times(let rec yes list x  = match list with
    |hd::tl -> (match hd with
      |Const(i) -> Const(i)::(yes tl x)
      |Var(a) -> (if a = x then diff(hd,x) else hd)::(yes tl x)
      |Power(a,i) -> (if a = x then diff(hd,x) else hd)::(yes tl x)
      |_-> diff(hd,x)::(yes tl x))
    |_ -> [] in yes list x)
|Sum list -> Sum(let rec sumdiff list x = match list with
  |hd::tl -> diff(hd,x)::(sumdiff tl x)
  |[] -> [] in sumdiff list x)


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> let c = ref 0 in let rec calf fx =
match fx with
|X -> !c
|INT(i) -> i
|ADD(a,b) -> (calf a) + (calf b)
|SUB(a,b) -> (calf a) - (calf b)
|MUL(a,b) -> (calf a) * (calf b)
|DIV(a,b) -> (calf a) / (calf b)
|SIGMA(n1,n2,x) -> let rec sum(n1,n2,x) = c:=n1; if n1<=n2 then sum(n1+1,n2,x) + calf x else 0 in sum(calf n1, calf n2, x) in calf e


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> let rec br(r,p,d) = match r with
|SimpleBranch(l,w) -> w * (l * d + p)
|CompoundBranch(l,m) -> (match m with
  |(a,b) -> (br(a,(p+l*d),-1) + br(b,(p+l*d),1))) in
if (let rec mob mb = match mb with
  |(a,b) -> br(a,0,-1) + br(b,0,1) in mob m)=0 then true else false


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> let rec rev l = match l with
|hd::tl -> (rev tl)@[hd]
|_ -> [] in let rec bindec b1 = match b1 with
|hd::tl -> (match hd with
  |ZERO -> 0 + (bindec tl) * 2
  |ONE -> 1 + (bindec tl) * 2 )
|_ -> 0 in let rec decbin d1 = if(d1/2) = 0 then [ONE] else
(match (d1 mod 2) with
 |1 -> (decbin (d1/2))@[ONE]
 |0 -> (decbin (d1/2))@[ZERO]
 |_->[ZERO]) in decbin(bindec(rev(b1)) * bindec(rev(b2)))

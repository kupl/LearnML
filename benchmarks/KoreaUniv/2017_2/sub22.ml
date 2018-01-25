(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
| Empty -> Empty
| Node(i,l,r) -> Node(i,(mirror r),(mirror l));;

(* problem 2*)
  type nat = ZERO | SUCC of nat;;

  let rec natadd : nat -> nat -> nat
  = fun a b -> match a with
  | ZERO -> b 
  | SUCC n -> natadd n (SUCC(b));;

  let natmul : nat -> nat -> nat 
  = fun a b -> let c=b in
  let rec natmulrec = fun t s u -> match t with
  | ZERO -> ZERO
  | SUCC n -> match n with
    | ZERO -> s
    | SUCC n -> natmulrec (SUCC(n)) (natadd s c) c in
    natmulrec a b c;;

  let natexp : nat -> nat -> nat
  = fun a b -> let c=a in
  let rec natexprec = fun t s u -> match s with
  | ZERO -> SUCC ZERO
  | SUCC n -> match n with
    | ZERO -> t 
    | SUCC n -> natexprec (natmul t u) (SUCC n) u in
    natexprec a b c;;

(*problem 3*)
  type formula =
    True
    |False
    |Var of string
    |Neg of formula
    |And of formula * formula
    |Or of formula * formula
    |Imply of formula * formula
    |Iff of formula * formula
  exception Problem;;

  let sat : formula -> bool
  = fun f ->
  let rec repele = fun lst x -> match lst with
  | [] -> false
  | hd::tl -> if x=hd then true else repele tl x in
  let rec arrange = fun lst -> match lst with
    | [] -> []
    | hd::tl -> if (repele tl hd)=false then hd::(arrange tl)
    else arrange tl in
  let rec mkinitlist
  = fun f -> match f with
    | True -> []
    | False -> []
    | Var x -> [(x,false)]
    | Neg x -> mkinitlist x
    | And (f1,f2) -> (mkinitlist f1)@(mkinitlist f2)
    | Or (f1,f2) -> (mkinitlist f1)@(mkinitlist f2)
    | Imply (f1,f2) -> (mkinitlist f1)@(mkinitlist f2)
    | Iff (f1,f2) -> (mkinitlist f1)@(mkinitlist f2) in
  let rec terminatelist = fun lst -> match lst with
  | [] -> []
  | hd::tl -> (match hd with
    | (x,_) -> ((x,true)::(terminatelist tl))) in
  let rec nextlist = fun lst -> match lst with
  | [] -> []
  | hd::tl -> (match hd with
      | (x,false) -> (x,true)::tl
      | (x,true) -> ((x,false)::(nextlist tl))) in
  let rec find = fun x lst -> match lst with
  | [] -> raise Problem
  | hd::tl -> (match hd with
    | (a,true) -> if a=x then true else find x tl
    | (a,false) -> if a=x then false else find x tl) in
  let rec eval = fun f lst -> match f with
  | True -> true
  | False -> false
  | Var x -> find x lst
  | Neg f1 -> not(eval f1 lst)
  | And (f1,f2) -> (eval f1 lst)&&(eval f2 lst)
  | Or (f1,f2) -> (eval f1 lst)||(eval f2 lst)
  | Imply(f1,f2) -> eval (Or(Neg(f1),f2)) lst
  | Iff(f1,f2) -> eval(Or(And(Neg(f1),Neg(f2)),And(f1,f2))) lst in
  let rec newsat = fun f lst -> if (eval f lst)=true then true
  else if lst=(terminatelist lst) then false else newsat f (nextlist lst) in
  newsat f (arrange (mkinitlist f));;

(* problem 4*)
  type aexp=
  | Const of int
  | Var of string
  | Power of string*int
  | Times of aexp list
  | Sum of aexp list

 let rec diff : aexp * string -> aexp (*istherex*)
  = fun (e,x) -> 
  let rec timediff = fun (tlst,x) -> match tlst with
  | [] -> []
  | hd::tl -> (match hd with
      | Const c -> (Const c)::timediff(tl,x)
      | Var v -> if v=x then (Const 1)::timediff(tl,x) else (Const 0)::timediff(tl,x)
      | Power (v,c) -> if v=x then (Times[Const c;Power(v,c-1)])::timediff(tl,x) else (Const 0)::timediff(tl,x)) in
  let rec sumdiff = fun (slst,x) -> match slst with
  | [] -> []
  | hd::tl -> (match hd with
      | Const c -> (Const 0)::sumdiff(tl,x)
      | Var v -> if v=x then (Const 1)::sumdiff(tl,x) else (Const 0)::sumdiff(tl,x)
      | Power (v,c) -> if v=x then (Times[Const c;Power(v,c-1)])::sumdiff(tl,x) else (Const 0)::sumdiff(tl,x)
      | Sum lst -> Sum(sumdiff (lst,x))::sumdiff(tl,x)
      | Times lst -> Times(timediff (lst,x))::sumdiff(tl,x)) in
  let rec differ = fun (e,x) -> match e with
  | Const c -> Const 0
  | Var v -> if v=x then Const 1 else Const 0
  | Power (v,c) -> if v=x then Times[Const c;Power(v,c-1)] else Const 0
  | Sum lst -> Sum(sumdiff(lst,x))
  | Times lst -> Times(timediff(lst,x)) in differ(e,x);;

  (*problem 5*)
  type exp = X
    | INT of int
    | ADD of exp*exp
    | SUB of exp*exp
    | MUL of exp*exp
    | DIV of exp*exp
    | SIGMA of exp*exp*exp

  let calculator : exp -> int
  = fun e -> let rec calc = fun (e,i) -> match e with
  | X -> i
  | INT a -> a
  | ADD (a,b) -> calc(a,i)+calc(b,i)
  | SUB (a,b) -> calc(a,i)-calc(b,i)
  | MUL (a,b) -> calc(a,i)*calc(b,i)
  | DIV (a,b) -> calc(a,i)/calc(b,i) in
  let rec calcsigma = fun (a,b,c,n) -> if b=n then calc(c,n)
  else calc(c,n)+calcsigma(a,b,c,n+1) in
  let getsigma = fun e -> match e with
  | SIGMA (INT a,INT b,c) -> calcsigma(a,b,c,a) in getsigma e;;

  (* problem 6*)
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun m -> let rec getw = fun b -> match b with
  | SimpleBranch (_,w) -> w
  | CompoundBranch (_,bc) -> (match bc with
      | (b1,b2) -> (getw(b1))+(getw(b2))) in
  let getl = fun b -> match b with
  | SimpleBranch (l,_) -> l
  | CompoundBranch (l,_) -> l in
  let rec balance = fun m -> match m with
  | (b1,b2) -> if (getl(b1)*getw(b1))=(getl(b2)*getw(b2)) then (match (b1,b2) with
  | (SimpleBranch (_,_), SimpleBranch(_,_)) -> true
  | (SimpleBranch (_,_), CompoundBranch(_,b)) -> true&&(balance b)
  | (CompoundBranch (_,b), SimpleBranch(_,_)) -> true&&(balance b)
  | (CompoundBranch (_,ba), CompoundBranch(_,bb)) -> true&&(balance ba)&&(balance bb))
  else false in balance m;;

  (*problem 7*)
  type digit = ZERO | ONE
  type bin = digit list

  let bmul
  = fun b1 b2 ->
  let rec power = fun b -> match b with
  | 0 -> 1
  | _ -> 2*(power (b-1)) in
  let rec getlen = fun b -> match b with
  | [] -> 0
  | hd::tl -> 1+ (getlen tl) in
  let rec btod = fun b -> match b with
  | [] -> 0
  | hd::tl -> (match hd with
      | ZERO -> 0+(btod tl)
      | ONE -> (power((getlen (hd::tl))-1)) + (btod tl) ) in 
  let rec dtob = fun d -> match (d mod 2) with
  | 0 -> if (d/2)=0 then [ONE] else (dtob(d/2))@[ZERO]
  | 1 -> if (d/2)=0 then [ONE] else (dtob(d/2))@[ONE] in
  (dtob((btod(b1))*(btod(b2))));;


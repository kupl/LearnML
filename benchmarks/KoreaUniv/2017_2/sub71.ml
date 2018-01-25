(* problem 1*)
type btree = Empty | Node of int * btree * btree;;

let rec mirror : btree -> btree
= fun t -> match t with
          |Empty -> t
          |Node(x,l,r) -> Node(x,(mirror r), (mirror l));;


(* problem 2*)
type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
  = fun n1 n2 ->
    match n1 with
    | ZERO -> n2
    | SUCC(x) -> (natadd x (SUCC(n2)));;

let rec natmul : nat -> nat -> nat
  = fun n1 n2 ->
    match n1 with 
    | ZERO -> ZERO
    | SUCC ZERO -> n2
    | SUCC x -> natadd (natmul x n2) n2 ;;


let rec natexp : nat -> nat -> nat
  = fun n1 n2 ->
    match n2 with
      | ZERO -> SUCC ZERO
      | SUCC ZERO -> n1
      | SUCC x -> natmul (natexp n1 x) n1;;



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
;;
type env = (string * bool) list;;
let env = ref [];;

let rec find_env x e = 
match e with
| [] -> true
| (y,v)::tl -> if x=y then false else (find_env x tl);;

let rec put_env f =
match f with
| Var s -> if (find_env s !env)=true then (env:=(s,true)::(!env))
| Neg k -> (put_env k)
| And (x,y) -> ((put_env x);(put_env y))
| Or (x,y) -> ((put_env x);(put_env y))
| Imply (x,y) -> ((put_env x);(put_env y))
| Iff (x,y) -> ((put_env x);(put_env y))
| True -> (env:=(!env))
| False -> (env:=(!env));;

let rec apply_env x e =
match e with
| [] -> true
| (y,v)::tl -> if x=y then v else (apply_env x tl);;

let rec res_form f l= 
match f with
| True -> true
| False -> false
| Var s -> if (apply_env s l)=true then true else false
| Neg k -> not(res_form k l)
| And (x,y) -> (res_form x l)&&(res_form y l)
| Or (x,y) -> (res_form x l)||(res_form y l)
| Imply (x,y) -> (res_form (Or (Neg x,y)) l)
| Iff (x,y) -> (res_form (And (Imply (x,y),Imply(y,x))) l);;

let rec chg_env f e l =
match e with
| []-> if (res_form f l)=true then true else false
| (x,y)::tl -> ((x,true)::l; if (chg_env f tl l)=true then true else false) || ((x,false)::l; if (chg_env f tl l)=true then true else false);;
let sat : formula -> bool
= fun f -> 
((put_env f);(chg_env f (!env) []));;



(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
;;
let rec diff : aexp * string -> aexp
= fun (e,x) ->
match e with
| Const n -> Const 0
| Var s -> if s=x then Const 1
           else Const 0
| Power (s,n) -> if s=x then 
                    if n=0 then Const 0
                    else Times [Const n; Power (s,n-1)]
                 else Const 0
| Times l -> begin
              match l with
              | [] -> Const 0
              | hd::tl -> Sum ((Times((diff (hd,x))::tl))::([Times((diff (Times tl,x))::[hd])]))
             end
| Sum k ->
          match k with
          | []-> Const 0
          | hd::tl -> Sum ((diff (hd,x))::([diff (Sum tl,x)]));;



(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
;;

let x=ref 0;;
let rec calculator : exp -> int
= fun e ->
match e with
| X -> !x
| INT n -> n
| ADD (a,b) -> (calculator a) + (calculator b)
| SUB (a,b) -> (calculator a) - (calculator b)
| MUL (a,b) -> (calculator a) * (calculator b)
| DIV (a,b) -> (calculator a) / (calculator b)
| SIGMA (a,b,f) -> if ((calculator a)=(calculator b)) then (x:=(calculator a); (calculator f))
                   else (x:=(calculator a); (calculator f)) + (calculator (SIGMA (INT ((calculator a)+1),b,f)));;

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int
;;

let rec compare : mobile -> int
= fun m ->
match m with
(a,b) -> match a,b with
| SimpleBranch(l,w), SimpleBranch(l2,w2) -> w+w2
| SimpleBranch(l,w), CompoundBranch(l2,m1) -> w+(compare m1)
| CompoundBranch(l,m1), SimpleBranch(l2,w2) -> w2+(compare m1)
| CompoundBranch(l,m1), CompoundBranch(l2,m2) -> (compare m1) + (compare m2);;


let rec balanced : mobile -> bool
= fun m ->
match m with
(a,b) -> match a,b with
| SimpleBranch (l,w), SimpleBranch (l2,w2) -> if (l*w)=(l2*w2) then true else false
| SimpleBranch (l,w), CompoundBranch (l2,m1) -> if (balanced m1)&&((l*w)=(l2*(compare m1))) then true else false
| CompoundBranch (l,m1), SimpleBranch (l2,w2) -> if (balanced m1)&&((l2*w2)=(l*(compare m1))) then true else false
| CompoundBranch (l,m1), CompoundBranch (l2,m2) -> if (balanced m1)&&(balanced m2)&&((compare m1)=(compare m2)) then true else false;;



(* problem 7*)
type digit = ZERO | ONE
type bin = digit list
;;

let rec getsize b = 
match b with
| []->0
| hd::tl -> 1+(getsize tl);;

let rec pow n =
match n with
| 0 -> 1
| _ -> 2*(pow (n-1));;

let rec btod b =
match b with
| []->0
| hd::tl -> (match hd with 
            | ZERO -> (btod tl)
            | ONE -> (pow ((getsize b)-1)) + (btod tl));;

let rec dtob n b =
if n=0 then b
else
  if (n mod 2)=0 then (dtob (n/2) (ZERO::b))
  else (dtob (n/2) (ONE::b));;

let bmul : bin -> bin -> bin
= fun b1 b2 ->
if (btod b1)=0 || (btod b2)=0 then [ZERO]
else dtob ((btod b1)*(btod b2)) [] ;;


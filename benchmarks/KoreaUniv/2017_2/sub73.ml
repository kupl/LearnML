(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
match t with 
|Empty -> Empty
|Node(x, left, right) ->
  if (left == Empty && right == Empty) then Node(x, left, right)
  else Node(x, mirror right, mirror left);;

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with 
|ZERO -> n2
|SUCC n1_mi -> SUCC (natadd n1_mi n2);;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC n1_mi -> natadd n2 (natmul n1_mi n2);;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
match n2 with
|ZERO -> SUCC ZERO
|SUCC n2_minus_1 -> natmul n1 (natexp n1 n2_minus_1);;

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
|Const n -> Const 0
|Var a -> if a=x then Const 1 else Const 0
|Power (a,n) -> if a=x && n=2 then Times[Const 2; Var a]
                else if a=x then Times[Const n; Power(a,n-1)]
                else Const 0
|Times l -> begin
  match l with
  |[] -> raise (Failure "unaccepted form")
  |[e] -> raise (Failure "unaccepted form")
  |[e1;e2] -> Sum [Times [diff (e1,x); e2]; Times [e1; diff (e2,x)]]
  |hd::tl -> Sum [Times [diff (hd,x); Times tl]; Times [hd; diff (Times tl,x)]]
  end
|Sum m -> begin
  match m with
  |[] -> raise (Failure "unaccepted form")
  |[e] -> raise (Failure "unaccepted form")
  |[e1;e2] -> Sum [diff (e1,x); diff(e2,x)]
  |hd::tl -> Sum [diff (hd,x); diff (Sum tl,x)]
  end
;;


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
  let rec ev_sigma : exp -> int -> int
  = fun e n ->
    match e with
    |X -> n
    |INT n -> n
    |ADD (a,b) -> ev_sigma a n + ev_sigma b n
    |SUB (a,b) -> ev_sigma a n - ev_sigma b n
    |MUL (a,b) -> ev_sigma a n * ev_sigma b n
    |DIV (a,b) -> ev_sigma a n / ev_sigma b n
    |SIGMA _ -> calculator e

in match e with
|X -> raise (Failure "Unbound value")
|INT n -> n
|ADD (a,b) -> calculator a + calculator b
|SUB (a,b) -> calculator a - calculator b
|MUL (a,b) -> calculator a * calculator b
|DIV (a,b) -> calculator a / calculator b
|SIGMA (a,b,c) ->
  if (calculator a) = (calculator b) then ev_sigma c (calculator a)
  else if (calculator a) < (calculator b) then ev_sigma c (calculator a) + calculator (SIGMA (INT ((calculator a) + 1), INT (calculator b), c))
  else raise (Failure "Error")
;;

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m ->
 let rec wb : mobile -> int
  = fun n ->
    match n with
    |(SimpleBranch(_,a_w), SimpleBranch(_,b_w)) -> a_w + b_w
    |(SimpleBranch(_,a_w), CompoundBranch(_,cm)) -> a_w + (wb cm)
    |(CompoundBranch(_,cm), SimpleBranch(_,b_w)) -> (wb cm) + b_w
    |(CompoundBranch(_,l_m), CompoundBranch(_,r_m)) -> (wb l_m) + (wb r_m)

in match m with
|(SimpleBranch(a_l,a_w), SimpleBranch(b_l,b_w)) ->
   if a_l * a_w = b_l * b_w then true else false
|(SimpleBranch(a_l,a_w), CompoundBranch(b_l,cm)) ->
   if a_l * a_w = b_l * (wb cm) then (balanced cm) else false
|(CompoundBranch(a_l,cm), SimpleBranch(b_l,b_w)) ->
   if a_l * (wb cm) = b_l * b_w then (balanced cm) else false
|(CompoundBranch(a_l,l_m), CompoundBranch(b_l,r_m)) ->
   if a_l * (wb l_m) = b_l * (wb r_m) then (balanced l_m) && (balanced r_m) else false
;;


(* problem 7*)
type digit = ONE | ZERO
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
(* 리스트 길이*)
let rec len a =
 match a with
 |[] -> 0
 |hd::tl -> 1 + len tl

(* 2의 제곱*)
in let rec mul n =
 if n > 0 then 2 * (mul (n-1))
 else 1

(*bin -> int*)
in let rec binary l =
 match l with
 |[] -> 0
 |hd::tl -> if hd = ONE then mul ((len l)-1) + binary tl
            else binary tl

(*int -> bin*)
in let rec bi n =
 if n = 0 then [ZERO]
 else if n = 1 then [ONE]
 else if (n mod 2) = 1 then bi ((n-1)/2) @ [ONE]
 else bi (n/2) @ [ZERO]

in bi ((binary b1) * (binary b2))
;;

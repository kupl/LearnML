(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec  mirror : btree -> btree = fun t ->
match t with
|Empty -> Empty
|Node(a, x, y) -> Node(a, mirror(y), mirror(x))
;;

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat = fun n1 n2 ->
match n1 with
|ZERO -> n2
|SUCC(n) -> SUCC(natadd n n2);;

let rec natmul : nat -> nat -> nat = fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC(ZERO) -> n2
|SUCC(n) -> natadd (natmul (SUCC(ZERO)) n2) (natmul n n2);;

let rec natexp : nat -> nat -> nat = fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC(ZERO) -> SUCC ZERO
|SUCC(n) -> match n2 with |ZERO -> SUCC ZERO |SUCC(ZERO) -> n1 |SUCC(m) ->
natmul n1 (natexp n1 m);;

(* problem 3*)
 
type formula =
True | False | Var of string | Neg of formula | And of formula * formula
| Or of formula * formula | Imply of formula * formula
| Iff of formula * formula

let rec sat : formula -> bool = fun t ->
let rec tab : formula -> (string * bool) list = fun r ->
let table = [] in
match r with
|Var x -> (x, true) :: table
|And (x,y) -> (tab x) @ (tab y)
|Or (x,y) -> (tab x) @ (tab y)
|Imply (x,y) -> (tab x) @ (tab y)
|Iff (x,y) -> (tab x) @ (tab y)
|Neg x -> tab x
|_ -> table
in
let rec apply x e =
match e with
|[] -> raise (Failure("error"))
|(y,v)::tl -> if (x=y) then v else apply x tl
in
let rec check : formula -> (string * bool) list -> bool = fun r t ->
match r with
| True -> true
| False -> false
| And(x,y) -> (check x t) && (check y t)
| Or(x,y) -> (check x t) || (check y t)
| Imply (x,y) -> (not (check x t)) || (check y t)
| Iff (x,y) -> ((check x t) || (check y t))&&((not (check x t)) ||
(not (check y t)))
| Neg x -> not (check x t)
| Var x -> apply x t
in
let rec change : (string * bool) list -> int -> bool ->
(string * bool) list -> (string * bool) list  = fun t i r s->
if (i = (List.length t) - 1) then match (List.nth t i) with
|(x,true) -> change t (i-1) true ((x,false)::s)
|(x,false) -> change t (i-1) false ((x,true)::s)
else if (i>=0) then match (List.nth t i) with
|(x,true) -> change t (i-1) true ((x,r)::s)
|(x,false) -> change t (i-1) r ((x,(not r))::s)
else s
in
let rec program : formula -> (string * bool) list -> int -> bool
= fun f t i ->
if(i<0) then false
else if(check f t = true) then true
else program f (change t ((List.length t)-1) true []) (i-1)
in if(List.length (tab t) = 0) then check t [] else program t (tab t) (List.length (tab t)-1);;

(*problem 4*)

type aexp = |Const of int | Var of string | Power of string * int |
Times of aexp list | Sum of aexp list

let rec diff : aexp * string -> aexp = fun (e,x) ->
match e with
|Power (s,i) -> if(s=x) then Times[Const i ;Power(s,i-1)] else Const 1
|Const i -> Const 0
|Var s -> if(s=x) then Const 1 else Const 0
|Sum [] -> Const 0
|Sum (hd::tl) -> Sum[(diff (hd,x));(diff ((Sum tl),x))]
|Times [] -> Const 1
|Times (hd::tl) -> Sum[Times((diff (hd,x))::tl);Times[hd;diff(Times tl,x)]];;


(*problem 5*)

type exp = X | INT of int | ADD of exp * exp | SUB of exp * exp |
MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp

let rec  calculator : exp -> int = fun e ->
let extend_env v e = v::e in
let rec lookup_env e =
match e with
|[] -> raise (Failure (" "))
|v::tl -> v
in
let rec eval : exp -> int list -> int = fun exp env ->
match exp with
|INT n -> n
|ADD (n1,n2) -> eval n1 env + eval n2 env
|SUB (n1,n2) -> eval n1 env - eval n2 env
|MUL (n1,n2) -> eval n1 env * eval n2 env
|DIV (n1,n2) -> eval n1 env / eval n2 env
|X -> lookup_env env
|SIGMA (n1,n2,n3) -> let v1 = eval n1 env in let env' = extend_env v1 env in
if(eval n1 env < eval n2 env)
then (eval n3 env') + (eval (SIGMA ((INT (v1+1)), n2,n3)) env)
else eval n3 env'
 in eval e [] ;;



(*problem 6*)

type mobile = branch * branch (*left and right branches*)
and branch = SimpleBranch of length * weight
| CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool = fun m ->
let rec bal : mobile -> int = fun m ->
match m with
|(SimpleBranch (l1,w1), SimpleBranch(l2,w2)) -> if(l1 * w1 = l2 * w2)
then w1+w2 else -1
|(SimpleBranch (l1,w1), CompoundBranch(l2,m2)) -> if(l1 * w1 = l2 * bal(m2))
then w1+bal(m2) else -1
|(CompoundBranch(l1,m1), SimpleBranch(l2,w2)) -> if(l1 * bal(m1) = l2 * w2)
then bal(m1)+w2 else -1
|(CompoundBranch(l1,m1), CompoundBranch(l2,m2)) -> if(l1 * bal(m1) = l2 * bal(m2)) then bal(m1)+bal(m2) else -1
in (bal(m)>0);;


(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin = fun b1 b2 ->
let shift : bin -> bin = fun b -> b @ [ZERO] in
let rec add : bin -> bin -> digit -> bin -> int -> bin = fun p q r s i ->
if ( List.length p >= i && List.length q >= i ) then
match (List.nth p ((List.length p) - i), List.nth q ((List.length q) - i), r) with
|(ONE, ONE, ONE) -> add p q ONE (ONE::s) (i+1)
|(ONE, ONE, ZERO) -> add p q ONE (ZERO::s) (i+1)
|(ONE, ZERO, ONE) -> add p q ONE (ZERO::s) (i+1)
|(ZERO, ONE, ONE) -> add p q ONE (ZERO::s) (i+1) 
|(ZERO, ZERO, ZERO) -> add p q ZERO (ZERO::s) (i+1)
|(ONE, ZERO, ZERO) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ONE, ZERO) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ZERO, ONE) -> add p q ZERO (ONE::s) (i+1)
else if(List.length p >= i) then
match (List.nth p ((List.length p) - i), r) with
|(ONE, ONE) -> add p q ONE (ZERO::s) (i+1)
|(ONE, ZERO) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ONE) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ZERO) -> add p q ZERO (ZERO::s) (i+1)
else if(List.length q >= i) then
match (List.nth q ((List.length q) - 1), r) with
|(ONE, ONE) -> add p q ONE (ZERO::s) (i+1)
|(ONE, ZERO) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ONE) -> add p q ZERO (ONE::s) (i+1)
|(ZERO, ZERO) -> add p q ZERO (ZERO::s) (i+1)
else if (r = ONE) then (ONE::s) else s
in let rec mul : bin -> bin -> int -> int -> bin -> bin = fun p q len i s ->
if(len>=i&&List.nth q ((List.length q)-i) = ONE) then mul (shift p) q len (i+1) (add p s ZERO [] 1)
else if(len>=i&&List.nth q ((List.length q)-i) = ZERO) then mul (shift p) q len (i+1) s
else s
in if(b1 = [ZERO] or b2 = [ZERO]) then [ZERO] else mul b1 b2 (List.length b2) 1 [];;

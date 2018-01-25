

(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
let rec is_mirror t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Node(_, l1, r1), Node(_, l2, r2) ->
     is_mirror l1 r2 && is_mirror r1 l2
  | _ -> false

let is_symmetric = function
  | Empty -> true
  | Node(_, l, r) -> is_mirror l r


(* problem 2*)
type nat = ZERO | SUCC of nat

add : nat -> nat -> nat 
= fun n1 n2 -> [n,p:Nat] [X:*] [f:X->X] [a:X] n X f (p X f a)

let natmul : nat -> nat -> nat 
= fun n1 n2 -> [n,p:Nat] [X:*] [f:X->X] [a:X] n X (p X f) a

let natexp : nat -> nat -> nat 
= fun n1 n2 -> [n,p:Nat] [X:*] p (X -> X) (n X)



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
= fun f -> let rec eval : ('a → bool) → 'a formula → bool = λ env → λ
  | True → true
  | False → false
  | Variable x → env x
  | Negation f → not (eval env f)
  | Conjunction (a, b) → (eval env a) && (eval env b)
  | Disjunction (a, b) → (eval env a) || (eval env b)
  | Implication (a, b) → not (eval env a) || (eval env b)
  | Equivalence (a, b) → (eval env a) = (eval env b)


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> Var(n)
ev(Const(n)) = Const(n)
ev(Plus(e1,e2)) = (cas ev(e1) of
 Var(n) => Plus(Var(n),ev(e2))  |
 Const(n) => (case ev(e2) of
   Var(m) => Plus(Const(n),Var(m))
   Const(m) => Const(n+m)
   Plus(e3,e4) => Plus(Const(n),Plus(e3,e4)) |
 Plus(e3,e4) => Plus(Plus(e3,e4),ev(e2)) );



(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> (* TODO *)


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> (* TODO *)


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)

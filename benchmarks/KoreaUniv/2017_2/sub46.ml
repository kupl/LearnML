(* problem 1*)
type btree = Empty | Node of int * btree * btree

let combine n l r = Node (n, l, r)

let rec mirror : btree -> btree
= fun t -> match t with | Empty -> t | Node (n, Empty, Empty) -> t | Node (n, l, r) -> combine (n) (mirror r) (mirror l)



(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with | ZERO -> n2 | SUCC(x) -> natadd (x) (SUCC(n2))

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with | ZERO -> ZERO | SUCC(ZERO) -> n2 | SUCC(x) -> natadd (natmul x n2) (n2)


let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with |ZERO->SUCC(ZERO) |SUCC(ZERO)-> n1 |SUCC(x) -> natmul (n1) (natexp n1 x)


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

type value = N | B of bool
type env = (string * value) list

let empty_env = []

let extend_env (x, v) e = (x, v) :: e

let rec apply_env x e = match e with | [] -> N | (y, v)::tl -> if (x = y) then v else apply_env x tl

let rec eval : formula -> env -> value
= fun formula env -> match formula with | True -> B true | False -> B false | Var p -> apply_env p env | Neg p -> let v1 = eval p env in (match v1 with |N -> N |B true -> B false |B false -> B true) | And (p, q) -> if (Neg p = q) then B false else (if (Neg q = p) then B false else let v1 = eval p env in let v2 = eval q env in (match v1, v2 with | B true, B true -> B true | N, B true -> N | B true, N -> N | N, N -> N |_ -> B false))| Or (p, q) -> let v1 = eval p env in let v2 = eval q env in (match v1, v2 with |B false, B false -> B false | N,B false -> N | B false, N -> N | N, N -> N |_ -> B true) | Imply (p, q) -> let v1 = eval p env in let v2 = eval q env in(match v1, v2 with | B true, B false -> B false | B true, N -> N | N, B false -> N | N, N -> N |_ -> B true) | Iff (p, q) -> if (Neg p = q) then B false else (if (Neg q = p) then B false else if (p = q) then B true else let v1 = eval p env in let v2 = eval q env in (match v1, v2 with |B true,B false -> B false | B false, B true -> B false | B true, B true -> B true | B false, B false -> B true |_ -> N))


let sat : formula -> bool
= fun f -> match (eval f empty_env) with |N -> true | B true -> true | B false -> false



(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let make_sum e = Sum ([e] @ [Const 0])

let rec f (a, x) = match a with
        | Const n -> Const 0
        | Var y -> if (x = y) then Const 1 else Const 0
        | Power (y, n) -> if (x = y) then (Times [Const n; Power(y, n - 1)]) else Const 0
        | Times l -> (match l with
                | [] -> Const 0
                | hd::tl -> Sum [Times (f (hd, x)::tl) ; Times (hd:: [(f(Times tl, x))])])
        | Sum l -> match l with
                | [] -> Const 0
                | hd::tl -> Sum [f (hd,x); f (Sum tl, x)]


let diff : aexp * string -> aexp
= fun (e,x) -> f(e, x)


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

type env = (exp * int) list
let empty_env = []
let extend_env (x, v) e = (x, v)::e

let rec apply_env x e = match e with |[] -> raise(Failure "n") |(y, v)::tl -> if (x = y) then v else apply_env x tl

let rec eval: exp -> env -> int
= fun exp env -> match exp with | X -> apply_env X env | INT n -> n | ADD (e1, e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1, v2 with | n1, n2 -> (n1 + n2)) | SUB(e1, e2) -> let v1 = eval e1 env in let v2 = eval e2 env in(match v1, v2 with | n1, n2 -> (n1 - n2)) | MUL (e1, e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1, v2 with |n1, n2 -> (n1 * n2)) | DIV (e1, e2) -> let v1 = eval e1 env in let v2 = eval e2 env in (match v1, v2 with |n1, n2 -> (n1 / n2)) | SIGMA (e1, e2, e3) -> let v1 = eval e1 env in let v2 = eval e2 env in (if v1 > v2 then 0 else (eval e3 (extend_env (X, v1) env)) + eval (SIGMA(ADD(INT 1, e1) , e2, e3)) env)

let calculator : exp -> int
= fun e -> eval e empty_env


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec get_weight m =
  match m with
  |(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> (w1 + w2)
  |(SimpleBranch(l1, w1), CompoundBranch(l2, m1)) -> (w1 + get_weight m1)
  |(CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> (get_weight m1 + w2)
  |(CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> (get_weight m1) + (get_weight m2)

let rec balanced : mobile -> bool
= fun m -> match m with
  |(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> (l1 * w1) = (l2 * w2)
  |(SimpleBranch(l1, w1), CompoundBranch(l2, m1)) -> ((l1 * w1) = (l2 * get_weight m1)) && balanced m1
  |(CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> ((l1 * get_weight m1) = (l2 * w2)) && balanced m1
  |(CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> ((l1 * get_weight m1) = (l2 * get_weight m2)) && (balanced m1) && (balanced m2)


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list


let rec reverse l = match l with |[] -> []| hd::tl -> (reverse tl) @ [hd]

let rec to_dec b n = match b with |[] -> 0|hd::tl -> if (hd=ZERO) then (to_dec (tl) (2 * n)) else (1 * n) + (to_dec (tl) (2 * n))

let proc b = to_dec (reverse b) 1

let rec to_bin n = match n with |0 -> [ZERO] |1 -> [ONE] |_->(to_bin (n / 2)) @ (to_bin (n mod 2))

let bmul : bin -> bin -> bin
= fun b1 b2 -> to_bin ((proc b1) * (proc b2))

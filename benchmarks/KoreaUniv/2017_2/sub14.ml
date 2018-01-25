(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> (* TODO *)
  let rec helper t =
    match t with
    | Empty -> t
    | Node(n, l1, r1) -> Node(n, (helper r1), (helper l1)) in
      helper t


(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  let rec helper n1 n2 =
    match n1 with
    | ZERO -> n2
    | SUCC (x) -> helper x (SUCC (n2)) in
      helper n1 n2

let natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  let rec helper n1 n2 =
    match n1 with
    | ZERO -> ZERO
    | SUCC (x) -> natadd n2 (helper x n2) in
      helper n1 n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  let rec helper n1 n2 =
    match n2 with
    | ZERO -> SUCC ZERO
    | SUCC (x) -> natmul n1 (helper n1 x) in
      helper n1 n2


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
= fun f -> (* TODO *)
  let rec helper f =
    match f with
    | False -> false
    | Neg (x) -> if (helper x = true) then false else true
    | And (Var x, Neg (Var y)) -> if x = y then false else true
    | Iff (Var x, Neg (Var y)) -> if x = y then false else true
    | And (x, y) -> helper x && helper y
    | Or (x, y) -> helper x || helper y
    | Imply (x, y) -> if ((helper x = true) && (helper y = false)) then false else true
    | Iff (x, y) -> if helper x = helper y then false else true
    | _ -> true in
      helper f


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)
  let rec helper (e,x) =
    match e with
    |Const a -> Const 0
    |Var v -> if v = x then Const 1 else Const 0
    |Power (s, i) -> if s = x then Times [Const i; Power (s, i-1)] else Const 0
    (*|Times lst -> begin
      match lst with
      |h::t -> Sum ([Times [helper (h, x)] @t] @ [Times [h; helper (t, x)]])
      end
    |Sum lst2 -> begin
      match lst2 with
      |h::t -> Sum (helper (h, x)::helper (Sum t, x)) 
      end*) in
        helper (e,x)


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
  let rec helper e = 
    match e with
    | INT i -> i
    | ADD (e1, e2) -> helper e1 + helper e2
    | SUB (e1, e2) -> helper e1 - helper e2
    | MUL (e1, e2) -> helper e1 * helper e2
    | DIV (e1, e2) -> helper e1 / helper e2 (*error when division by zero*)
    | SIGMA (e1, e2, e3) -> if helper e1 = helper e2 then (let X = e1 in helper e3) else (let X = e1 in helper e3) + helper (SIGMA(ADD(e1, INT 1), e2, e3)) in
      helper e



(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> (* TODO *)
  let rec calcweight comb =
    match comb with
    |SimpleBranch (a1,a2), SimpleBranch (b1,b2) -> a1*a2 + b1*b2
    |SimpleBranch (a1,a2), CompoundBranch (b1,b2) -> a1*a2 + b1*(calcweight b2)
    |CompoundBranch (a1,a2), SimpleBranch (b1,b2) -> a1*(calcweight a2) + b1*b2
    |CompoundBranch (a1,a2), CompoundBranch (b1,b2) -> a1*(calcweight a2) + b1*(calcweight b2) in
      let rec helper m =
        match m with
        |SimpleBranch (a1,a2), SimpleBranch (b1,b2) -> if a1*a2 = b1*b2 then true else false
        |SimpleBranch (a1,a2), CompoundBranch (b1,b2) -> if a1*a2 = b1*(calcweight b2) then true else false
        |CompoundBranch (a1,a2), SimpleBranch (b1,b2) -> if a1*(calcweight a2) = b1*b2 then true else false
        |CompoundBranch (a1,a2), CompoundBranch (b1,b2) -> if a1*(calcweight a2) = b1*(calcweight b2) then true else false in
          helper m



(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
  (*let x = List.rev b1 and
  let y = List.rev b2 in*)
  let rec helper b1 b2 =
    match b1 with
    |[ZERO] -> [ZERO]
    |[ONE] -> b2
    |[ONE;ZERO] -> b2@[ZERO]
    |h::t -> if h=ZERO then helper t b2 else b2@[ZERO] in
      helper b1 b2

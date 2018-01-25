(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
  match t with
  | Empty -> Empty
  | Node (n, l, r) -> Node (n, mirror r, mirror l) 


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  | ZERO -> n1
  | SUCC n -> SUCC (natadd n1 n)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  | ZERO -> ZERO
  | SUCC n -> natadd n1 (natmul n1 n)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  | ZERO -> SUCC ZERO
  | SUCC n -> natmul n1 (natexp n1 n)


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
  let rec appendvar str l =
    match l with
    | [] -> str::l
    | h::t -> if h = str then l else h::(appendvar str t) in
  let rec findvar form l =
    match form with 
    | True
    | False -> l
    | Var str -> appendvar str l
    | Neg f1 -> findvar f1 l
    | And (f1, f2)
    | Or (f1, f2)
    | Imply (f1, f2)
    | Iff (f1, f2) -> findvar f1 (findvar f2 l) in
  let rec setvar form (s, b) =
    match form with 
    | True -> True
    | False -> False
    | Var str -> if str = s then b else (Var str)
    | Neg f1 -> Neg (setvar f1 (s, b))
    | And (f1, f2) -> And (setvar f1 (s, b), setvar f2 (s, b))
    | Or (f1, f2) -> Or (setvar f1 (s, b), setvar f2 (s, b))
    | Imply (f1, f2) -> Imply (setvar f1 (s, b), setvar f2 (s, b))
    | Iff (f1, f2) -> Iff (setvar f1 (s, b), setvar f2 (s, b)) in
  let rec eval form =
    match form with 
    | True -> true
    | False -> false
    | Var str -> raise (Failure "There is variable in formula")
    | Neg f1 -> not (eval f1)
    | And (f1, f2) -> (eval f1) && (eval f2)
    | Or (f1, f2) -> (eval f1) || (eval f2)
    | Imply (f1, f2) -> (not (eval f1)) || (eval f2)
    | Iff (f1, f2) -> (eval f1) = (eval f2) in
  let rec recursiveeval form l =
    match l with
    | [] -> eval form
    | h::t ->
        (recursiveeval (setvar form (h, True)) t)
        || (recursiveeval (setvar form (h, False)) t) in
  recursiveeval f (findvar f [])

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e, x) ->
  match e with
  | Const n -> Const 0
  | Var str -> if str = x then Const 1 else Const 0
  | Power (str, n) ->
      if str = x then Times [Const n; Power (x, (n-1))]
      else Const 0
  | Times l ->
    (match l with
    | [] -> Const 0
    | h::t -> Sum [Times (diff (h, x)::t);
                   Times [h; diff (Times t, x)]])
  | Sum l -> 
    (match l with
    | [] -> Const 0
    | h::t -> Sum [diff (h, x); diff (Sum t, x)])


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
  match e with
  | X -> raise (Failure "Variable X is not bounded")
  | INT n -> n
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> (calculator e1) / (calculator e2)
  | SIGMA (e1, e2, e3) ->
    let rec sigmacalc ex v =
      match ex with
      | X -> v
      | INT n -> n
      | ADD (e1, e2) -> (sigmacalc e1 v) + (sigmacalc e2 v)
      | SUB (e1, e2) -> (sigmacalc e1 v) - (sigmacalc e2 v)
      | MUL (e1, e2) -> (sigmacalc e1 v) * (sigmacalc e2 v)
      | DIV (e1, e2) -> (sigmacalc e1 v) / (sigmacalc e2 v)
      | SIGMA (e1, e2, e3) ->
          calculator (SIGMA (INT (sigmacalc e1 v), INT (sigmacalc e2 v), e3)) in
    let rec summation f l exp =
      if f > l then 0
      else (sigmacalc exp f) + (summation (f+1) l exp) in
    summation (calculator e1) (calculator e2) e3


(* problem 6*)
type mobile = branch * branch     (* left and right branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m ->
  let rec branchweight b =
    match b with
    | SimpleBranch (len, w) -> w
    | CompoundBranch (len, m) ->
      (match m with (left, right) ->
        (branchweight left) + (branchweight right)) in
  let torque b =
    match b with
    | SimpleBranch (len, w) -> len * w 
    | CompoundBranch (len, m) -> len * (branchweight b) in
  let branchbalanced b =
    match b with
    | SimpleBranch (len, w) -> true
    | CompoundBranch (len, m) -> balanced m in
  match m with (l, r) ->
    (branchbalanced l) && (branchbalanced r)
    && ((torque l) = (torque r))


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let rec bintoint b n =
    match b with
    | [] -> n
    | h::t ->
      if h = ZERO then (bintoint t (2*n))
      else if h = ONE then (bintoint t ((2*n) + 1))
      else raise (Failure "Invalid binary value") in
  let rec inttobin n b =
    if n = 0 then b
    else if n mod 2 = 0 then inttobin (n/2) (ZERO::b)
    else inttobin (n/2) (ONE::b) in
  if (b1 = []) || (b2 = []) then raise (Failure "Invalid Input")
  else inttobin ((bintoint b1 0)*(bintoint b2 0)) []
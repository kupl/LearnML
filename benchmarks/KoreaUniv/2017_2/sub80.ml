(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> (* TODO *)
let rec impl _t =
  match _t with
  | Empty -> Empty
  | Node (idx, left, right) -> Node (idx, (impl right), (impl left)) in
impl t;;


(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
let rec impl m1 m2 =
  match m1 with
  | ZERO -> m2
  | SUCC (x) -> impl x (SUCC (m2)) in
impl n1 n2;;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
let rec impl m1 m2 =
  match m1 with
  | ZERO -> ZERO
  | SUCC (x) -> natadd (impl x m2) m2 in
impl n1 n2;;

let natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
let rec impl m1 m2 =
  match m2 with
  | ZERO -> SUCC ZERO
  | SUCC (x) -> natmul (impl m1 x) m1 in
impl n1 n2;;


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
let rec to_DNF _f =
  match _f with
  | Neg (x) ->
  let impl = 
    match x with
    | True -> False
    | False -> True
    | Neg (y) -> to_DNF y
    | And (a, b) -> Or ((to_DNF (Neg a)), (to_DNF (Neg b)))
    | Or (a, b) -> And ((to_DNF (Neg a)), (to_DNF (Neg b)))
    | _ -> _f in
  impl
  | And (_x, _y) ->
  let impl =
    let x = to_DNF _x in
    let y = to_DNF _y in
    match x with
    | True -> y
    | False -> False
    | Or (a, b) -> Or ((to_DNF (And (a, y))), (to_DNF (And (b, y))))
    | _ ->
    let _impl =
      match y with
      | True -> x
      | False -> False
      | Or (a, b) -> Or ((to_DNF (And (x, a))), (to_DNF (And (x, b))))
      | _ -> And (x, y) in
    _impl in
  impl
  | Or (_x, _y) ->
  let impl =
    let x = to_DNF _x in
    let y = to_DNF _y in
    match x with
    | True -> True
    | False -> y
    | _ ->
    let _impl =
      match y with
      | True -> True
      | False -> x
      | _ -> Or (x, y) in
    _impl in
  impl
  | _ -> _f in
let rec to_NegAndOr _f =
  match _f with
  | Neg (x) -> Neg (to_NegAndOr x)
  | And (x, y) -> And ((to_NegAndOr x), (to_NegAndOr y))
  | Or (x, y) -> Or ((to_NegAndOr x), (to_NegAndOr y))
  | Imply (x, y) -> Or ((Neg (to_NegAndOr x)), (to_NegAndOr y))
  | Iff (x, y) ->
  let _x = to_NegAndOr x in
  let _y = to_NegAndOr y in
  And ((Or ((Neg _x), _y)), (Or ((Neg _y), _x)))
  | _ -> _f in
(to_DNF (to_NegAndOr f)) <> False;;


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)
let rec impl _e =
  match _e with
  | Const (_) -> Const (0)
  | Var (y) ->
  if x = y then Const (1)
  else Const (0)
  | Power (y, n) ->
  if x = y then
    if n = 0 then Const (0)
    else
      if n = 1 then Const (1)
      else
        if n = 2 then Times [Const (2); Var (y)]
        else Times ([Const (n); Power (y, (n - 1))])
  else Const (0)
  | Times (l) ->
  let tmp =
    match l with
    | [] -> Const (0)
    | hd::tl ->
    let res = impl hd in
    let diff_tl_part =
      if hd = (Const (1)) then impl (Times (tl))
      else Times ([hd; (impl (Times (tl)))]) in
    if res = (Const (0)) then diff_tl_part
    else
      if diff_tl_part = (Const (0)) then Times ((impl hd)::tl)
      else Sum ([(Times ((impl hd)::tl)); diff_tl_part]) in
  tmp
  | Sum (l) ->
  let tmp =
    match l with
    | [] -> Const (0)
    | hd::tl ->
    let res = impl hd in
    if res = (Const (0)) then impl (Sum (tl))
    else Sum ([(impl hd); (impl (Sum (tl)))]) in
  tmp in
impl e;;


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
let rec replace _e n =
  match _e with
  | X -> INT (n)
  | ADD (x, y) -> ADD ((replace x n), (replace y n))
  | SUB (x, y) -> SUB ((replace x n), (replace y n))
  | MUL (x, y) -> MUL ((replace x n), (replace y n))
  | DIV (x, y) -> DIV ((replace x n), (replace y n))
  | _ -> _e in
let rec impl _e =
  match _e with
  | INT (x) -> x
  | ADD (x, y) -> (impl x) + (impl y)
  | SUB (x, y) -> (impl x) - (impl y)
  | MUL (x, y) -> (impl x) * (impl y)
  | DIV (x, y) -> (impl x) / (impl y)
  | SIGMA (a, b, x) ->
  let _a = impl a in
  let _b = impl b in
  if _a == _b then impl (replace x _a)
  else
    if _a < _b then (impl (replace x _a)) + (impl (SIGMA ((INT (_a + 1)), INT (_b), x)))
    else (impl (replace x _b)) + (impl (SIGMA ((INT (_b + 1)), INT (_a), x)))
  | _ -> 0 in
impl e;;


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> (* TODO *)
let rec impl _m =
  match _m with
  | (left, right) ->
  let under b =
    match b with
    | SimpleBranch(l, w) -> w
    | CompoundBranch(l, sm) -> impl sm in
  let bv b w =
    match b with
    | SimpleBranch(l, _) -> l * w
    | CompoundBranch(l, _) -> l * w in
  let lw = under left in
  let rw = under right in
  if (bv left lw) = (bv right rw) then lw + rw
  else 0 in
(impl m) <> 0;;
  
  


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
let to_int b =
  let rec to_int_impl _b n =
    match _b with
    | [] -> n
    | hd::tl ->
    if hd = ONE then to_int_impl tl (n * 2 + 1)
    else to_int_impl tl (n * 2) in
  to_int_impl b 0 in
let to_bin i =
  let rec to_bin_impl b _i =
    if _i = 0 then b
    else
      if _i mod 2 = 1 then to_bin_impl (ONE::b) (_i / 2)
      else to_bin_impl (ZERO::b) (_i / 2) in
  if i = 0 then [ZERO]
  else to_bin_impl [] i in
to_bin ((to_int b1) * (to_int b2));;

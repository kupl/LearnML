(* problem 1*)
type btree = Empty | Node of int * btree * btree;;

let rec mirror : btree -> btree
= fun t ->
  match t with
    Empty -> Empty
    |Node (int, t1, t2) -> Node (int, mirror t2, mirror t1);;

(* problem 2*)
type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n2 with
    ZERO -> n1
    |SUCC n3 -> SUCC (natadd n1 n3);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n2 with
    ZERO -> ZERO
    |SUCC n3 -> natadd n1 (natmul n1 n3);;

let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
  match n2 with
    ZERO -> SUCC ZERO
    |SUCC n3 -> natmul n1 (natexp n1 n3);;
(* problem 3*)
type formula =
    True
    |False
    |Var of string
    |Neg of formula
    |And of formula * formula
    |Or of formula * formula
    |Imply of formula * formula
    |Iff of formula * formula;;

let rec sat : formula -> bool
= fun f ->
  let rec eval : formula -> bool -> bool
  = fun f b -> match f with
                |True -> true
                |False -> false
                |Var s -> if b = true then true else false
                |Neg f1 -> not (eval f1 true)
                |And (f1, f2) -> (eval f1 true) && (eval f2 true)
                |Or (f1, f2) -> (eval f1 true) || (eval f2 true)
                |Imply (f1, f2) -> (not (eval f1 true)) || (eval f2 true)
                |Iff (f1, f2) -> if (eval f1 true) then (eval f2 true) 
                                 else not (eval f2 true)
  in eval f true;;
(* problem 5*)
type exp = X
        | INT of int
        | ADD of exp * exp
        | SUB of exp * exp
        | MUL of exp * exp
        | DIV of exp * exp
        | SIGMA of exp * exp * exp;;

let rec calculator : exp -> int
= fun e ->
  let rec eval : exp -> int -> int
  = fun e n -> match e with
                |INT n1 -> n1
                |X -> n
                |ADD (e1, e2) -> (eval e1 n) + (eval e2 n)
                |SUB (e1, e2) -> (eval e1 n) - (eval e2 n)
                |MUL (e1, e2) -> (eval e1 n) * (eval e2 n)
                |DIV (e1, e2) -> (eval e1 n) / (eval e2 n)
                |SIGMA (a, b, f) ->
                  if (eval a n) > (eval b n) then 0 else
                  (eval f (eval a n)) + eval (SIGMA (ADD (a, INT 1), b, f)) n
    in eval e 1;;

(* problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int;;

let get_length : branch -> int
= fun b -> match b with
            |SimpleBranch (l, w) -> l
            |CompoundBranch (l, m) -> l;;

let rec branch_weight : branch -> int
= fun b -> 
  let mobile_weight : mobile -> int
  = fun m -> match m with
              |(left, right) -> (branch_weight left) + (branch_weight right)
  in
  match b with
    |SimpleBranch (l, w) -> (get_length b) * w
    |CompoundBranch (l, m) -> (get_length b) * (mobile_weight m);;

let balanced : mobile -> bool
= fun m -> match m with
            |(left, right) -> (branch_weight left) == (branch_weight right);;

(* problem 7*)
type digit = ZERO | ONE;;
type bin = digit list;;

let digitize : int -> digit
= fun n -> match n with
            |0 -> ZERO
            |_ -> ONE;;

let rec getbin : int -> bin
= fun n ->
  let rec dtob : int -> bin -> bin
  = fun a l -> match a with
                |0 -> l
                |_ -> dtob (a / 2) ((digitize (a mod 2)) :: l)
  in dtob n [];;

let rec exp : int -> int
= fun n -> match n with
            |0 -> 1
            |_ -> 2 * exp (n - 1);;


let rec count : bin -> int
= fun l -> match l with
            |[] -> 0
            |hd :: tl -> 1 + (count tl);;

let rec btod : bin -> int
= fun l -> match l with
            |[] -> 0
            |hd :: tl  -> if hd = ZERO then (btod tl) else (exp ((count l) - 1)) + btod tl;;

let bmul : bin -> bin -> bin
= fun b1 b2 -> getbin ((btod b1) * (btod b2));;
                
                


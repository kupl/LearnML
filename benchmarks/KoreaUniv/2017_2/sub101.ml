(* problem 1*)
(* type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> (* TODO *)
*)

(* problem 2*)
type nat = ZERO | SUCC of nat
;;

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC n1_two -> SUCC (natadd n1_two n2)
;;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC n1_two -> natadd n2 (natmul n1_two n2)
;;
(*
let natexp : nat -> nat -> nat 
= fun n1 n2 -> 
*)

(* problem 3*)
(*type formula =
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
*)

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
  | Const _ -> Const 0
  | Var y -> if x = y then Const 1 else Const 0
  | Power (y, z) -> if not (x = y) then Const 0
                    else Times [Const z; Power (y, z - 1)]
  | Times [] -> Const 0
  | Times (hd::tl) -> Sum [Times (diff (hd, x)::tl); Times [hd; diff(Times tl, x)]]
  | Sum aexps -> Sum (List.map (fun ae -> diff (ae, x)) aexps)
;;


(* problem 5*)
(*module Problem3 = struct
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->
  match exp with
    | X -> raise (Failure "Unbound X")
    | INT n -> n
    | ADD (e1, e2) -> calculator e1 + calculator e2
    | SUB (e1, e2) -> calculator e1 - calculator e2
    | MUL (e1, e2) -> calculator e1 * calculator e2
    | DIV (e1, e2) -> calculator e1 / calculator e2
    | SIGMA (e1, e2, e3) ->
      let s e_new = calculator e1, calculator e2 in
        if s = e_new then eval_sigma e3 s else
        if s < e_new then eval_sigma e3 s + calculator (SIGMA (INT (s + 1), INT e_new, e3))
        else raise (Failure "error")
    and eval_sigma : exp -> int -> int 
    = fun exp n ->
      match exp with
        | X -> n
        | INT n -> n 
        | ADD (e1, e2) -> eval_sigma e1 n + eval_sigma e2 n
        | SUB (e1, e2) -> eval_sigma e1 n - eval_sigma e2 n 
        | MUL (e1, e2) -> eval_sigma e1 n * eval_sigma e2 n 
        | DIV (e1, e2) -> eval_sigma e1 n / eval_sigma e2 n 
        | SIGMA _ -> calculator exp 
end
;;
*)

(* problem 6*)
(*module Problem6 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
let rec totalWeight m : mobile -> int
  =fun m -> match m with 
  	| (SimpleBranch(_,w1), SimpleBranch(_,w2)) -> w1 + w2
  	| (CompoundBranch(_,m1), SimpleBranch(_,w2)) -> (totalWeight m1) + w2
  	| (SimpleBranch(_,w1), CompoundBranch(_,m2)) -> (totalWeight m2) + w1
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb, rb with
  	| (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) -> if (l1 * w1) = (l2 * w2) then true else false
  	| (CompoundBranch(l1,m1), SimpleBranch(l2,w2)) -> if (l1 * (totalWeight m1)) = (l2 * w2) then true else false 
  	| (SimpleBranch(l1,w1), CompoundBranch(l2,m2)) -> if (l1 * w1) = (l2 * (totalWeight m2)) then true else false;;
end *)

(* problem 7*)
(*type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
*)
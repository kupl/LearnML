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
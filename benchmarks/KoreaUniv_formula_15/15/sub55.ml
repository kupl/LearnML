type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f ->
        let negb b : bool =
                if b then false else true in
        let orb ba bb : bool =
                if ba then true else bb in
        let andb ba bb : bool =
                if ba then bb else false in
        let implyb ba bb : bool =
                if ba then bb else true in
        let equivb ba bb : bool =
                if ba = bb then true else false in
        match f with
        | True -> true
        | False -> false
        | Neg a -> negb (eval a)
        | Or (a, b) -> orb (eval a) (eval b)
        | And (a, b) -> andb (eval a) (eval b)
        | Imply (a, b) -> implyb (eval a) (eval b)
        | Equiv (a, b) -> equivb (eval a) (eval b)

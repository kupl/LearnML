type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff (aexp, x) =
  match aexp with
  | Const n -> Const 0
  | Var v -> if v = x then Const 1 else Const 0
  | Power (v, n) ->
      if v = x then Times [ Const n; Power (v, n - 1) ] else Const 0
  | Sum l ->
      let rec lstRec2 l =
        match l with [] -> Const 0 | h :: t -> Sum [ diff (h, x); lstRec2 t ]
      in
      lstRec2 l

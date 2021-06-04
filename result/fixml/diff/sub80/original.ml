type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, var) ->
  match exp with
  | Const c -> Const 0
  | Var v -> if v = var then Const 1 else Const 0
  | Power (st, i) ->
      if st = var then Times [ Const i; Power (st, i - 1) ] else Power (st, i)
  | Times h :: t -> h
  | Sum h :: t ->
      let diffHelp alexp = diff (alexp, var) in
      Sum (diff (h, var) :: List.map diffHelp t)
  | _ -> exp

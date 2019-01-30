type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp ->
  match exp with
    | INT x -> x
    | ADD (a, b) -> (calculator a) + (calculator b)
    | SUB (a, b) -> (calculator a) - (calculator b)
    | MUL (a, b) -> (calculator a) * (calculator b)
    | DIV (a, b) -> (calculator a) / (calculator b) 
    | SIGMA (a, b, c) ->
      let rec funcalc xexp xval =
        match xexp with
          | X -> xval
          | INT x -> x
          | ADD (a, b) -> (funcalc a xval) + (funcalc b xval)
          | SUB (a, b) -> (funcalc a xval) - (funcalc b xval)
          | MUL (a, b) -> (funcalc a xval) * (funcalc b xval)
          | DIV (a, b) -> (funcalc a xval) / (funcalc b xval)
          | SIGMA (a, b, c) -> calculator (SIGMA ((INT (funcalc a xval)), (INT (funcalc b xval)), c)) in
      if calculator a <= calculator b then funcalc c (calculator a) + calculator (SIGMA ((INT ((calculator a) + 1)), b, c))
      else 0;;


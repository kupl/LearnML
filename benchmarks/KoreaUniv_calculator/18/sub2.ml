type exp = 
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int = fun exp -> 
  let rec inner exp x =
    match exp with
      |X -> x
      |INT a -> a
      |SUB (a, b) -> (inner a x) - (inner b x)
      |ADD (a, b) -> (inner a x) + (inner b x)
      |MUL (a, b) -> (inner a x) * (inner b x)
      |DIV (a, b) -> (inner a x) / (inner b x)
      |SIGMA (i, k, e) -> 
        let rec sigma : int * int * exp * int -> int = fun (i, k, e, accum) ->
          if i > k then accum
          else sigma ((i + 1), k, e, (accum + (inner e i))) in
        let start = inner i x in
        let en = inner k x in
        sigma (start, en, e, 0)
  in inner exp 0;;
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
    |X -> 0
    | INT a -> a
    | ADD (a,b) -> calculator a + calculator b
    | SUB (a,b) -> calculator a - calculator b
    | MUL (a,b) -> calculator a * calculator b
    | DIV (a,b) -> calculator a / calculator b 
    | SIGMA (a, b, c) ->
      let rec putin : exp -> exp -> exp
      = fun ex inp ->
        match ex with
          |X -> inp
          |INT a -> INT a
          |ADD (a,b) -> ADD(putin a inp, putin b inp)
          |SUB (a,b) -> SUB(putin a inp, putin b inp)
          |MUL (a,b) -> MUL(putin a inp, putin b inp)
          |DIV (a,b) -> DIV(putin a inp, putin b inp)
          |SIGMA (a, b, c) ->
            if calculator (putin a inp) = calculator (putin b inp)  then putin (putin c a) inp
            else ADD(putin (putin c a) inp, (putin (SIGMA ((ADD (a, INT 1)), b, c)) inp)) in
          if calculator a = calculator b then calculator(putin c a) else calculator (putin c a) + calculator (SIGMA ((ADD (a, INT 1)), b, c));;
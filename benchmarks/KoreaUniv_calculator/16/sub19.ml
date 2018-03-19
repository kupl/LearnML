
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


  let rec calculator : exp -> int
  = fun exp ->
  match exp with
  | X -> raise(Failure "Wrong input")
  | INT m -> m
  | ADD (m, n) -> calculator m + calculator n
  | SUB (m, n) -> calculator m - calculator n
  | MUL (m, n) -> calculator m * calculator n
  | DIV (m, n) -> calculator m / calculator n
  | SIGMA(start, last, polynomial) ->
  let s = calculator start in
    let l = calculator last in
    if s = l then
    begin
    let rec helper = fun p la ->
    match p with
    | X -> helper la la
    | INT n -> n
    | ADD (m, n) -> (helper m la) + (helper n la)
    | SUB (m, n) -> (helper m la) - (helper n la)
    | MUL (m, n) -> (helper m la) * (helper n la)
    | DIV (m, n) -> (helper m la) / (helper n la)
    | SIGMA (st, las, pol) -> calculator p
    in helper polynomial last
    end
   else
  calculator (SIGMA (start, SUB(last, INT 1), polynomial)) + calculator (SIGMA(last, last, polynomial))
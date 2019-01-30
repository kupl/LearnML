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
  let rec calc : exp -> bool -> int -> int
  = fun exp flag x -> 
    match exp with 
      | X -> x
      | INT n -> n
      | ADD (n1, n2) -> (calc n1 flag x) + (calc n2 flag x) 
      | SUB (n1, n2) -> (calc n1 flag x) - (calc n2 flag x)
      | MUL (n1, n2) -> (calc n1 flag x) * (calc n2 flag x)
      | DIV (n1, n2) -> (calc n1 flag x) / (calc n2 flag x)
      | SIGMA (n1, n2, fx) -> 
        let rec sigma : int -> int -> exp -> int -> int
        = fun cur n2 fx result ->
          if cur <= n2 then sigma (cur + 1) n2 fx (result + (calc fx true cur))
          else result
        in sigma (calc n1 false x) (calc n2 false x) fx 0
  in calc exp false 0;;

    type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let calculator : exp -> int
  = fun exp -> 
  let inttoexp : int -> exp
  = fun int1 -> match int1 with
  | x -> INT x in
  let rec meaning : exp -> exp -> int
  = fun exp n-> match exp with
  | X -> meaning n n
  | INT a -> a
  | ADD(a, b) -> meaning a n + meaning b n
  | SUB(a, b) -> meaning a n - meaning b n
  | MUL(a, b) -> (meaning a n) * (meaning b n)
  | DIV(a, b) -> (meaning a n) / (meaning b n)
  | SIGMA(a, b ,c) ->   
    let rec sigma : exp -> int -> int -> int
    = fun k x1 y1->
    if x1 < y1 then meaning k (inttoexp x1) + sigma k (x1+1) y1 else meaning k (inttoexp x1) 
    in 
  sigma c (meaning a n) (meaning b n)
in
  meaning exp (inttoexp 0)
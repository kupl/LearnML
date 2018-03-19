(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

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
    let rec oper t =
      (match t with
      X -> raise NotImplemented  |
      INT n1 -> n1 |
      ADD (n1, n2) -> (oper n1) + (oper n2) |
      SUB (n1, n2) -> (oper n1) - (oper n2) |
      MUL (n1, n2) -> (oper n1) * (oper n2) |
      DIV (n1, n2) -> (oper n1) / (oper n2) |
      SIGMA (n1, n2, n3) ->
      let rec cal k =
         (match k with
         X -> (fun x->x) |
         INT i -> (fun x->i) |
         ADD (p,q) -> (fun x->((cal p) x)+((cal q) x)) |
         SUB (p,q) -> (fun x->((cal p) x)-((cal q) x)) |
         MUL (p,q) -> (fun x->((cal p) x)*((cal q) x)) |
         DIV (p,q) -> (fun x->((cal p) x)/((cal q) x)) |
         SIGMA (p,q,r) -> raise (Failure "impossible"))
       in
       let rec sigma v1 v2 =
         if v1>v2 then 0
         else if v1=v2 then ((cal n3) v1)
         else (((cal n3) v1) + (sigma (v1+1) v2))
       in (sigma (oper n1) (oper n2)))
    in
    match exp with
    X -> 0 |
    INT a -> a |
    ADD (a,b) -> (oper (ADD (a,b))) |
    SUB (a,b) -> (oper (SUB (a,b))) |
    MUL (a,b) -> (oper (MUL (a,b))) |
    DIV (a,b) -> (oper (DIV (a,b))) |
    SIGMA (a,b,c) -> (oper (SIGMA (a,b,c)));;
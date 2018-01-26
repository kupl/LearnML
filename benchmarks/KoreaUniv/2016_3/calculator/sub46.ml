
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec sigma : int * int * (int->int) -> int
  = fun (n1, n2, func) ->
    if n1 > n2 then 0
    else if n1 = n2 then func n1
    else (func n1) + (sigma (n1+1,n2,func));; 

  let rec etof : exp -> (int -> int)
  = fun e ->
      match e with
  | X -> (fun x -> x)
  | INT n -> (fun x -> n)
  | ADD (exp1, exp2) -> (fun x -> ((etof exp1) x) + ((etof exp2) x))
  | SUB (exp1, exp2) -> (fun x -> ((etof exp1) x) - ((etof exp2) x))
  | MUL (exp1, exp2) -> (fun x -> ((etof exp1) x) * ((etof exp2) x))
  | DIV (exp1, exp2) -> (fun x -> ((etof exp1) x) / ((etof exp2) x))
  | SIGMA (exp1, exp2, exp3) -> (fun x -> sigma( (etof exp1) x, (etof exp2)x, (etof exp3)));;

  let rec calculator : exp -> int
  = fun exp -> match exp with
    | X -> raise (Failure "error : free-variable")
    | INT a -> a
    | ADD (INT n1, INT n2) -> n1 + n2
    | ADD (exp1, exp2) -> (calculator exp1) + (calculator exp2)
    | SUB (INT n1, INT n2) -> n1 - n2
    | SUB (exp1, exp2) -> (calculator exp1) - (calculator exp2)
    | MUL (INT n1, INT n2) -> n1 * n2
    | MUL (exp1, exp2) -> (calculator exp1) * (calculator exp2)
    | DIV (INT n1, INT n2) -> if n2 = 0 then raise (Failure "DIVISION ERROR") else n1/n2
    | DIV (exp1, exp2) -> (calculator exp1) / (calculator exp2)
    | SIGMA (INT n1, INT n2, exp1) -> sigma (n1, n2, (etof exp1))
    | SIGMA (exp1, exp2, exp3) -> sigma (calculator exp1, calculator exp2, etof exp3);;
  
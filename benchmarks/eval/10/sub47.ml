type expr = NUM of int
	    | PLUS of expr * expr
	    | MINUS of expr * expr
	    | MULT of expr * expr
	    | DIVIDE of expr * expr
	    | MAX of expr list;;

let rec eval2 e =
  match e with
      NUM n -> n
    | PLUS(e1, e2) -> (eval2 e1) + (eval2 e2)
    | MINUS(e1, e2) -> (eval2 e1) - (eval2 e2)
    | MULT(e1, e2) -> (eval2 e1) * (eval2 e2)
    | DIVIDE(e1, e2) -> (eval2 e1) / (eval2 e2);;

let rec maximum e l =
  match l with
      [] -> e
    | head :: tail -> if e >= (eval2 head) then (maximum e tail) else (maximum (eval2 head) tail);;

let eval e =
  match e with
    | NUM n -> (eval2 e)
    | PLUS(e1, e2) -> (eval2 e)
    | MINUS(e1, e2) -> (eval2 e)
    | MULT(e1, e2) -> (eval2 e)
    | DIVIDE(e1, e2) -> (eval2 e)
    | MAX [] -> 0
    | MAX(head :: tail) -> (maximum (eval2 head) tail);;

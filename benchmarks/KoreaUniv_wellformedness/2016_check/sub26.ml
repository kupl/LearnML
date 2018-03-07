
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
;;

let rec evalCheck1 ex bd = 
match ex with
  |V x -> false
  |P (arg, body) -> if arg = bd then true else (evalCheck1 body bd)
  |C (a, b) -> (evalCheck1 a bd) || (evalCheck1 b bd)
;;
let rec evalCheck2 ex bd =
match bd with
  |V x -> (evalCheck1 ex x)
  |P (arg, body) -> (evalCheck2 ex body)
  |C (a, b) -> (evalCheck2 ex a) && (evalCheck2 ex b)
;;

  let rec check : exp -> bool
  = fun exp -> match exp with
    |V x -> false
    |P (arg, body) -> (evalCheck2 exp body)
    |C (a, b) -> (check a) && (check b)
;;

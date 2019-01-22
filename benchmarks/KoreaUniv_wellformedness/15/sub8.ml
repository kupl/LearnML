type exp = 
  | V of name
  | P of name * exp
  | C of exp * exp
and name = string 

let rec checkWellFormed expression checkList = 
    match expression with 
    | V (u) -> List.mem u checkList
    | P (u,v) -> checkWellFormed v (u::checkList)
    | C (u,v) -> (checkWellFormed u checkList) && (checkWellFormed v checkList)

let check: exp -> bool = fun expression -> checkWellFormed expression []

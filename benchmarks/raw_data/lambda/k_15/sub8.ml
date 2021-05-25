type lambda = 
  | V of name
  | P of name * lambda
  | C of lambda * lambda
and name = string 

let rec checkWellFormed lambdaression checkList = 
    match lambdaression with 
    | V (u) -> List.mem u checkList
    | P (u,v) -> checkWellFormed v (u::checkList)
    | C (u,v) -> (checkWellFormed u checkList) && (checkWellFormed v checkList)

let check: lambda -> bool = fun lambdaression -> checkWellFormed lambdaression []

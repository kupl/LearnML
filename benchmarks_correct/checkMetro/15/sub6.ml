type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec check2 someMetro chkList = 
    match someMetro with 
    | V (u) -> List.mem u chkList
    | P (u,v) -> check2 v (u::chkList)
    | C (u,v) -> (check2 u chkList) && (check2 v chkList)

let check someMetro = check2 someMetro []

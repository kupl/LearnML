type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda

and var = string

let arealist : string list = []

let rec checkarea ((a : string list) , (b : lambda)) : bool =
  match b with
  | V nm -> List.mem nm a
  | P (nm, mt) -> checkarea(nm::a, mt)
  | C (m1, m2) -> checkarea(a, m1) && checkarea(a, m2)

let check (b: lambda) : bool =
  checkarea (arealist,b)
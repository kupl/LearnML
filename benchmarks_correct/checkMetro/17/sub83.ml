type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check (x: lambda): bool = 
  let rec checkRec (x: lambda) (y: var list): bool = match x with 
  | V a -> List.exists (fun x -> x = a) y
  | P (a, b) -> checkRec b (a :: y)
  | C(a, b) -> (checkRec a y) && (checkRec b y)
  in checkRec x []

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec checkHelp ((x: lambda), (l: 'a list)): bool =
  match x with
  | V s -> List.mem s l
  | C (lambda1, lambda2) -> (checkHelp (lambda1, l)) && (checkHelp (lambda2, l))
  | P (s, lambda) -> checkHelp (lambda, s::l)

let rec check (x: lambda): bool = 
  checkHelp (x, [])

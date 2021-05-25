type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let same y = fun x -> (x= y)

let rec checkName (x, y) = match y with
							|V a -> List.exists (same a) x
							|P (a, b) -> checkName(List.append x [a], b)
							|C (a, b) -> checkName(x, a) && checkName(x, b)

let rec check x = match x with
						|V a -> false
						|C (a, b) -> (check a) && (check b)
						|P (a, b) -> checkName ([a], b)


type exp =
|C of (exp * exp)
|P of (var * exp)
|V of var
and var = string

let rec scheck  : ((string list * exp) -> bool) = 
fun (nlist, m) -> if (nlist = []) then true
 else 
 (match m with 
|V n -> __list_exists__ (
fun x -> (n = x)) (nlist)
|P (n, m1) -> scheck (((n :: nlist), m1))
|C (m1, m2) -> (scheck ((nlist, m1)) && scheck ((nlist, m2))))

let rec check  : (exp -> bool) = 
fun m -> 
 (match m with 
|V n -> false
|P (n, m1) -> scheck (((n :: []), m1))
|C (m1, m2) -> (check (m1) && check (m2)))


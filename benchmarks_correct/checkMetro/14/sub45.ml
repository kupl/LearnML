type var = string
type lambda = V of var
|P of var * lambda
|C of lambda * lambda

let rec hasName(n, l) = match l with
|a::b -> (a=n)||hasName(n,b)
|[] -> false

let rec isMetro(m, l) = match m with
|V n -> hasName(n, l)
|P (n, nm) -> isMetro(nm, n::l)
|C (nm1, nm2) -> isMetro(nm1, l) && isMetro(nm2,l)

let check(m) = isMetro(m, [])

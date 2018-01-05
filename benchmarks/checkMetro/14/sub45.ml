type name = string
type metro = STATION of name
|AREA of name * metro
|CONNECT of metro * metro

let rec hasName(n, l) = match l with
|a::b -> (a=n)||hasName(n,b)
|[] -> false

let rec isMetro(m, l) = match m with
|STATION n -> hasName(n, l)
|AREA (n, nm) -> isMetro(nm, n::l)
|CONNECT (nm1, nm2) -> isMetro(nm1, l) && isMetro(nm2,l)

let checkMetro(m) = isMetro(m, [])

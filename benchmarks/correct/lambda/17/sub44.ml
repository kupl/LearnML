type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec checkImpl (m : lambda) (l : 'a list) : bool = 
    match m with
    | V n -> List.mem n l
    | P (n, mm) -> checkImpl mm (n::l)
    | C (m1, m2) -> (checkImpl m1 l) && (checkImpl m2 l)

let check (m : lambda) : bool = checkImpl m []

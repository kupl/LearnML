type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let rec check (m : lambda) =
let rec evalMetro ((areas: var list), (m_eval : lambda)) = match m_eval with
| V(svar) -> List.mem svar areas
| P(avar, m_prime) -> evalMetro((avar :: areas), m_prime)
| C(m1, m2) -> evalMetro(areas, m1) && evalMetro(areas, m2)
in evalMetro([], m)

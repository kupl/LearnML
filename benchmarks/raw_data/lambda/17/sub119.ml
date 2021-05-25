type lambda = V of var
                    | P of var * lambda
                    | C of lambda * lambda
and var = string

let rec stationinList : lambda * string list -> bool = fun c ->
    match c with
   |(V a, b) -> List.mem a b
   |(P(a,b), c) -> stationinList(b, a::c)
   |(C(a,b),c) -> stationinList(a,c) && stationinList(b,c)
    
let rec check : lambda -> bool = fun c ->
    match c with
    |a -> stationinList(a, [])
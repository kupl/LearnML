type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec checkInner: lambda -> string list -> bool =
    fun m sl ->
        match m with
        | V n -> (List.mem n sl)
        | P (n, m) -> (checkInner m (n::sl))
        | C (m1, m2) -> (checkInner m1 sl)&&(checkInner m2 sl)

let check: lambda -> bool =
    fun m ->
        (checkInner m [])

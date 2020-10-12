type lambda = V of var
        | P of var * lambda
        | C of lambda * lambda
and var = string

let compareList (var, l) =
        List.mem var l

let rec checkTail (lambda, l) = match lambda with
        V (x) -> compareList(x, l)
        | P (x, y) -> checkTail(y, x::l)
        | C (x, y) -> checkTail(x, l) && checkTail(y, l)

let check lambda = let l = [] in match lambda with
         V (x) -> compareList(x, l)
        | P (x, y) -> checkTail(y, x::l)
        | C (x, y) -> checkTail(x, l) && checkTail(y, l)

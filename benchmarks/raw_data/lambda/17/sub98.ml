type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check (met: lambda): bool = 
    let rec check' (met: lambda) (lst: string list) = match met with
        | V(n) -> List.exists (fun x -> x = n) lst
        | P(n, met) -> check' met (n :: lst)
        | C(met1, met2) -> 
                (check' met1 lst) && (check' met2 lst)
    in
    check' met []

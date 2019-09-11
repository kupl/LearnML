type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check: lambda -> bool = fun met ->
    let rec saveCheck : lambda * var list -> bool = 
        fun (m, l) -> match m with
        | V x -> List.mem x l
        | P (n, m1) ->  saveCheck(m1, n::l)
        | C (m1, m2) -> saveCheck(m1, l) && saveCheck(m2, l)
    in saveCheck(met, [])

type lambda =
    | V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string

let rec check: lambda -> bool = fun (lambda) ->
    let rec checkArea: string list * lambda -> bool = fun (nl, m) ->
        match m with
        | V s ->
                let rec isExist: string list * string -> bool = fun (str_list, str) ->
                    match str_list with
                    | [] -> false
                    | e::e_list -> if e = str then true else isExist (e_list, str)
                in
                isExist (nl, s)
        | P (a, b) -> checkArea (a::nl, b)
        | C (a, b) -> checkArea (nl, a) && checkArea (nl, b)
    in
    match lambda with
    | V n -> false
    | P (n, m) -> checkArea ([n], m)
    | C (m1, m2) -> (check m1) && (check m2)

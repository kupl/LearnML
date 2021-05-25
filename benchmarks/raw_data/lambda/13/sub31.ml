type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec check met =
    let rec aux m ids =
        match m with
        | V id -> List.mem id ids
        | P (id, m') -> aux m' (id::ids)
        | C (m1, m2) -> (aux m1 ids) && (aux m2 ids) 
		in aux met []

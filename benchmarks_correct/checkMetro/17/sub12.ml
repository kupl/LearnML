type lambda = 
    | V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string

let check m = 
    let rec helper m area_li = match m with
        V n -> List.exists (fun x -> x = n) area_li
        | P (n, m) -> helper m (n :: area_li)
        | C (m1, m2) -> (helper m1 area_li) && (helper m2 area_li)
    in
    helper m [] 

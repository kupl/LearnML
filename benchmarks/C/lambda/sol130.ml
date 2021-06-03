type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec subCheckMetro areaList mtr = 
        match mtr with
        | V var -> (List.mem var areaList)
        | P (var, lambda) -> (subCheckMetro (areaList @ [var]) lambda)
        | C (m1, m2) -> (subCheckMetro areaList m1) && (subCheckMetro
        areaList m2)

let rec check mtr = 
        match mtr with
        | V _ -> false
        | C (a, b) -> (check a) && (check b)
        | P (var, lambda) -> (subCheckMetro [var] lambda)
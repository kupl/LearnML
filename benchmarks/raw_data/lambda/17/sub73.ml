type lambda =
    |V of var
    |P of var * lambda
    |C of lambda * lambda
and var = string

(*station list?*)
let rec checkArray (n, arr1, arr2) = 
    match arr2 with
    |[] -> arr1
    |h::t ->    if n = h then checkArray(n, arr1, t)
                else checkArray(n, h::arr1, t)
let rec checkStation m = 
    match m with
    |V n -> n::[]
    |P (n, m1) -> (*n is inside m1*)
            let arr = checkStation m1 in
            checkArray(n, [], arr)
    |C (m1, m2) -> (*get stations in m1 and m2*)
            (checkStation m1)@(checkStation m2)

let rec check m =
    let result = checkStation m in
    if result = [] then true
    else false

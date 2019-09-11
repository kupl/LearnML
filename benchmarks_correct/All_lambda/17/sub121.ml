type lambda  = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec checkhelper(x, l) =
    match x with
    | V y -> (List.mem y l)
    | P (y, z) -> checkhelper(z, (y :: l))
    | C (y, z) -> (checkhelper(y, l) && checkhelper(z, l))

let check x =
    checkhelper(x, [])

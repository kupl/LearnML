type lambda =
    V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string

let check lambda =

    let rec lambdaWithList lambda mlist =
        match lambda with
        |V n -> (List.mem n mlist)
        |P (n,m) -> (lambdaWithList m ((n::[])@mlist))
        |C (m1,m2) -> (lambdaWithList m1 mlist) && (lambdaWithList m2 mlist) in
    
    lambdaWithList lambda []


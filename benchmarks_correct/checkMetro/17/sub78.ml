type lambda = V of var
           | P of var*lambda
           | C of lambda * lambda
and var = string
;;

let check (m : lambda) : bool = 
        let rec checkRec ((m : lambda), (a : string list)) : bool = 
                match m with
                | V n ->
                                (if List.exists (fun x -> x = n) a then true
                                else false)
                | P(n, m2) -> checkRec(m2, n::a)
                | C(m1, m2) -> checkRec(m1, a) && checkRec(m2, a)
        in
        checkRec (m, [])
;;

type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string
;;

let rec check(l, m) =
        match m with
        V a -> List.exists (fun n-> n=a) l
        |P(a, b) -> check(a::l, b)
        |C(a, b) -> check(l, a) && check(l, b)
;;

let check m =
        check([], m)
;;

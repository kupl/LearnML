type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string
;;

let rec check2(l, m) =
        match m with
        V a -> List.exists (fun n-> n=a) l
        |P(a, b) -> check2(a::l, b)
        |C(a, b) -> check2(l, a) && check2(l, b)
;;

let check m =
        check2([], m)
;;

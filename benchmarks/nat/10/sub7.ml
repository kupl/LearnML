type nat = ZERO | SUCC of nat
;;

let rec natadd (a, b) =
        match (a, b) with
        (ZERO, b) -> b
        |(a, ZERO) -> a
        |(SUCC(a), SUCC(b)) -> SUCC(SUCC(natadd(a, b)))
;;

let rec natmul (a, b) =
        match b with
        ZERO -> ZERO
        |SUCC(b) -> natadd(a, natmul(a,b))
;;

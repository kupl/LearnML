type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
        match (a, b) with
        | (ZERO, _) -> b
        | (SUCC (aa), _) -> SUCC (natadd (aa, b))
;;

let natmul (a, b) =
        match (a, b) with
        | (ZERO, _) -> ZERO
        | (a, ZERO) -> ZERO
        | (SUCC _, SUCC _) -> 
                        let rec loop (x, y) =
                                (match y with
                                | ZERO -> ZERO
                                | SUCC (yy) -> natadd(x, loop(x, yy)))
                        in
                        loop(a, b)
;;



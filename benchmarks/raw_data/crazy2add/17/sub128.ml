type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

(* exercise 3*)


let crazy2add (add1, add2) =
        let toint x =
               match x with
                | NIL -> (0, NIL)
                | ZERO(t) -> (0, t)
                | ONE(t) -> (1, t)
                | MONE(t) -> (-1, t)
        in
        let rec calculater (sadd1, sadd2) carry =
                let x = toint sadd1 in
                let y = toint sadd2 in
                let z = ((fst x) + (fst y) + carry) in
                if ((sadd1 == NIL) && (sadd2 == NIL) && (carry == 0)) then NIL
                else (
                        match (z mod 2) with
                        | 1 -> ONE(calculater (snd x, snd y) (z/2))
                        | 0 -> ZERO(calculater (snd x, snd y) (z/2))
                        | -1 -> MONE(calculater (snd x, snd y) (z/2))
                        | _ -> NIL
                )
        in
        calculater (add1, add2) 0



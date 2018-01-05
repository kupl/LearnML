(* exercise 2*)
type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let crazy2val crazy2in =
        let rec exp val2in n =
                match val2in with
                | NIL -> 0
                | ZERO(x) -> (exp x n*2)
                | ONE(x) -> (n + (exp x n*2))
                | MONE(x) -> ((exp x n*2) - n)
        in
        exp crazy2in 1

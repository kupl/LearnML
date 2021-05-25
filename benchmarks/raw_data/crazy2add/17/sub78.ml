type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;
type crazy2char = Z | O | M

let crazy2add ((cz1 : crazy2), (cz2 : crazy2)) : crazy2 = 
        let crazy2ToChar (c : crazy2) : crazy2char * crazy2 = 
                match c with
                | NIL -> (Z, NIL)
                | ZERO d -> (Z, d)
                | ONE d -> (O, d)
                | MONE d -> (M, d)
        in

        let crazy2addOne ((c1 : crazy2char), (c2 : crazy2char)) : crazy2char * crazy2char = 
                match(c1, c2) with
                | (M, M) -> (Z, M)
                | (M, O) -> (Z, Z)
                | (Z, c) -> (c, Z)
                | (c, Z) -> (c, Z)
                | (O, M) -> (Z, Z)
                | (O, O) -> (Z, O)
        in                
        
        let rec crazy2addRec ((c1: crazy2), (c2 : crazy2), (c : crazy2char)) : crazy2 = 
                match(c1, c2, c) with
                | (NIL, NIL, c) ->
                        (match c with
                        | Z -> NIL
                        | M -> MONE NIL
                        | O -> ONE NIL)
                | (_, _, _) -> 
                        (let (a1, b1) = crazy2ToChar c1 in
                        let (a2, b2) = crazy2ToChar c2 in 
                        let (a3, a4) = crazy2addOne(a1, a2) in
                        let (a5, a6) = crazy2addOne(a3, c) in
                        let (a7, _) = crazy2addOne(a4, a6) in

                        match a5 with
                        | Z -> ZERO (crazy2addRec(b1, b2, a7))
                        | O -> ONE (crazy2addRec(b1, b2, a7))
                        | M -> MONE (crazy2addRec(b1, b2, a7))
                        )
        in
        
        crazy2addRec(cz1, cz2, Z)
;;

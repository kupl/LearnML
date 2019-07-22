(*컴퓨터공학부/2011-11729/안진우/2-3*)

type crazy2 = NIL
        | ZERO of crazy2
        | ONE of crazy2
        | MONE of crazy2

type crazy2c = N
        | O 
        | M  

let rec crazy2add_h ((x: crazy2), (y: crazy2), (z: crazy2c)) : crazy2 =
        match (x,y,z) with
        | (NIL,_,_)    ->( match (y,z) with
                    | (NIL,_)    -> (match z with
                                | N      -> NIL
                                | O  -> ONE NIL
                                | M -> MONE NIL
                    )
                    | (ZERO b,_) -> (match z with
                                | N      -> ZERO(b) 
                                | O  -> ONE(b)
                                | M -> MONE(b)
                    )
                    | (ONE  b,_) -> (match z with
                                | N      -> ONE(b)
                                | O  -> ZERO(crazy2add_h(NIL, b, O)) 
                                | M -> ZERO(b) 
                    )
                    | (MONE b,_) -> (match z with
                                | N      -> MONE(b)
                                | O  -> ZERO(b)
                                | M -> ZERO(crazy2add_h(NIL, b, M))
                    )
        )
        | (ZERO a,_,_) -> (match (y,z) with
                    | (NIL,_)    -> (match z with
                                | N      -> ZERO(a) 
                                | O  -> ONE(a)
                                | M -> MONE(a)
                    )
                    | (ZERO b,_) -> (match z with
                                | N      -> ZERO(crazy2add_h(a, b, N))
                                | O  -> ONE(crazy2add_h(a, b, N))
                                | M -> MONE(crazy2add_h(a, b, N))
                    )
                    | (ONE  b,_) -> (match z with
                                | N      -> ONE(crazy2add_h(a, b, N))
                                | O  -> ZERO(crazy2add_h(a, b, O))
                                | M -> ZERO(crazy2add_h(a, b, N))
                    )
                    | (MONE b,_) -> (match z with
                                | N      -> MONE(crazy2add_h(a, b, N))
                                | O  -> ZERO(crazy2add_h(a, b, N))
                                | M -> ZERO(crazy2add_h(a, b, M))
                    )
        )
        | (ONE  a,_,_) -> (match (y,z) with
                    | (NIL,_)    -> (match z with
                                | N      -> ONE(a)
                                | O  -> ZERO(crazy2add_h(a, NIL, O)) 
                                | M -> ZERO(a)
                    )
                    | (ZERO b,_) -> (match z with
                                | N      -> ONE(crazy2add_h(a, b, N))
                                | O  -> ZERO(crazy2add_h(a, b, O))
                                | M -> ZERO(crazy2add_h(a, b, N))
                    )
                    | (ONE  b,_) -> (match z with
                                | N      -> ZERO(crazy2add_h(a, b, O))
                                | O  -> ONE(crazy2add_h(a, b, O))
                                | M -> ONE(crazy2add_h(a, b, N))
                    )
                    | (MONE b,_) -> (match z with 
                                | N      -> ZERO(crazy2add_h(a, b, N))
                                | O  -> ONE(crazy2add_h(a, b, N)) 
                                | M -> MONE(crazy2add_h(a, b, N))
                    )
        )
        | (MONE a,_,_) -> (match (y,z) with
                    | (NIL,_)    -> (match z with
                                | N      -> MONE(a)
                                | O  -> ZERO(a)
                                | M -> ZERO(crazy2add_h(a, NIL, M))
                    )
                    | (ZERO b,_) -> (match z with
                                | N      -> MONE(crazy2add_h(a, b, N))
                                | O  -> ZERO(crazy2add_h(a, b, N)) 
                                | M -> ZERO(crazy2add_h(a, b, M))
                    )
                    | (ONE  b,_) -> (match z with
                                | N      -> ZERO(crazy2add_h(a, b, N))
                                | O  -> ONE(crazy2add_h(a, b, N))
                                | M -> MONE(crazy2add_h(a, b, N))
                    )
                    | (MONE b,_) -> (match z with 
                                | N      -> ZERO(crazy2add_h(a, b, M))
                                | O  -> MONE(crazy2add_h(a, b, N))
                                | M -> MONE(crazy2add_h(a, b, M))
                    )
        )
 
let crazy2add ((x:crazy2), (y:crazy2)) : crazy2 =
        crazy2add_h(x, y, N)
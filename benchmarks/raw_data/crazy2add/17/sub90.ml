type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2sum ((c1 : crazy2), (c2 : crazy2), (cin : crazy2)) : crazy2 =
    match (c1, c2, cin) with
    | (_, _, NIL) -> NIL
    | (NIL, NIL, _) -> cin
    | (NIL, _, ZERO(_)) -> c2
    | (NIL, ZERO(c2m), ONE(_))      -> ONE  (crazy2sum(c1, c2m, ZERO(NIL)))
    | (NIL, ONE(c2m), ONE(_))       -> ZERO (crazy2sum(c1, c2m, ONE(NIL)))
    | (NIL, MONE(c2m), ONE(_))      -> ZERO (crazy2sum(c1, c2m, ZERO(NIL)))
    | (NIL, ZERO(c2m), MONE(_))     -> MONE (crazy2sum(c1, c2m, ZERO(NIL)))
    | (NIL, ONE(c2m), MONE(_))      -> ZERO (crazy2sum(c1, c2m, ZERO(NIL)))
    | (NIL, MONE(c2m), MONE(_))     -> ZERO (crazy2sum(c1, c2m, MONE(NIL)))
    | (_, NIL, ZERO(_)) -> c1
    | (ZERO(c1m), NIL, ONE(_))      -> ONE  (crazy2sum(c1m, c2, ZERO(NIL)))
    | (ONE(c1m), NIL, ONE(_))       -> ZERO (crazy2sum(c1m, c2, ONE(NIL)))
    | (MONE(c1m), NIL, ONE(_))      -> ZERO (crazy2sum(c1m, c2, ZERO(NIL)))
    | (ZERO(c1m), NIL, MONE(_))     -> MONE (crazy2sum(c1m, c2, ZERO(NIL)))
    | (ONE(c1m), NIL, MONE(_))      -> ZERO (crazy2sum(c1m, c2, ZERO(NIL)))
    | (MONE(c1m), NIL, MONE(_))     -> ZERO (crazy2sum(c1m, c2, MONE(NIL)))

    | (ZERO(c1m), ZERO(c2m), ZERO(_))   -> ZERO (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (ZERO(c1m), ZERO(c2m), ONE(_))    -> ONE  (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (ZERO(c1m), ZERO(c2m), MONE(_))   -> MONE (crazy2sum(c1m, c2m, ZERO(NIL)))

    | (ZERO(c1m), ONE(c2m), ZERO(_))    -> ONE  (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (ZERO(c1m), ONE(c2m), ONE(_))     -> ZERO (crazy2sum(c1m, c2m, ONE(NIL)))
    | (ZERO(c1m), ONE(c2m), MONE(_))    -> ZERO (crazy2sum(c1m, c2m, ZERO(NIL)))

    | (ZERO(c1m), MONE(c2m), ZERO(_))   -> MONE (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (ZERO(c1m), MONE(c2m), ONE(_))    -> ZERO (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (ZERO(c1m), MONE(c2m), MONE(_))   -> ZERO (crazy2sum(c1m, c2m, MONE(NIL)))

    | (ONE(c1m), ZERO(c2m), ZERO(_))    -> ONE  (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (ONE(c1m), ZERO(c2m), ONE(_))     -> ZERO (crazy2sum(c1m, c2m, ONE(NIL)))
    | (ONE(c1m), ZERO(c2m), MONE(_))    -> ZERO (crazy2sum(c1m, c2m, ZERO(NIL)))

    | (ONE(c1m), ONE(c2m), ZERO(_))     -> ZERO (crazy2sum(c1m, c2m, ONE(NIL)))
    | (ONE(c1m), ONE(c2m), ONE(_))  -> ONE  (crazy2sum(c1m, c2m, ONE(NIL)))
    | (ONE(c1m), ONE(c2m), MONE(_))     -> ONE  (crazy2sum(c1m, c2m, ZERO(NIL)))

    | (ONE(c1m), MONE(c2m), ZERO(_))    -> ZERO (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (ONE(c1m), MONE(c2m), ONE(_)) -> ONE  (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (ONE(c1m), MONE(c2m), MONE(_))    -> MONE (crazy2sum(c1m, c2m, ZERO(NIL)))

    | (MONE(c1m), ZERO(c2m), ZERO(_))   -> MONE (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (MONE(c1m), ZERO(c2m), ONE(_))    -> ZERO (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (MONE(c1m), ZERO(c2m), MONE(_)) -> ZERO (crazy2sum(c1m, c2m, MONE(NIL)))

    | (MONE(c1m), ONE(c2m), ZERO(_))    -> ZERO (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (MONE(c1m), ONE(c2m), ONE(_))     -> ONE  (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (MONE(c1m), ONE(c2m), MONE(_))    -> MONE (crazy2sum(c1m, c2m, ZERO(NIL)))

    | (MONE(c1m), MONE(c2m), ZERO(_)) -> ZERO (crazy2sum(c1m, c2m, MONE(NIL)))
    | (MONE(c1m), MONE(c2m), ONE(_))    -> MONE (crazy2sum(c1m, c2m, ZERO(NIL)))
    | (MONE(c1m), MONE(c2m), MONE(_)) -> MONE (crazy2sum(c1m, c2m, MONE(NIL)))

let crazy2add ((c1 : crazy2), (c2 : crazy2)) : crazy2 =
	crazy2sum (c1, c2, ZERO(NIL))
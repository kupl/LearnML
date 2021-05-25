type crazy2 =
    NIL
    | ZERO of crazy2
    | ONE of crazy2
    | MONE of crazy2

(* ex2.ml *)

(* exception 이름은 대문자로 시작!! *)
exception Error of string

let rec crazy2add (_c1,_c2) =
    let rec carry = function
    | (NIL,NIL,cr) -> (match cr with ZERO NIL -> NIL | _ -> cr)
    | (c1,NIL,cr) -> carry(c1,ZERO NIL,cr)
    | (NIL,c2,cr) -> carry(ZERO NIL,c2,cr)

    | (ZERO c1',ZERO c2',ZERO NIL) | (ZERO c1',ONE c2',MONE NIL) | (ZERO c1',MONE c2',ONE NIL) 
    | (ONE c1',ZERO c2',MONE NIL) | (ONE c1',MONE c2',ZERO NIL) | (MONE c1',ZERO c2',ONE NIL)
    | (MONE c1',ONE c2',ZERO NIL)
        -> ZERO(carry(c1',c2',ZERO NIL))

    | (ZERO c1',ZERO c2',ONE NIL) | (ZERO c1',ONE c2',ZERO NIL) | (ONE c1',ZERO c2',ZERO NIL)
    | (ONE c1',ONE c2',MONE NIL) | (ONE c1',MONE c2',ONE NIL) | (MONE c1',ONE c2',ONE NIL)
        -> ONE(carry(c1',c2',ZERO NIL))

    | (ZERO c1',ZERO c2',MONE NIL) | (ONE c1',MONE c2',MONE NIL) | (MONE c1',ZERO c2',ZERO NIL)
        -> MONE(carry(c1',c2',ZERO NIL))

    | (ZERO c1',ONE c2',ONE NIL) | (ONE c1',ZERO c2',ONE NIL) | (ONE c1',ONE c2',ZERO NIL)
        -> ZERO(carry(c1',c2',ONE NIL))

    | (ZERO c1',MONE c2',ZERO NIL) | (MONE c1',ONE c2',MONE NIL) | (MONE c1',MONE c2',ONE NIL)
        -> MONE(carry(c1',c2',ZERO NIL))

    | (ZERO c1',MONE c2',MONE NIL) | (MONE c1',ZERO c2',MONE NIL) | (MONE c1',MONE c2',ZERO NIL)
        -> ZERO(carry(c1',c2',MONE NIL))

    | (ONE c1',ONE c2',ONE NIL)
        -> ONE(carry(c1',c2',ONE NIL))

    | (MONE c1',MONE c2',MONE NIL)
        -> MONE(carry(c1',c2',MONE NIL))
    | _ -> raise (Error "carry error")
    in carry(_c1,_c2,ZERO NIL)
            

(* WITH INT
let rec crazy2add (_c1,_c2) =
    
    let rec carry = function
        (NIL,NIL,cr) -> (match cr with -1 -> MONE NIL | 0 -> ZERO NIL | 1 -> ONE NIL | _ -> raise error)
        | (c1,NIL,cr) -> carry (c1,ZERO(NIL),cr)
        | (NIL,c2,cr) -> carry (ZERO(NIL),c2,cr)
        | (c1,c2,cr) -> 
            (match value_c c1 with (i1,p1) ->
                (match value_c c2 with (i2,p2) ->
                    (match i1 + i2 + cr with
                        -3 -> MONE(carry(p1,p2,-1))
                        | -2 -> ZERO(carry(p1,p2,-1))
                        | -1 -> MONE(carry(p1,p2,0))
                        | 0 -> ZERO(carry(p1,p2,0))
                        | 1 -> ONE(carry(p1,p2,0))
                        | 2 -> ZERO(carry(p1,p2,1))
                        | 3 -> ONE(carry(p1,p2,1))
                    )))
    and value_c = function
        ZERO(c') -> (0,c')
        | ONE(c') -> (1,c')
        | MONE(c') -> (-1,c')
        | NIL -> raise error

    in carry (_c1,_c2,0)
*)


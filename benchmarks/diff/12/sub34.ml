type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let rec diff (ae,str) =
        match (ae,str) with
        |(CONST a, str) -> CONST 0
        |(VAR a, str) -> if(a=str) then CONST 1 else CONST 0
        |(POWER (a , n),str) -> if(a=str) then TIMES [CONST n;POWER(a,(n-1))] else CONST 0
        |(TIMES (hd::[]),str) -> (diff (hd, str))
        |(TIMES (hd::tl),str) -> (SUM (( TIMES ((diff (hd,str))::tl) ) :: [( TIMES ( hd :: [(diff ((TIMES tl),str))] ) )]))
        |(SUM (hd::[]),str) -> (diff (hd,str))
        |(SUM (hd::tl),str) -> SUM ((diff (hd,str))::[(diff ((SUM tl),str))])

type aexp = Const of int
        | Var of string
        | Power of string * int
        | Times of aexp list
        | Sum of aexp list

let rec diff (aexp,str) =
        match (aexp,str) with
        |(Const a, str) -> Const 0
        |(Var a, str) -> if(a=str) then Const 1 else Const 0
        |(Power (a , n),str) -> if(a=str) then Times [Const n;Power(a,(n-1))] else Const 0
        |(Times (hd::[]),str) -> (diff (hd, str))
        |(Times (hd::tl),str) -> (Sum (( Times ((diff (hd,str))::tl) ) :: [( Times ( hd :: [(diff ((Times tl),str))] ) )]))
        |(Sum (hd::[]),str) -> (diff (hd,str))
        |(Sum (hd::tl),str) -> Sum ((diff (hd,str))::[(diff ((Sum tl),str))])

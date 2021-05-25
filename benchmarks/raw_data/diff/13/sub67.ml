type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

exception InvalidArgument

let rec diff (aexpin,str) =
 match aexpin with
 | Const n -> Const 0
 | Var s -> if s=str then Const 1 else Const 0
 | Power(s,n) -> if s=str then
                   if n=0 then Const 0
                   else if n=1 then Const 1
                   else Times((Const n)::Power (s,n-1)::[])
                  else Const 0
 | Times([]) -> raise InvalidArgument
 | Sum([]) -> raise InvalidArgument
 | Times(hd::tl) -> if tl=[] then diff(hd,str)
                    else Sum( (Times( (diff(hd,str))::tl ))::(Times( hd::(diff(Times(tl),str))::[] ))::[] )
 | Sum(hd::tl) -> if tl=[] then diff(hd,str)
                  else Sum( (diff(hd,str))::(diff(Sum tl,str))::[] )

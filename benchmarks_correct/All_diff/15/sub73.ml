type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
  match aexp with
  | Const n -> Const 0
  | Var st -> if st = x then Const 1 else Const 0
  | Power (st, n) -> if st = x then Times [Const n; Power (st , (n - 1))]
                               else Const 0
  | Times [] -> Const 0
  | Times (hd :: []) -> Times [ diff (hd , x) ; Const 1]
  | Times (hd :: tl) -> let hdd = diff (hd , x)         in
                        let ttl = Times tl              in
                        let hdp = Times [hdd ; ttl]     in
                        let tlp = Times [hd; diff (ttl , x)]  in
                        Sum [hdp ; tlp]
  | Sum [] -> Const 0
  | Sum (hd :: []) -> Sum [(diff (hd , x)) ; Const 0]
  | Sum (hd :: tl) -> Sum [(diff (hd , x)) ; diff (Sum tl, x)]

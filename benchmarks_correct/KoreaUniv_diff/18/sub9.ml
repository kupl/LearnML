type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with
  | Const n -> Const 0
  | Var s -> if s <> x then Const 0
            else Const 1
  | Power (s,n) -> if s <> x then Const 0
                  else Times [Const n; Power (s,n-1)]
  | Times l -> (match l with
    |[] -> Const 0
    |hd::tl -> Sum [Times ((diff (hd,x))::tl) ; Times [hd; (diff (Times tl,x))] ]
  )
  | Sum l -> (match l with
    |[] -> Const 0
    |hd::tl -> Sum [diff (hd,x) ; diff ((Sum tl),x)]
  )
  ;;

  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list




  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->  (* TODO *)
  match exp with
  |Const n -> Const 0
  |Var x -> if x = var then Const 1 else Const 0
  |Power (x,integer) -> if x = var then Times [Const integer;Power(x,integer-1)] else Const 0
  |Sum aexplist ->
    (match aexplist with
    |hd::tl -> if tl != [] then Sum ( diff (hd,var) :: [diff (Sum tl, var)] ) else diff (hd,var)
    )
  |Times aexplist -> 
    (match aexplist with
    |hd::tl -> if tl != [] then Sum [Times [diff (hd,var);Times tl];Times[hd;diff(Times tl,var)]] 
                else diff (hd,var)
    )
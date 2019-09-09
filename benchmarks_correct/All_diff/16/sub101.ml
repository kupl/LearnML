
  type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
  match exp with
  |Const i -> Const 0
  |Var s -> if (s=var) then Const 1 else Const 0
  |Power(v, i) -> if (v=var) then Times [Const i; Times[Power(var,i-1)]] else Const 0
  |Times [] -> Const 0
  |Times (hd::tl) -> Sum [Times(diff (hd, var)::tl); Times[hd; diff(Times tl, var)]]
  |Sum [] -> Const 0
  |Sum lst         -> 
     begin match lst with
      |hd::[] -> diff(hd,var)
      |hd::tl -> Sum [diff(hd, var); diff(Sum tl, var)]
      |[]-> Const 0
    end
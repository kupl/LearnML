
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

 let rec diff : aexp * string -> aexp
  = fun (exp, var) -> (* raise NotImplemented  TODO *)
   match exp with
   | Const n -> Const 0
   | Var str -> if str = var then Const 1 else Const 0
   | Power(str,n) ->
      if str = var then(
      if n=0 then Const 0
      else if n=1 then Const 1
      else Times([Const n;Power(str,(n-1))])
      )
      else Const 0
   | Times(lst) ->(
      match lst with
      |[] -> Times([])
      |hd::tl ->
         if tl = [] then diff(hd,var)
         else Sum (Times(diff(hd,var)::tl)::[Times(hd::[diff(Times(tl),var)])])
      )
   | Sum (lst) ->
      match lst with
      |[] -> Sum([])
      |hd::tl -> Sum (diff(hd,var)::[diff(Sum(tl),var)])
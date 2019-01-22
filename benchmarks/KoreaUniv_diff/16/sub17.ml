
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff exp var = 
  match exp with
  | Const(i) -> Const(0)
  | Var(a) -> 
    if a=var then Const(1)
    else Const(0)
  | Power(a,i) ->
    if (a=var)=false then Const(0)
    else if i==0 then Const(0)
    else if i==1 then Var(a)
    else Times([Const i;Power(a, i-1)]);;
type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff(e, v) = 
  match e with
  |Const(a) -> Const(0)
  |Var(a) -> 
      if a = v then
        Const(1)
      else
        Const(0)
  |Power(a, n) ->
      if a = v then
        if n = 0 then 
          Const(0)
        else
          Times[Const(n);Power(a, n-1)]
      else
        Const(0)
  |Times(l) -> (
    match l with 
    |[] -> Const(1)
    |h::t -> 
        if diff(h,v) = Const(0) then
          Times([h;diff(Times(t),v)])
        else if diff(h,v) = Const(1) then
          Sum([ Times(t) ; Times([h;diff(Times(t),v)]) ])
        else
          Sum([ Times([diff(h,v)]@t) ;Times([h;diff(Times(t),v)]) ])
  )
  |Sum(l) -> (
    match l with
    |[] -> Const(0)
    |h::t -> 
        if diff(h,v) = Const(0) then
          diff(Sum(t),v)
        else
          Sum([diff(h,v) ; diff(Sum(t),v)])
  )

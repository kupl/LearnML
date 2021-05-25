
  (*Problem 4*)
  
  type aexp = 
  |Const of int
  |Var of string 
  |Power of string * int
  |Times of aexp list
  |Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (e,x) -> (match e with 
      |Const k -> Const 0
      |Var v -> if v = x then Const 1 else Const 0
      |Power (v,n) -> if v = x then 
      if n = 0 then Const 0
      else if n =1 then Const 1 
      else  Times [Const n; Power(v, n-1)]
      else Const 0
      |Times l -> (match l with 
        |hd ::[] -> diff (hd,x)
        |hd ::tl -> Sum[Times [diff(hd,x); Times tl]; Times [hd;diff(Times(tl),x)]])
      |Sum l -> match l with 
        |hd::[] -> diff(hd,x)
        |hd::tl -> Sum[diff(hd,x); diff(Sum(tl), x)])

    
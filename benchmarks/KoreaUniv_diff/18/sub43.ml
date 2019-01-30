type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  let tolst : aexp -> aexp list
  = fun exp ->
    match exp with
    |Times(lst) -> lst
    |Sum(lst) -> lst
    |_ -> [] in
  match exp with
    |Const(n) -> Const(0)
    |Var(chr) -> if chr = x then Const(1) else Const(0)
    |Power(chr, i) -> if chr = x then Times([Const(i);Power(chr, i - 1)]) else Const(0)
    |Sum(hd::tl) -> Sum( diff(hd,x)::(tolst (diff(Sum(tl),x)) ) )
    |Times(hd::tl) -> Sum ([ Times([diff(hd,x);Sum(tl)]) ; Times(  [hd;diff(Sum(tl),x)]  )  ])
    |_ -> Const(0);;
    
diff( Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1] , "x");;
diff( Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1] , "y");;
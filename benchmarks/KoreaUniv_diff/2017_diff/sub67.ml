(*problem 4*)

type aexp = 
|Const of int
|Var of string
|Power of string *int
|Times of aexp list
|Sum of aexp list

let rec  diff : aexp * string -> aexp
=fun(e,x) -> 
match e with
|Const a -> Const 0
|Var b -> if x = b then Const 1 else Const 0
|Power (b, c) -> if x=b then Times [Const c; Power(b,(c-1))]  else Const 0
|Times aelist -> 
  (match aelist  with 
  |[]-> Const 0
  |hd::tl -> Sum[(Times[hd;(diff((Times tl),x))]); (Times ((diff(hd,x))::tl))])
|Sum aexplist -> 
  match aexplist with 
  |[]->Const 0
  |hd::tl -> Sum [(diff (hd, x)); (diff((Sum tl), x))]

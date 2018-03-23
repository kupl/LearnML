(*problem 4*)
type aexp =
| Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff : aexp * string -> aexp = fun (e,x) ->
match e with
|Const i -> Const 0
|Var s ->
  if x=s then Const 1 else Const 0
|Power (s,n) ->
  if x=s then match n with
  |1 -> Const 1
  |0 -> Const 0
  |_ -> Times[Const n; Power (x,n-1)]
  else Const 0
|Times(l) ->
  (match l with
  |[] -> Const 0
  |hd::tl -> Sum [Times ([diff(hd, x)] @ tl); Times (hd::[diff(Times tl, x)])])
|Sum l -> 
  (match l with
  |[] -> Const 0
  |hd::tl -> Sum[diff (hd,x);diff (Sum tl,x)])

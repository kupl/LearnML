type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
match aexp with
Const n -> Const 0
|Var v -> if v=x then Const 1 else Const 0
|Power (v,n) -> 
  if v = x 
  then 
    if n=1 then Var v
    else Times[Const n; Power ("x",n-1)]
  else Const 0
|Times l -> 
  (
    match l with
    []->Times []
    |h::t -> 
    (match h,t with 
      _,[] -> diff (h,x)
      |Const 0,_->Const 0
      |_,_ -> Sum ( Times (diff (h,x) :: t) :: (Times ( h :: ( diff (Times t,x)::[]) ) ::[])  )
    )
  )
| Sum l ->
(
    match l with 
    []->Const 0
    |h::t -> if t = [] then diff (h,x) else Sum (diff (h,x) :: diff (Sum t,x) ::[]) 
)

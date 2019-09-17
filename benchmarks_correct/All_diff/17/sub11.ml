
(*problem4*)

type aexp =
|Const of int
|Var of string
|Power of string * int
|Times of aexp list
|Sum of aexp list;;

let rec diff :aexp * string -> aexp = fun(e,x)->
match e with
|Const a-> Const 0
|Var s -> if x = s then Const 1 else Const 0
|Power (s,i)-> if x = s then match i with
|1-> Const 1
|0 -> Const 0
|_ -> Times[Const i;Power(x,i-1)] else Const 0
|Times (l) -> (match l with
    |[]->Const 0
    |h::t -> Sum [Times ([diff(h,x)] @ t);Times (h::[diff(Times t,x)])])
|Sum l -> (match l with 
    |[] -> Const 0
    |h::t -> Sum[diff(h,x);diff(Sum t,x)]);;

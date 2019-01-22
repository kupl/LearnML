(*4*)
  
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp = fun (e,x) ->
  match e with
  Const n -> Const 0
 |Var x-> Const 1
 |Power(x,1) -> Times[Var "x";]
 |Power(x,e) ->Times[Const e ; Power("x",(e-1))]
 |Times[Const n; Power("x",1)]-> Times[Const(n)]
 |Times[Const n; Power("x",e)]-> Times[Const (n*e); Power("x",(e-1))] 
 |Times[Const n; Var("x")]->Times[Const(n)]
 |Times _ -> raise(Failure("Error"))

 |Sum [aexp] -> Sum [diff (aexp,"x")]
 |Sum l -> match l with
                |[] -> raise(Failure("Error"))
                |hd::tl-> Sum [diff(hd,"x") ; diff (Sum tl,"x")];;

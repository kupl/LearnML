
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
match (exp, var) with 
| (Const exp ,_) -> Const 0
| (Var "x", "x") ->  Const 1
| (Var "y", "x") -> Const 0
| (Times exp, "x") -> (match exp with 
                     | [] -> Times []
                     | [Const a]  ->Times [Const 0] 
                     | [Var "y"] -> Times [Const 0]
                     | [Var "x"] -> Times [Const 1]
                     | hd::tl -> Sum [Times [diff (hd,"x"); Times tl]  ;Times [hd; diff( Times tl,"x")]])
 
| (Power ("x", i), "x") -> (match i with 
                         | 1-> Const 1
                         | 2-> Times [Var "x"; Const 2]
                         |_ -> Times [Power ("x", i-1); Const i])
| (Sum exp, "x") -> (match exp with 
                         | [] -> Sum []
                         | hd::tl -> Sum [diff (hd ,"x") ; diff ( Sum tl, "x" )])
| (exp, _) -> exp;;

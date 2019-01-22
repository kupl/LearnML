
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list



let rec diff  : aexp * string -> aexp
  =fun (exp, var) ->
   match (exp, var) with
  |(Const exp,_) -> Const 0
  |(Var "x", "x") -> Const 1 
  |(Var "y", "x") -> Const 0
  |(Times exp,"x") ->(match (exp,"x") with
                      | ([],"x") -> Times[Const 0]
                      | ([Const a],"x")->Times[Const 0]
                      | ([Var "y"],"x")->Times[Const 0]
                      | ([Var "x"],"x")->Times[Const 1]
                      | (a::b,"x")->
                              Sum[Times[diff(a,"x");Times b] ;Times[a ;diff(Times b,"x")]]
                      | (exp,_) -> Times exp)
  |(Power("x",y),"x") ->(match y with 
                          |1 -> Const 1 
                          |2 -> Times[Var "x"; Const 2] 
                          |_ -> Times[Power("x",y-1) ; Const y])
  |(Sum exp, "x") -> (match (exp,"x") with
                       |([],"x")-> Sum[]
                       |(a::b,"x") ->Sum[diff (a,"x") ; diff (Sum b,"x")]
                       |(exp,_) -> Sum exp)
  |(exp,_) -> exp;;

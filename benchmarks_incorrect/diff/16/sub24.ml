type aexp = 
 | Const of int
 | Var of string
 | Power of string * int
 | Times of aexp list
 | Sum of aexp list

let rec diff : aexp * string -> aexp = 
fun (exp, env) ->   
match exp with
     |Const a -> Const 0
     |Var x -> if x = env then Const 1
               else Var x
     |Power (s, i) -> if s=env then Times[Const i;Power (s,i-1)]
                      else Power (s, i)

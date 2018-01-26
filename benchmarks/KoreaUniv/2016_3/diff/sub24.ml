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

       

     

type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let calculator exp= 
  let rec eval exp env =
        match (exp,env) with
  (X, []) -> 0
      | (X, hd::tl) -> hd

   | (INT n, _) -> n
   | (ADD (a,b),env) -> let v1 = eval a env in
                      let v2 = eval b env in
                          v1 + v2   

   | (SUB (a,b),env) -> let v1 = eval a env in
                      let v2 = eval b env in
                          v1 - v2 

   | (MUL (a,b),env) -> let v1 = eval a env in
                      let v2 = eval b env in
                           v1 * v2
 
   | (DIV (a,b),env) -> let v1 = eval a env in
                        let v2 = eval b env in
                               v1 / v2
   | (SIGMA (a,b,c),env) -> let v1 = eval a env in
                            let v2 = eval b env in
                              if v1 > v2 then 0
                              else
                               (eval c (v1::env)) + (eval (SIGMA ( INT (v1 +1), INT v2, c)) env)

       in

eval exp [] 



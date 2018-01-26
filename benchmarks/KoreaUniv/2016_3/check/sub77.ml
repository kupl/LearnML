(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
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
end 

 (* TODO *)




(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec weightB =
    fun exp->
     match exp with
     |(SimpleBranch (a,b), SimpleBranch (c,d))-> b+d 
     |(SimpleBranch (a,b), CompoundBranch (c,d))-> b+(weightB (d))
     |(CompoundBranch (a,b),  SimpleBranch (c,d))-> (weightB (b))+d 
     |(CompoundBranch (a,b), CompoundBranch (c,d))-> (weightB (b)) + (weightB (d))

  let balanced : mobile -> bool
  = fun mob->  (* TODO *)
    match mob with
    |(SimpleBranch(a,b),SimpleBranch(c,d))-> if (a*b) = (c*d) then true else false 
    |(SimpleBranch(a,b),CompoundBranch(c,d))-> if(a*b) =(c*(weightB (d))) then true else false
    |(CompoundBranch(a,b), SimpleBranch (c,d))-> if((a*(weightB (b))) = (c*d)) then true else false
    |(CompoundBranch(a,b), CompoundBranch(c,d))-> if((a*(weightB (b))) = (c*(weightB (d)))) then true else false

end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec sub =
      fun (exp,x) ->
          match (exp,x) with
           | (X,x) -> x 
           | (INT a,x) -> a
           | (ADD(a,b),x) ->(match (a,b) with
                        | (X,b) -> x+ sub (b,x)
                        | (a,X) -> x+ sub (a,x)
                        | (a,b) -> sub (a,x) + sub (b,x))
           | (SUB(a,b),x) -> (match (a,b) with
                        | (X,b) -> x- sub (b,x)
                        | (b,X) -> sub (b,x) - x 
                        | (a,b) -> sub (a,x) - sub (b,x))
           | (MUL(a,b),x) ->(match (a,b) with
                        | (X,b) -> x*(sub(b,x))
                        | (a,X) -> (sub(a,x))*x
                        | (a,b) -> (sub(a,x))*(sub(b,x)))
           | (DIV(a,b),x) -> (match (a,b) with
                        | (X,b) -> x/(sub(b,x))
                        | (a,X) -> (sub(a,x))/x
                        | (a,b) -> (sub(a,x))/(sub(b,x)))
           | (SIGMA(a,b,c),x) ->(match (a,b,c) with
                        | (X,b,c) -> if x = sub(b,x) then sub (c,x) 
                                    else (sub (c,sub (b,x))) + sub(SIGMA(INT x,SUB(b,INT 1),c),x)
                        | (a,X,c) -> if x = sub(a,x) then sub (c,x)
                                    else (sub (c,sub (X,x))) + sub(SIGMA(a,SUB(X,INT 1),c),x) 
                        | (a,b,X) -> if sub(a,x) = sub(b,x) then sub (c,x)
                                    else (sub (X,sub (b,x))) + sub(SIGMA(a,SUB(b, INT 1),X),x)
                        | (a,b,c) -> if (sub (a,x)) = (sub(b,x)) then sub (c,x) 
                                    else (sub (c,sub (b,x))) + sub(SIGMA(a,SUB(b, INT 1),c),sub (c,x)))
  let rec calculator : exp -> int
  = fun exp -> 
    match exp with
    | X -> raise (Failure "Lack of arguments")
    | INT a -> a
    | ADD (a,b) ->(match (a,b) with
                    | (INT a, INT b) -> a+b
                    |_ -> (calculator a) + (calculator b))
    | SUB (a,b) ->(match (a,b) with
                    | (INT a, INT b) -> a-b
                    |_ -> (calculator a) - (calculator b))
    | MUL (a,b) ->(match (a,b) with
                    | (INT a, INT b) -> a * b
                    |_ -> (calculator a) * (calculator b))
    | DIV (a,b) ->(match (a,b) with
                    | (INT a, INT b) -> a / b
                    |_ -> (calculator a) / (calculator b))
    | SIGMA (a,b,c) ->(match (a,b,c) with
                    |(INT a, INT b, c) 
                    -> if a = b then sub (c,b) 
                    else (sub (c,b)) + calculator(SIGMA(INT a ,INT (b-1),c))
                    |(a,b,SIGMA(x,y,z)) 
                    -> if (calculator a) = (calculator b) then sub(SIGMA(x,y,z),(calculator b))
                      else (sub (SIGMA(x,y,z),(calculator b)) + calculator(SIGMA(a,SUB(b,INT 1),SIGMA(x,y,z))))
                    |_-> if (calculator a) = (calculator b) then sub (c,(calculator b)) 
                        else (sub (c,(calculator b)) + calculator(SIGMA(a,SUB(b,INT 1),c))))
end
(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
    
let rec check : exp -> bool
   = fun exp -> 
   match exp with
   |P(a, b) ->true
   |V a -> true
   |C(a,b) -> true
end

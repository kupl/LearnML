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

end 
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

  let rec  weight : mobile  -> int 
  = fun mob  ->
  match mob  with  
  | (SimpleBranch (a,b), SimpleBranch (c,d ))-> b+d
  | (CompoundBranch (a,b), SimpleBranch (c,d))-> (weight b) + d
  | (SimpleBranch (a,b), CompoundBranch (c,d))-> b+ (weight d)
  | (CompoundBranch (a,b), CompoundBranch (c,d))-> (weight b) + (weight d)
 
  let balanced: mobile -> bool
  = fun mob ->
  match mob with 
  | (SimpleBranch (a,b), SimpleBranch (c,d ))-> if a*b=c*d then true else false
  | (CompoundBranch (a,b), SimpleBranch (c,d))->if a*(weight b)= c*d then true else false
  | (SimpleBranch (a,b), CompoundBranch (c,d))-> if a*b= c*(weight d) then true else false
  | (CompoundBranch (a,b), CompoundBranch (c,d))-> if a*(weight b)= c*(weight d) then true else false
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

 
let rec cal =
   fun (exp, x)->
      match (exp, x) with
      | (X,x)->x
      | (INT a,x) -> a
      | (ADD (a,b), x) -> (match (a, b) with 
                     | (X, b)-> x + cal (b,x)
                     | (a, X)-> x + cal (a,x)
                     | (a, b)-> cal (a,x) + cal(b,x)) 
      | (SUB (a,b), x) -> (match (a, b) with
                     | (X, b)-> x - cal (b,x)
                     | (a, X)-> x - cal (a,x)
                     | (a, b)-> cal (a,x) - cal(b,x)) 
      | (MUL (a,b), x) -> (match (a, b) with
                     | (X, b)-> x * cal (b,x)
                     | (a, X)-> x * cal (a,x)
                     | (a, b)-> cal (a,x) * cal(b,x)) 
      | (DIV (a,b), x) -> (match (a, b) with
                     | (X, b)-> x / cal (b,x)
                     | (a, X)-> x / cal  (a,x)
                     | (a, b)-> cal (a,x) / cal (b,x)) 
      | (SIGMA (a,b,c), x)->(match a,b,c with
                     | X, b, c-> if x= cal(b,x) then cal (c,x)
                                 else cal (c, (cal (b,x))) + cal (SIGMA (INT x, SUB(b, INT 1),c),x)
                     | a, X, c-> if x= cal (a,x) then cal (c,x)
                                 else cal( c, (cal (X, x))) + cal (SIGMA (a, SUB(X, INT 1),c),x)
                     | a, b, X-> if cal (a,x) = cal (b,x)  then cal (b,x)
                                 else cal (X, cal(b,x))+ cal (SIGMA (a, SUB(b, INT 1),X),x))

  let rec calculator : exp -> int
  = fun exp -> 
  match exp with 
  | X-> raise (Failure "can't be calculated") 
  | (INT n)-> n
  | (ADD (a, b))-> (match a,b with 
                  | INT a, INT b-> a+b
                  | INT a, b-> a+ (calculator b)
                  | a, INT b -> (calculator a) + b
                  | a , b -> (calculator a) + (calculator b))
  |( SUB (a, b)) ->(match a,b with
                  | INT a, INT b-> a - b 
                  | INT a, b-> a - (calculator b) 
                  | a, INT b -> (calculator a) - b
                  | a , b -> (calculator a) - (calculator b))  
  | (MUL (a, b))-> (match a,b with
                  | INT a, INT b-> a * b
                  | INT a, b-> a * (calculator b) 
                  | a, INT b -> (calculator a) *  b
                  | a , b -> (calculator a) * (calculator b))
  | (DIV (a, b)) -> (match a,b with
                  | INT a, INT b-> a / b
                  | INT a, b-> a / (calculator b) 
                  | a, INT b -> (calculator a) / b
                  | a , b -> (calculator a) / (calculator b))
  | (SIGMA (a,b,c))-> (match a,b,c with 
                  | INT a, INT b, c -> if a=b then cal (c,b) else (cal (c,b))+ calculator (SIGMA (INT a, INT (b-1),c ))
                  | a,b, SIGMA (x,y,z) -> if (calculator a) = (calculator b) then cal (SIGMA (x,y,z), (calculator b)) else cal (SIGMA( x,y,z), (calculator b) ) + calculator( SIGMA (a, SUB (b, INT 1), SIGMA ( x,y,z))) 
                  |_-> if (calculator a) = (calculator b) then cal (c,(calculator b)) else (cal(c,(calculator b))+ calculator (SIGMA (a, SUB( b, INT 1),c))))
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

(*  let add "n" lst =
  match lst with 
 |[]-> "n"::lst
 |_->  "n"::lst
  
  let rec find "n" lst =
  match lst with 
  | []-> false
  | hd::tl ->  if hd = "a" then true else (find "a" tl) 
*)
  let rec  check : exp -> bool
	= fun exp -> 
  match exp with 
|P ("a", e) ->(match e with 
               |V "b" -> if "b"= "a" then true else false
               |C(V "b", V"c")-> if "b"="a"&&"c"="a" then true else false
               |P("b", V "c") -> if "c"="b"||"c"="a" then true else false) 
|_ -> false 
  (* match exp with 
| (P ("a", exp))-> (match exp with 
                    | V "n" -> let lst= add "a" lst in 
                                  if (find "n" lst = true) then true else false
                    | P("n", V "m")-> check P("n", "m") 
                    | C( V "n", V "m")-> if (find "n" lst = true) && ( find "m" lst= true ) then true else false)
*)
end

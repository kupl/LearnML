(*
1. You can modify the given function specifications as recursive.
2. Do not modify the function names or types.
3. It is free to define any helper functions.
*)

(*2014130211 Donghyun Koh*)


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

  let diff : aexp * string -> aexp
    = fun (exp, var) -> raise NotImplemented 

(*
let rec calc x =
match x with
|


in match (exp, var) with
|Const(i) -> Const(i)
|Var(s) -> Var(s)
|Power(
*)

end




(*********************)
(*     Problem 2     *)
(*********************)

(*Warning! Need Further TEST!!!! - Prototype *)

module Problem2 = struct
  type mobile = branch * branch
  and branch = 
      | SimpleBranch of length * weight
      | CompoundBranch of length * mobile
  and length = int
  and weight = int



  let balanced : mobile -> bool
    = fun mob -> 
      let rec mfilter b = (*make mobile has two simple branch -- return sum of below branches' weight*)
        match b with
          |(SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
          |(SimpleBranch(l1,w1), CompoundBranch(l2,m)) -> w1 + (mfilter m)
          |(CompoundBranch(l1,m), SimpleBranch(l2,w2)) -> (mfilter m) + w2
          |(CompoundBranch(l1,m1), CompoundBranch(l2,m2)) -> (mfilter m1)+ (mfilter m2)

      in match mob with(*top level of mobile*)
        |(SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> (l1*w1) = (l2*w2)
        |(SimpleBranch(l1,w1), CompoundBranch(l2,m)) -> (l1*w1) = (l2*(mfilter m))
        |(CompoundBranch(l1,m), SimpleBranch(l2,w2)) -> (l1*(mfilter m)) = (l2*w2)
        |(CompoundBranch(l1,m1), CompoundBranch(l2,m2)) -> (l1*(mfilter m1)) = (l2*(mfilter m2))


(*
let mo = (CompoundBranch (3, (CompoundBranch (2, (SimpleBranch (1, 1), SimpleBranch (1, 1))), SimpleBranch (1, 4))), SimpleBranch (6, 3)) 
*)
end




(*********************)
(*     Problem 3     *)
(*********************)

(*Warning! Need Further TEST!!!! - Prototype*)

module Problem3 = struct
  type exp =
      | X
      | INT of int
      | ADD of exp * exp
      | SUB of exp * exp
      | MUL of exp * exp
      | DIV of exp * exp
      | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
    = fun exp ->
      let rec xfilter x e =(*calculation with X-exp-, return exp*)
        match (x,e) with
          |(x,X) -> x

          |(x,INT(n)) -> INT(n)
          |(x,ADD(e1, e2)) -> ADD((xfilter x e1) , (xfilter x e2))
          |(x,SUB(e1, e2)) -> SUB((xfilter x e1) , (xfilter x e2))
          |(x,MUL(e1, e2)) -> MUL((xfilter x e1) , (xfilter x e2))
          |(x,DIV(e1, e2)) -> DIV((xfilter x e1) , (xfilter x e2))

          |(x,SIGMA(e1,e2,e3)) -> (*change SIGMA to ADD *)
              if e1 == e2 then (xfilter e1 e3) 
              else ADD((xfilter e1 e3),(SIGMA((ADD(e1, INT(1))),e2,e3)))

      in let rec sfilter e1 e2 e3 = (*change SIGMA to ADD, return exp *)
        if (calculator e1) = (calculator e2) then (xfilter e1 e3) 
        else ADD((xfilter e1 e3),(SIGMA( (ADD( INT(calculator e1),  INT(1)) ),e2,e3)))


      in match exp with (*formal calculation, return int*)
        |INT(n)-> n

        |ADD(e1, e2) -> ((calculator e1) + (calculator e2))
        |SUB(e1, e2) -> ((calculator e1) - (calculator e2))
        |MUL(e1, e2) -> ((calculator e1) * (calculator e2))
        |DIV(e1, e2) -> ((calculator e1) / (calculator e2))

        |SIGMA(e1,e2,e3) ->  calculator(sfilter e1 e2 e3)
        |X -> 0


(*
let ex = SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))
*)
end


(*********************)
(*     Problem 4     *)
(*********************)

(*Warning! Need Further TEST!!!! - Prototype *)

module Problem4 = struct
  type exp =
      | V of var
      | P of var * exp
      | C of exp * exp
  and var = string
  let rec check : exp -> bool
    = fun exp -> 

      (*let lst = [](*default list*)*)



      let rec finalcheck e l =(*check variable form by list, return bool*)
        match l with
          |[] -> false
          |hd::tl -> if hd == e then true else (finalcheck e tl)


      in let rec checklst e l = (*make variable list, check form by procedure list, return bool *)
        match e with
          |P(v,e)-> (checklst e (l@[v]))
          |C(e1,e2) -> (checklst e1 l)&&(checklst e2 l)

          |V(v) -> (finalcheck v l)


      (*match l with
        |[] -> false
        |hd::tl -> if hd == v then true else (checklst e tl)*)

      in (checklst exp [])

(*

let ex11 = P ("a", V "a")
let ex12 = P ("a", P ("a", V "a"))
let ex13 = P ("a", P ("b", C (V "a", V "b")))
let ex14 = P ("a", C (V "a", P ("b", V "a")))
let ex21 = P ("a", V "b")
let ex22 = P ("a", C (V "a", P ("b", V "c")))
let ex23 = P ("a", P ("b", C (V "a", V "c")))
*)
end


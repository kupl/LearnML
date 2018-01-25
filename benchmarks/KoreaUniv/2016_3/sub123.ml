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

  (*test case*)
  let a1 = Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1]
  let a2 = Sum [Times [Const 2; Var "x"]; Const 2]
  let a3 = Sum [Times [Const 2; Power("x", 2); Var "y"]; Const 1]
  let a4 = Times [Power ("x", 2); Power ("x", 2); Var "x"]
  let a5 = Times [Power ("x", 2); Power ("y", 2); Var "y"]

  let rec lengthOf l = match l with
  | [] -> 0
  | hd :: tl -> 1 + lengthOf tl

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
  | Const _ -> Const 0
  | Var x -> if (x = var) then Const 1 else Const 0
  | Power (x, p) ->
     (* p가 0이면 상수 *)
      if (x = var && p != 0) then Times [Const p; Power (x, p - 1)]
      else Const 0
  | Times e ->
      let rec timesDiff (Sum left, right, i) = match right with
      (* 원본 오른쪽, 미분 후 왼쪽 *)
      | [] -> Sum left
      | hd :: tl ->
        if i = 0 then Sum left
        else timesDiff (Sum (left @ [Times (diff (hd, var) :: tl)]), tl @ [hd], i - 1)
          in timesDiff (Sum [], e, lengthOf e)
  | Sum e ->
      let rec sumDiff (Sum left, right) = match right with
      (* 원본 오른쪽, 미분 후 왼쪽 *)
      | [] -> Sum left
      | hd :: tl -> sumDiff(Sum (left @ [diff (hd, var)]), tl)
   
    in sumDiff (Sum [], e)

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

  (*test case*)
  let s1 = SimpleBranch (2, 3)
  let s2 = SimpleBranch (3, 2)
  let m1 = (s1, s2)
  let s3 = CompoundBranch (4, m1)
  let s4 = SimpleBranch (4, 4)
  let s5 = SimpleBranch (4, 5)
  let m2 = (s3, s1)
  let m3 = (s3, s4)
  let m4 = (s5, s3)
  let m5 = (s3, s3)
  let m6 = (CompoundBranch (3, (CompoundBranch (2, (SimpleBranch (1, 1), SimpleBranch (1, 1))), SimpleBranch (1, 4))), SimpleBranch (6, 3))


  let rec getWeight : branch -> int
  = fun br -> match br with
  | SimpleBranch (l1, w1) -> w1
  | CompoundBranch (l1, m)
      -> match m with
      |(br1, br2) -> getWeight(br1) + getWeight(br2)

  let lengthOf : branch -> int
  = fun br -> match br with
  | SimpleBranch (l1, w1) -> l1
  | CompoundBranch (l1, m) -> l1


  let branchBalanced : mobile -> bool
  = fun mob -> match mob with
  | (br1, br2) -> (lengthOf br1) * (getWeight br1) = (lengthOf br2) * (getWeight br2)


  let rec balanced : mobile -> bool
  = fun mob -> match mob with
  | (br1, br2) -> match br1, br2 with
    | SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> (branchBalanced mob)
    | CompoundBranch (l1, m1), SimpleBranch (l2, w2) -> (balanced m1) && (branchBalanced mob)
    | SimpleBranch (l1, w1), CompoundBranch (l2, m2) -> (balanced m2) && (branchBalanced mob)
    | CompoundBranch (l1, m1), CompoundBranch (l2, m2) -> (balanced m1) && (balanced m2)



  

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

  (*test case*)
  let t1 = SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))
  let t2 = DIV(INT 5, INT 2)
  let t3 = ADD(t2, INT 1)
  let t4 = SIGMA(INT 1, INT 2, MUL(X, X))
  let t5 = SIGMA(INT 2, INT 4, t4)

  let zero = INT 0
  let rec solveTerm : exp -> exp -> int
  = fun term k -> match term with
  | X -> solveTerm k k
  | INT n -> n
  | ADD (a, b) -> (solveTerm a k) + (solveTerm b k)
  | SUB (a, b) -> (solveTerm a k) - (solveTerm b k)
  | MUL (a, b) -> (solveTerm a k) * (solveTerm b k)
  | DIV (a, b) -> (solveTerm a k) / (solveTerm b k)
  | SIGMA (a, b, term) -> if ((solveTerm a k) > (solveTerm b k)) then 0
       else solveTerm (SIGMA ((INT(solveTerm a k + 1)), b, term)) k + solveTerm term a

  let rec calculator : exp -> int
  = fun exp -> solveTerm exp zero

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


  (*test case*)
  let e1 = P ("a", V "a")
  let e2 = P ("a", e1)
  let e3 = P ("a", P ("b", C (V "a", V "b")))
  let e4 = P ("a", C (V "a", P ("b", V "a")))
  
  let e5 = P ("a", V "b")
  let e6 = P ("a", C (V "a", P ("b", V "c")))
  let e7 = P ("a", P ("b", C (V "a", V "c")))

  let e8 = V("a")		(* false *)
  let e9 = C(e8, e8)		(* false *)
  let e10 = C (e4, e5)		(* false *)
  let e11 = C (P ("a", V "b"), P ("b", V "a"))		(* false *)
  let e12 = C (e11, e11)		(* false *)
  let e13 = C (e3, e4)		(* true *)
  let e14 = C (e13, e13)		(* true *)
  let e15 = C (e12, e13)		(* false *)
  let e16 = C (e14, e14)		(* true *)

  let rec makePList exp pl = match exp with
  | V v -> pl
  | P (v, e) -> pl @ [v] @ makePList e pl
  | C (e1, e2) -> makePList e1 pl @ makePList e2 pl

  let rec makeVList exp vl = match exp with
  | V v -> vl @ [v]
  | P (v, e) -> vl @ makeVList e vl
  | C (e1, e2) -> makeVList e1 vl @ makeVList e2 vl

  let rec compareToPlist pl e = match pl with
  | [] -> false
  | hd :: tl -> (e = hd) || (compareToPlist tl e)

  let rec compareList pl vl = match vl with
  | [] -> true
  | hd :: tl -> (compareList pl tl) && (compareToPlist pl hd)



  let rec check : exp -> bool
  = fun exp -> match exp with
  | V v -> false
  | P (v, e)->
      let pl = makePList exp []
        in let vl = makeVList exp []
          in compareList pl vl
  | C (e1, e2) -> check e1 && check e2

end


(*
Hi all.

There are some points that you might have missed on the specification for HW3.


In the problem 1,

1) Your diff function should be able to work on multivariate functions. e.g) (Sum [Var "y", Power ("x", 2)], "y") can be an input.

2) As explained in the problem description, the representation of an expression can be more than one. Plus, the result for the same input can be also various. For example, the results of differentiating Power ("x", 3) with respect to "x" can be 3x^2 or 3*x*x, which are the same in mathematics but not in our aexp language. We will grade both the answers as correct, so you do not have to worry about the issue.

In the problem 4, V or C can be an input since they are also type of exp although only Procedures are introduced as example.


Thanks
*)

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

  let concat : aexp * aexp -> aexp
  = fun (exp1, exp2) ->
  (match (exp1, exp2) with  
  | (Sum l1, Sum l2) -> Sum (l1 @ l2)
  | (Times l1, Times l2) -> Times (l1 @ l2)
  | _ -> raise (Failure "Invalid operation"))

  let rec count_var : aexp * string -> int
  = fun (exp, var) ->
  (match exp with
  | Const n -> 0
  | Var str -> if str = var then 1 else 0
  | Power (str, n) -> if str = var then 1 else 0
  | Sum lst ->
    (match lst with
    | [] -> 0 
    | hd::tl -> if (count_var (hd, var)) =  1 then 1 else (count_var (Sum tl, var)))
  | Times lst ->
    (match lst with
    | [] -> 0
    | hd::tl -> (count_var (hd, var)) + (count_var (Times tl, var))))  
  
  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
  (match exp with
  | Const n -> Const 0
  | Var str -> if str = var then Const 1 else Const 0
  | Power (str, n) -> 
    if str = var then 
    (match n with
    | n when n = 1 -> Const 1
    | n when n = 0 -> Const 0
    | n when n = 2 -> Times [Const n ; Var str]
    | _ -> Times [Const n ; Power (str, n-1)])
    else Const 0
  | Sum lst -> 
    (match lst with
    | [] -> Sum [Const 0]
    | hd::tl -> if (diff (hd, var)) = Const 0 then (diff ((Sum tl), var)) else concat ((Sum [diff (hd, var)]), diff ((Sum tl), var)))
  | Times lst ->
    (match (count_var (Times lst, var)) with
    | 0 -> Const 0
    | 1 -> (match lst with
          | [] -> raise (Failure "Impossible reach")
          | hd::[] -> Times [diff (hd, var)]
          | hd::tl -> if (count_var (hd, var)) = 1 then concat (Times [diff (hd, var)], Times tl) 
                      else concat (Times [hd], diff (Times tl, var)))
    | _ -> (match lst with
          | [] -> Sum [Const 0]
          | hd::[] -> Sum [diff (hd, var)]
          | hd::tl -> if diff (hd, var) = Const 0 then Sum [Times[hd; diff (Times tl, var)]]
                      else if diff (hd, var) = Const 1 then Sum [Times tl; Times[hd; diff (Times tl, var)]]
                      else Sum [Times[diff (hd, var); Times tl]; Times[hd; diff (Times tl, var)]])))
end

(*
let a = Problem1.Const 5;;
let b = Problem1.Var "x";;
let c = Problem1.Power ("x", 4);;
let d = Problem1.Power ("y", 4);;
let e = Problem1.Power ("x", 0);;
let f = Problem1.Power ("x", -5);;
let g = Problem1.Power ("x", 1);;
let h = Problem1.Power ("y", -1);;
let s1 = Problem1.Sum [Problem1.Const 0];;
let s2 = Problem1.Sum [Problem1.Const 1];;
let s3 = Problem1.Sum [Problem1.Var "x"];;
let s4 = Problem1.Sum [Problem1.Var "x"; Problem1.Var "y"];;
let s5 = Problem1.Sum [Problem1.Power ("x", 2); Problem1.Const (-1)];;
let s6 = Problem1.Sum [Problem1.Power ("x", 2); Problem1.Var "x"; Problem1.Var "y"; Problem1.Const (-5); Problem1.Times[Problem1.Var "x"; Problem1.Power ("x", 3)]];;
let s7 = Problem1.Sum [Problem1.Power ("x", 4); Problem1.Power ("x", 2); Problem1.Power ("x", -7); Problem1.Const 5];;
let a1 = Problem1.Times [Problem1.Power ("h", 2); Problem1.Const 3; Problem1.Const 16; Problem1.Power ("x", 4) ;Problem1.Var "y"; Problem1.Power ("z", 3)];;
let a2 = Problem1.Times [Problem1.Power ("x", 2); Problem1.Sum [Problem1.Var "x"; Problem1.Const (-1)]; Problem1.Const 5];;
*)

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

  let rec balanced : mobile -> bool
  = fun mob -> 
  (match mob with 
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> l1 * w1 == l2 * w2
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> if (balanced m2) then l1 * w1 == l2 * (total_weight m2) else false
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> if (balanced m1) then l1 * (total_weight m1) == l2 * w2 else false
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> if (balanced m1) && (balanced m2) then l1 * (total_weight m1) == l2 * (total_weight m2) else false)

  and total_weight : mobile -> int
  = fun mob -> 
  (match mob with
  | (SimpleBranch (l1, w1), SimpleBranch(l2, w2)) -> w1 + w2 
  | (SimpleBranch (l1, w1), CompoundBranch(l2, m2)) -> w1 + (total_weight m2)
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> (total_weight m1) + w2
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> (total_weight m1) + (total_weight m2))
end

(*
let mob1 = (Problem2.CompoundBranch (3, (Problem2.CompoundBranch (1, (Problem2.SimpleBranch (1,2), Problem2.SimpleBranch (1,2))), Problem2.SimpleBranch (1, 4))), Problem2.SimpleBranch (6,4));;
let res1 = Problem2.balanced mob1;;

let mob2 = (Problem2.SimpleBranch (4, 4), Problem2.CompoundBranch (2, (Problem2.CompoundBranch (1, (Problem2.SimpleBranch (3, 1), Problem2.SimpleBranch (1, 3))), Problem2.SimpleBranch (1, 4))))
let res2 = Problem2.balanced mob2;;

let mob3 = (Problem2.CompoundBranch (1, (Problem2.SimpleBranch (1001, 2), Problem2.SimpleBranch (1001, 2))), Problem2.SimpleBranch (1, 4));;
let res3 = Problem2.balanced mob3;;

let mob4 = (Problem2.SimpleBranch (4, 4), Problem2.CompoundBranch (2, mob3));;
let res4 = Problem2.balanced mob4;;
*)

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

  let rec fillx : exp -> exp -> exp
  = fun exp1 exp2 ->
  (match exp1 with
  | X -> exp2
  | INT n -> INT n
  | ADD (e1, e2) -> ADD (fillx e1 exp2, fillx e2 exp2)
  | SUB (e1, e2) -> SUB (fillx e1 exp2, fillx e2 exp2)
  | MUL (e1, e2) -> MUL (fillx e1 exp2, fillx e2 exp2)
  | DIV (e1, e2) -> DIV (fillx e1 exp2, fillx e2 exp2)
  | SIGMA (e1, e2, e3) -> SIGMA (fillx e1 exp2, fillx e2 exp2, e3))

  let rec calculator : exp -> int
  = fun exp ->
  (match exp with
  | X -> raise (Failure "variable cannot be calculated")
  | INT n -> n
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> (calculator e1) / (calculator e2)
  | SIGMA (e1, e2, e3) ->
	if (calculator e1) > (calculator e2) then 0
	else if (calculator e1) == (calculator e2) then calculator (fillx e3 (INT (calculator e1)))
	else calculator (fillx e3 (INT (calculator e1))) + calculator (SIGMA (ADD (e1, INT 1), e2, e3)))
end

(*
let a1 = Problem3.INT 1;;
let a2 = Problem3.INT 2;;
let a3 = Problem3.INT 3;;
let a4 = Problem3.INT 4;;
let a5 = Problem3.INT 5;;
let sigma1 = Problem3.SIGMA (Problem3.INT 1, Problem3.INT 10, Problem3.SUB(Problem3.MUL(Problem3.X, Problem3.X), Problem3.INT 1));;
let sigma2 = Problem3.SIGMA (Problem3.INT 1, Problem3.INT 5, Problem3.SUB(Problem3.MUL(Problem3.X, Problem3.X), sigma1));;
let sigma = Problem3.SIGMA (Problem3.INT 1, sigma1, Problem3.SUB(Problem3.MUL(Problem3.X, Problem3.X), sigma1));;
let result = Problem3.calculator sigma2;;
let result2 = Problem3.calculator sigma1;;
let result3 = Problem3.calculator sigma;;
let s1 = Problem3.SIGMA (Problem3.INT 2, Problem3.X, Problem3.ADD( Problem3.X, Problem3.INT 1));;
let s2 = Problem3.SIGMA (Problem3.INT 1, Problem3.INT 10, s1);;
let result4 = Problem3.calculator s2;;
let s3 = Problem3.SIGMA (Problem3.INT 1, Problem3.INT 11, Problem3.MUL(Problem3.INT 5, Problem3.X));;
let result5 = Problem3.calculator s3;;
*)

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec is_V_here : string list -> string -> bool
  = fun lst var ->
  (match lst with
  | [] -> false
  | hd::tl -> if hd = var then true else is_V_here tl var)

  let rec check : exp -> bool
  = fun exp -> 
  (match exp with
  | V v -> false
  | P (v, e) -> sub_check [v] e
  | C (e1, e2) -> check e1 && check e2)

  and sub_check : string list -> exp -> bool
  = fun lst exp ->
  (match exp with
  | V v -> is_V_here lst v
  | P (v, e) -> sub_check (v::lst) e
  | C (e1, e2) -> (sub_check lst e1) && (sub_check lst e2))
end

(*
let a1 = Problem4.P ("a", Problem4.V "a");;
let a2 = Problem4.P ("a", Problem4.P ("a", Problem4.V "a"));;
let a3 = Problem4.P ("a", Problem4.P ("b", Problem4.C (Problem4.V "a", Problem4.V "b")));;
let a4 = Problem4.P ("a", Problem4.C (Problem4.V "a", Problem4.P ("b", Problem4.V "a")));;
let a5 = Problem4.P ("a", Problem4.V "b");;
let a6 = Problem4.P ("a", Problem4.C (Problem4.V "a", Problem4.P ("b", Problem4.V "c")));;
let a7 = Problem4.P ("a", Problem4.P ("b", Problem4.C (Problem4.V "a", Problem4.V "c")));;
*)

open Printf

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
          match exp with
          | Const n -> Const 0
          | Var v -> if v = var then Const 1 else Var v
          | Power (v, n) -> if v = var then Times [Const n; Power (v, n-1)] else Const 0
          | Times li -> 
            begin match li with
              | [] -> Const 0
              | hd::tl -> Sum [Times ((diff (hd, var))::tl); Times [hd; (diff (Times tl, var))]]
            end
          | Sum li -> 
            begin match li with 
              | [] -> Const 0
              | hd::tl -> Sum [diff (hd, var); diff (Sum tl, var)]
            end;;

end

open Problem1
let example = Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1];;
diff (example, "x");;
let yaho = Sum [Power ("y", 3); Power ("x", 2)];;
diff (yaho, "x");;
let times_test = Times [Times[Const 3; Var "x"]; Times[Const 3; Var "x"]];;
diff (times_test, "x");;



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

  let rec weightSum : mobile -> int
  = fun mob ->
     match mob with
     | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
     | (CompoundBranch (l1, sub_mob1), SimpleBranch (l2, w2)) -> weightSum(sub_mob1) + w2
     | (SimpleBranch (l1, w1), CompoundBranch (l2, sub_mob2)) -> w1 + weightSum(sub_mob2)
     | (CompoundBranch (l1, sub_mob1), CompoundBranch (l2, sub_mob2)) -> weightSum(sub_mob1) + weightSum(sub_mob2);;

  let rec balanced : mobile -> bool
  = fun mob ->
     match mob with
     | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if l1 * w1 == l2 * w2 then true else false
     | (CompoundBranch (l1, sub_mob1), SimpleBranch (l2, w2)) -> if balanced sub_mob1 && l1 * weightSum(sub_mob1) == l2 *w2 then true else false
     | (SimpleBranch (l1, w1), CompoundBranch (l2, sub_mob2)) -> if balanced sub_mob2 && l1 * w1 == l2 * weightSum(sub_mob2) then true else false
     | (CompoundBranch (l1, sub_mob1), CompoundBranch (l2, sub_mob2)) -> if balanced sub_mob1 && balanced sub_mob2 && l1 * weightSum(sub_mob1) == l2 * weightSum(sub_mob2) then true else false;;
end

open Problem2
let easy = (SimpleBranch (1, 4), CompoundBranch(2, (SimpleBranch(1, 1), SimpleBranch (1, 1))));;
weightSum easy;;
weightSum(SimpleBranch(1, 1), SimpleBranch (1, 1));;
balanced easy;;
let second_test = (CompoundBranch (2,
  (CompoundBranch (2, (SimpleBranch (1, 2), SimpleBranch (1, 2))),
     SimpleBranch (2, 4))),
      SimpleBranch (4, 4));;
balanced second_test;;
weightSum second_test;;
let exam_2 = (CompoundBranch (3,
  (CompoundBranch (2, (SimpleBranch (1, 1), SimpleBranch (1, 1))),
     SimpleBranch (1, 4))),
      SimpleBranch (6, 3));;
balanced exam_2;;


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

  let rec exp_x : exp -> int -> int
  = fun exp value ->
    match exp with
    | INT n -> n
    | X -> value
    | ADD (e1, e2) -> (exp_x e1 value) + (exp_x e2 value)
    | SUB (e1, e2) -> (exp_x e1 value) - (exp_x e2 value)
    | MUL (e1, e2) -> (exp_x e1 value) * (exp_x e2 value)
    | DIV (e1, e2) -> (exp_x e1 value) / (exp_x e2 value);;

  let rec calculator : exp -> int
  = fun exp -> 
    match exp with
    | INT n -> n
    | ADD (INT n1, INT n2) -> n1 + n2
    | SUB (INT n1, INT n2) -> n1 - n2
    | MUL (INT n1, INT n2) -> n1 * n2
    | DIV (INT n1, INT n2) -> n1 / n2
    | SIGMA (start_e, finish_e, func) ->
        let start = calculator start_e in 
        let finish = calculator finish_e in
        if start > finish then 0
        else if start == finish then (exp_x func start)
        else (exp_x func start) + calculator (SIGMA (INT (start+1), INT finish, func))
    |_ -> raise NotImplemented;;
end

open Problem3
let third_test1 = ADD (INT 1, INT 10);;
calculator third_test1;;
let third_test = SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1));;
calculator third_test;;
let third_test2 = SIGMA(INT 1, INT 10, ADD(MUL(X, X), ADD(X, X)));;
calculator third_test2;;


(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec result : exp -> var
  = fun exp ->
      match exp with
      | V var -> var
      | P (v, e) -> result(e)
      | C (e1, e2) -> result(e2)

  let rec check : exp -> bool
  = fun exp -> 
      match exp with
      | V var -> false
      | P (v1, V v2) -> if v1 = v2 then true else false
      | P (v1, P (v2, e2)) -> if check(P (v2, e2)) || v1 = result(e2) then true else false
      | P (v1, C (e1, e2)) -> if v1 = result(e2) then true else false
      |_ -> raise NotImplemented;;
end

open Problem4
let test1 = P ("a", V "a");;
check test1;;

let test2 = P ("a", P ("a", V "a"));;
check test2;;

let test3 = P ("a", P ("b", C (V "a", V "b")));;
check test3;;
let test4 = P ("a", C (V "a", P ("b", V "a")));;
check test4;;
let test5 = P ("a", V "b");;
check test5;;
let test6 = P ("a", C (V "a", P ("b", V "c")));;
check test6;;
let test7 = P ("a", P ("b", C (V "a", V "c")));;
check test7;;


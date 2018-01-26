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

  let rec diffhelp: (aexp * string) -> aexp
  = fun (exp, string) ->
  match exp with
  | Const i -> Const 0
  | Var str -> if (str = string) then Const 1
               else exp
  | Power (str, i) -> if (str = string) then Times [Const i; Power (string, i-1)]
                      else exp
  | Times [Const i; xp] -> Times [Const i; diffhelp (xp, string)]
  | Times [Power (str1,i1); Power (str2,i2)] ->
                      if (str1 = str2) 
                      then diffhelp (Power (str1, i1 + i2), string)
                      else Times [diffhelp ((Power (str1, i1), string)); 
                      diffhelp ((Power (str2, i2), string))]
  |_ -> exp ;;

  let rec clean: aexp -> aexp
  = fun exp ->
  match exp with
  | Times [Const 1; xp] -> xp
  | Times [Const i; Const 1] -> Const i
  | Times [Const i; Power (str, 1)] -> Times [Const i; Var str]
  | Times [Const i1; Times [Const i2; xp]] -> clean (Times [Const (i1 * i2); xp])
  |_ -> exp;;

 let rec listofdiff: (aexp list * string ) -> (aexp list)
 =fun (list, string) ->
 match list with
 | [] -> []
 | hd::tl -> if ((diffhelp (hd, string)) = Const 0) then listofdiff (tl, string) 
             else  [clean (diffhelp (hd, string))] @  listofdiff (tl, string);;

  let diff : aexp * string -> aexp
  = fun (exp, var) -> 
  match exp with
  | Sum l -> Sum (listofdiff (l, var))
  |_ -> clean (diffhelp (exp, var));; (* TODO *)
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

 let rec cal_weight: branch -> int
  = fun branch ->
  match branch with
  | SimpleBranch (l, w) -> w
  | CompoundBranch (l, (br1, br2)) -> (cal_weight br1) + (cal_weight br2);;

  let balanced : mobile -> bool
  = fun mob -> 
  match mob with
  | ((SimpleBranch (l1, w1)), (SimpleBranch (l2,w2))) -> if (l1 *w1) == (l2 *w2) then true
    else false
  | ((SimpleBranch (l1, w1)), (CompoundBranch (l2, (br1, br2)))) ->
     if (l1 * w1) == (l2 * cal_weight (CompoundBranch (l2, (br1, br2)))) then true
    else false
  | ((CompoundBranch (l1, (br1, br2))), (SimpleBranch (l2,w2))) ->
     if (l1 * cal_weight (CompoundBranch (l1, (br1, br2)))) == (l2 * w2) then true
    else false
  | ((CompoundBranch (l1, (br11, br12))), (CompoundBranch (l2, (br21, br22)))) ->
    if (l1 * cal_weight (CompoundBranch (l1, (br11, br12)))) ==
    (l2 * cal_weight (CompoundBranch (l2, (br21, br22)))) then true
    else false;;
 (* TODO *)
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

 let setx: (exp * int) -> int
  = fun (exp, int) ->
  match exp with
  | X -> int
  |_ -> raise (Failure ("no X to set value"));;

  let  rec calculate: (exp * int) -> int
  = fun (exp, int)  ->
  match exp with
  | X -> setx (exp, int)
  | INT n -> n
  | ADD (e1, e2) -> calculate (e1, int) + calculate (e2, int)
  | SUB (e1, e2) -> calculate (e1, int) - calculate (e2, int)
  | MUL (e1, e2) -> calculate (e1, int) * calculate (e2, int)
  | DIV (e1, e2) -> calculate (e1, int) / calculate (e2, int)
  | SIGMA (e1, e2, e3) -> if (calculate (e1, int) == calculate (e2, int)) 
                             then calculate (e3, calculate (e1,0)) 
                          else calculate (e3, calculate (e2,0)) +
                               calculate (SIGMA (e1, INT (calculate (e2,0) -1), e3), 0) ;;

  let calculator : exp -> int
  = fun exp -> calculate (exp, 0) ;;  (* TODO *)
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

let rec allvar: exp -> string list
  = fun exp ->
 match exp with
 | V v -> [v]
 | P (v, ex) -> []
 | C (ex1, ex2) -> allvar ex1 @ allvar ex2;;

let rec searchlist: (var list * var) -> bool
  = fun (list, var) ->
  match list with
  | [] -> false
  | hd::tl -> if (hd = var) then true
              else searchlist (tl, var);;

let rec complist: (var list * var list) -> bool
  = fun (l1, l2) ->
  match l2 with
  | [] -> true
  | hd::tl -> if searchlist (l1, hd) then complist (l1, tl)
              else false;;

let rec checktest: (exp * var list) -> bool
  = fun (exp, l) ->
  match exp with
  | V v -> true
  | P (v, ex) -> if complist ([v] @ l, allvar ex) then checktest (ex, [v] @ l)
                 else false
  | C (ex1, ex2) -> checktest (ex1, l) && checktest (ex2, l)

  let check : exp -> bool
  = fun exp ->
  match exp with 
  | V v -> false
  |_ -> checktest (exp, []);; (* TODO *)
end


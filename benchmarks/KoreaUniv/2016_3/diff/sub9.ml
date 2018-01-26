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

  let rec zero : aexp -> bool
  = fun exp ->
  match exp with
  | Const n -> if n = 0 then true else false
  | Times lst -> 
    (match lst with
    | [] -> true
    | hd::tl ->
      if (zero hd) then true
      else if tl = [] then false
      else (zero (Times tl))
    )
  | Sum lst -> 
    (match lst with
    | [] -> true
    | hd::tl -> if not((zero hd)) then false else (zero (Sum tl))
    )
  | _ -> false

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
  match exp with
  |Const n -> Const 0
  |Var str -> if str = var then Const 1 else Const 0
  |Power (str, n) ->
    (if str = var then
      (if n = 0 then Const 0
      else if n = 1 then Const 1
      else if n = 2 then (Times [Const 2; Var var])
      else (Times [Const n; Power (str, n - 1)])
    )
    else Const 0)
  |Times lst ->
    (match lst with
      | [] -> Const 0
      | hd::tl ->
        if tl = [] then diff (hd, var)
        else
          let product1 = Times [diff (hd, var); Times tl] in
          let zero1 = (zero product1) in
          let product2 = Times [hd; diff (Times tl, var)] in
          let zero2 = (zero product2) in
          if (zero1 && zero2) then Const 0
          else if zero1 then product2
          else if zero2 then product1
          else Sum [product1; product2]
    )
  |Sum lst ->
    (match lst with
      | [] -> Const 0
      | hd::tl ->
        let d1 = diff (hd, var) in
        let zero1 = zero d1 in
        let d2 = diff ((Sum tl), var) in
        let zero2 = zero d2 in
        if (zero1 && zero2) then Const 0
        else if zero1 then d2
        else if zero2 then d1
        else
          (match d2 with
            | Sum lst2 -> Sum (d1::lst2)
            | _ -> Sum [d1; d2]
          )
    )

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

  let rec getWeight : mobile -> int
  = fun mob ->
  let (left, right) = mob in
  match left with
  | SimpleBranch (left_len, left_w) ->
    (match right with
      | SimpleBranch (right_len, right_w) ->
        left_w + right_w
      | CompoundBranch (right_len, right_mob) ->
        left_w + (getWeight right_mob)
    )
  | CompoundBranch (left_len, left_mob) ->
    (match right with
      | SimpleBranch (right_len, right_w) ->
        (getWeight left_mob) + right_w
      | CompoundBranch (right_len, right_mob) ->
        (getWeight left_mob) + (getWeight right_mob)
    )

  let rec balanced : mobile -> bool
  = fun mob ->
  let (left, right) = mob in
  match left with
  | SimpleBranch (left_len, left_w) ->
    (match right with
      | SimpleBranch (right_len, right_w) ->
        left_len * left_w = right_len * right_w
      | CompoundBranch (right_len, right_mob) ->
        (left_len * left_w = right_len * (getWeight right_mob))
        && (balanced right_mob)
    )
  | CompoundBranch (left_len, left_mob) ->
    (match right with
      | SimpleBranch (right_len, right_w) ->
        (left_len * (getWeight left_mob) = right_len * right_w)
        && (balanced left_mob)
      | CompoundBranch (right_len, right_mob) ->
        (left_len * (getWeight left_mob) = right_len * (getWeight right_mob))
        && (balanced left_mob)
        && (balanced right_mob)
    )

	
	
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

  let rec sigma : int -> int -> exp -> int
  = fun start last exp ->
  if start > last then 0
  else (eval exp start) + (sigma (start + 1) last exp)

  and eval : exp -> int -> int
  = fun exp k ->
  match exp with
  | X -> k
  | INT i -> i;
  | ADD (e1, e2) -> (eval e1 k) + (eval e2 k)
  | SUB (e1, e2) -> (eval e1 k) - (eval e2 k)
  | MUL (e1, e2) -> (eval e1 k) * (eval e2 k)
  | DIV (e1, e2) -> (eval e1 k) / (eval e2 k)
  | SIGMA (e1, e2, e3) ->
    let start = (eval e1 k) in
    let last = (eval e2 k) in
    (sigma start last e3)
		

  let calculator : exp -> int
  = fun exp -> 
  (eval exp 0)

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

  let rec lst_check : string -> string list -> bool
  = fun var env ->
  match env with
  | [] -> false
  | hd::tl -> if hd = var then true else (lst_check var tl) 

  let rec well : exp -> string list -> bool
  = fun exp env ->
  match exp with
  | V var -> (lst_check var env)
  | P (var, e) -> (well e (var::env))
  | C (e1, e2) -> (well e1 env) && (well e2 env)

  let check : exp -> bool
  = fun exp -> (well exp [])
end


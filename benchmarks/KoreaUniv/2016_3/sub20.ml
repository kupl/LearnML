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
  
  let rec cal_diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
  | Const n -> Const 0
  | Var x -> if x=var then Const 1 else Const 0
  | Power (x,n) -> if x=var then (match n with
	|0 -> Const 0
	|1 -> Const 1
	|2 -> Times[Const 2; Var var]
	|_ -> Times[Const n; Power (var, n-1)])
	else Const 0
  | Times (hd :: tl) -> if tl = [] then (cal_diff (hd,var))
  else (Sum [(Times([cal_diff(hd,var)] @ tl));(Times([hd] @ [(cal_diff(Times tl,var))]))])
  | Sum (hd :: tl) -> if tl = [] then (cal_diff(hd,var)) else (Sum ([(cal_diff(hd,var))] @ [(cal_diff(Sum(tl),var))]))
  | Times [] -> Const 0
  | Sum [] -> Const 0
  
  let diff : aexp * string -> aexp
  = fun (exp, var) -> cal_diff (exp,var)
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

  let rec cal_mob : mobile -> int
  = fun mob -> match mob with
  | SimpleBranch (m1, m2),SimpleBranch (n1, n2) -> 
	if m1*m2=n1*n2 then m2+n2 else -1
  | SimpleBranch (m1, m2),CompoundBranch (n1, n2) ->
	if m1*m2=n1*(cal_mob n2) then m2+(cal_mob n2) else -1	
  | CompoundBranch (n1, n2),SimpleBranch (m1, m2) -> 
	if m1*m2=n1*(cal_mob n2) then m2+(cal_mob n2) else -1
  | CompoundBranch (m1, m2),CompoundBranch (n1, n2) -> 
	if m1*(cal_mob m2)=n1*(cal_mob n2) then (cal_mob m2)+(cal_mob n2) else -1
  
  let balanced : mobile -> bool
  = fun mob -> if (cal_mob mob)>(-1) then true else false
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

  let rec cal_sigma : exp -> int
  = fun exp -> 0
  
  let rec cal_exp : int -> exp -> int
  = fun x exp -> match exp with
  | X -> x 
  | INT n -> n
  | ADD (e1,e2) -> (cal_exp x e1) + (cal_exp x e2)
  | SUB (e1,e2) -> (cal_exp x e1) - (cal_exp x e2)
  | MUL (e1,e2) -> (cal_exp x e1) * (cal_exp x e2)
  | DIV (e1,e2) -> (cal_exp x e1) / (cal_exp x e2)
  | SIGMA(e1,e2,e3) -> (match e1,e2,e3 with
	| INT v1, INT v2, v3 -> if v1>v2 then 0
	else (cal_exp v2 v3) + cal_exp 0 (SIGMA(INT v1,INT (v2-1),v3))
	| v1,v2,v3 -> 
	let n1 = cal_exp 0 v1 in
	let n2 = cal_exp 0 v2 in
	if n1>n2 then 0
	else (cal_exp n2 v3) + cal_exp 0 (SIGMA(INT n1,INT (n2-1),v3))
	)
    
  let calculator : exp -> int
  = fun exp -> cal_exp 0 exp
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
  
  let rec isin : var * var list -> bool
  =fun (str,lst) -> match lst with
  | [] -> false
  | hd::tl -> if hd=str then true else isin (str,tl)
  
  let rec filter lst lst2 = 
  match lst with
  | [] -> true
  | hd::tl -> if (isin (hd,lst2) = true) then (filter tl lst2) else false

  let rec bool_check : exp * var list -> var list
  =fun (exp,lst) -> match exp with
  | V str ->  lst
  | P (e1,e2) -> bool_check (e2,[e1] @ lst)
  | C (e1,e2) -> (bool_check (e1, lst)) @ (bool_check (e2, lst))
  
  let rec cal_check : exp * var list -> var list
  =fun (exp,lst) -> match exp with
  | V str -> str::lst
  | P (e1,e2) -> cal_check (e2, lst)
  | C (e1,e2) -> (cal_check (e1,lst)) @ (cal_check (e2, lst))
  
  let check : exp -> bool
  = fun exp -> if ((filter (cal_check (exp,[])) (bool_check (exp,[])))=true) then true else false
end

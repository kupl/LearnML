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

  let rec map f l =
  	match l with
	| [] ->	[]
	| hd::tl ->	(f hd)::(map f tl)

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
  match exp with
  | Const n ->	Const 0
  | Var str ->	if str = var then Const 1 else Const 0
  | Power (_, 0) ->	Const 0
  | Power (str, n) ->	if str <> var then Const 0 else Times [Const n; Power (str, n - 1)]
  | Times [] ->	Const 0
  | Times (hd::tl) ->	Sum [Times ((diff (hd, var))::tl); Times [hd; diff (Times tl, var)]]
  | Sum [] ->	Const 0
  | Sum l ->	Sum (map (fun exp -> diff (exp, var)) l)
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

  let rec tortal_weight_br : branch -> int 
  = fun br ->
  match br with
  | SimpleBranch (_, w) ->	w
  | CompoundBranch (_, (lbr, rbr)) ->	(tortal_weight_br lbr) + (tortal_weight_br rbr)

  let rec tortal_weight : mobile -> int
  = fun mo ->
  match mo with
  | (lbr, rbr) ->	(tortal_weight_br lbr) + (tortal_weight_br rbr)


  let rec balanced : mobile -> bool
  = fun mob -> 
  match mob with
  | (SimpleBranch (ll, lw),	SimpleBranch (rl, rw)) ->
  	if (ll * lw) = (rl * rw) then true else false
  | (CompoundBranch (ll, lm),	SimpleBranch (rl, rw)) ->
  	if (ll * tortal_weight lm) = (rl * rw) then balanced lm else false
  | (SimpleBranch (ll, lw),	CompoundBranch (rl, rm)) ->
  	if (ll * lw) = (rl * tortal_weight rm) then balanced rm else false
  | (CompoundBranch (ll, lm),	CompoundBranch (rl, rm)) ->
  	if (ll * tortal_weight lm) = (rl * tortal_weight rm) then (balanced lm) && (balanced rm) else false
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

  let rec calcx : exp -> int -> int
  = fun exp x ->
  match exp with
  | X -> x
  | INT n ->	n
  | ADD (e1, e2) ->	calcx e1 x + calcx e2 x
  | SUB (e1, e2) ->	calcx e1 x - calcx e2 x
  | MUL (e1, e2) -> calcx e1 x * calcx e2 x
  | DIV (e1, e2) -> calcx e1 x / calcx e2 x

  let rec calculator : exp -> int
  = fun exp ->
  match exp with
  | INT n ->	n
  | ADD (e1, e2) ->	calculator e1 + calculator e2 
  | SUB (e1, e2) ->	calculator e1 - calculator e2 
  | MUL (e1, e2) -> calculator e1 * calculator e2 
  | DIV (e1, e2) -> calculator e1 / calculator e2 
  | SIGMA (x1, x2, e) ->	if calculator x1 > calculator x2 then 0 else (calcx e (calculator x1)) + (calculator (SIGMA (ADD (x1, INT 1), x2, e)))
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

  let rec isexist : var list -> var -> bool
  = fun vars v ->
  match vars with
  | [] ->	false
  | hd::tl ->	if hd = v then true else isexist tl v

  let rec chkvars : exp -> var list -> bool
  = fun exp vars ->
  match exp with
  | V v -> isexist vars v
  | P (v, e) ->	chkvars e (v::vars)
  | C (V v, e) ->	chkvars e (v::vars)

  let check : exp -> bool
  = fun exp -> 
  chkvars exp []
end


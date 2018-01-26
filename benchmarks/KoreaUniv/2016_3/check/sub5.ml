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
      | Var v -> if v = var  then Const 1  else Const 0
      | Power (v, n) -> 
         if(v = var&&n>1) then Times [Const n; Power(v,n - 1)] 
         else if n=1 then Const 1
          else Const 0
      | Times lst -> 
         begin
         match lst with 
         | [] -> Const 0
         | hd::tl -> Sum (Times((diff (hd,var))::tl)::[Times(hd::([diff (Times(tl),var)]))])
         end
      | Sum lst ->
         match lst with
         | [] -> Const 0 
         | hd::tl -> Sum((diff (hd,var))::[diff (Sum(tl),var)])
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

  let rec plus : branch -> int
  = fun p ->
  match p with
  | SimpleBranch(m, n) -> n
  | CompoundBranch(m, (n, o)) -> (plus n) + (plus o);;

  let rec mul : branch -> int
  = fun mo ->
  match mo with
  | SimpleBranch(a, b) -> a * (plus mo)
  | CompoundBranch(a, (b, c)) ->
a * (plus mo);;

  let rec balanced : mobile -> bool
  = fun mob ->
  let rec bal : branch -> bool
  = fun br ->
match br with
| SimpleBranch(l, w) -> true
| CompoundBranch(l, (w1, w2)) -> if (mul w1) = (mul w2) then (bal w1) && (bal w2)
else false
in
match mob with
| (a, b) -> ((bal a) && (bal b)) && ((mul a) = (mul b))
end;;

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

  let rec calculator2 : exp * int -> int
  = fun (exp, a) -> 
		match exp with
		| X -> a
		| INT num1-> num1
		| ADD (num1, num2) -> (calculator2  (num1, a)) + (calculator2 (num2, a))
		| SUB (num1, num2) -> (calculator2 (num1, a)) - (calculator2 (num2, a))
		| MUL (num1, num2) -> (calculator2 (num1, a)) * (calculator2 (num2, a))
		| DIV (num1, num2) -> (calculator2 (num1, a)) / (calculator2 (num2, a))
	 	| SIGMA (num1, num2, num3) ->
			if (calculator2 (num1, a)) > (calculator2 (num2, a)) then 0
			else calculator2 (num3, (calculator2 (num1, a))) + calculator2 (SIGMA ((INT (calculator2 (num1, a) + 1)), num2, num3), 0) 

	let calculator : exp -> int
	= fun exp -> calculator2 (exp, 0)
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


  let rec comp : exp -> string list -> bool
  = fun exp lst ->
    match exp with
    | V x -> 
			begin
      match lst with
      | [] -> false
      | hd::tl -> if x = hd then true
                  else comp exp tl
			end
    | P (x, expa) -> comp expa (x::lst)
		| C (expa, expb) -> (comp expa lst) && (comp expb lst)


  let rec check : exp -> bool
  = fun exp ->
		let lst = [] in   
		match exp with
		| V x -> false
		| P (x, expa) -> comp expa (x::lst)
		| C (expa, expb) -> (comp expa lst) && (comp expb lst)

end


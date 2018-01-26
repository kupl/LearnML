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
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
= fun (aexp, x) -> (match aexp with
  Const n -> Const 0
| Var v -> if v=x then Const 1 else Const 0
| Power (s,n) -> (if s=x then
								(match n with
									  0 -> Const 0
									| 1 -> Const 1
									| 2 -> Times [Const 2 ; Var s]
									| _ -> Times [Const n ; Power (s,n-1)])
								else Const 0)
| Times lst -> (match lst with 
								  [] -> Const 1
								| hd :: [] -> diff (hd,x)
								| hd :: tl -> match hd with
															  Const 0 -> Const 0
															| Const 1 -> diff (Times tl,x)
															| Const n -> Times[Const n ; diff (Times tl,x)]
															| _ -> Sum[Times (diff (hd,x)::tl);Times[hd ; diff (Times tl, x)]])
| Sum lst -> (match lst with
							  [] -> Const 0
							| hd :: [] -> diff (hd,x)
							| hd :: tl -> Sum [diff (hd,x) ; diff (Sum tl,x)]));;
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

  let rec balanced : mobile -> bool
  = fun (left, right) -> 
let rec cal_Weight : branch -> weight
= fun calweight -> match calweight with
  SimpleBranch (l,w) -> w
| CompoundBranch (l,m) -> match m with (left,right) -> (cal_Weight left)+(cal_Weight right)
in let cal_Torque : branch -> int
=fun caltorque -> match caltorque with 
  SimpleBranch (l,w) -> l*w
| CompoundBranch (l,m) -> match m with (left,right) -> l*(cal_Weight left) + l*(cal_Weight right)
in
 match left,right with
  SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> if (cal_Torque left)=(cal_Torque right) then true else false
| SimpleBranch (l1,w1), CompoundBranch (l2,m2) -> if (balanced m2 = true) && (cal_Torque left = cal_Torque right) then true else false
| CompoundBranch (l1,m1), SimpleBranch (l2,w2) -> if (balanced m1 = true) && (cal_Torque left = cal_Torque right) then true else false
| CompoundBranch (l1,m1), CompoundBranch (l2,m2) -> if (balanced m1 = true) && (balanced m2 = true) && (cal_Torque left = cal_Torque right) then true else false;;
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
  | SIGMA of exp * exp * exp;;

  let rec calculator : exp -> int
  = fun exp -> 
let rec sigma (a,b,f) = if a=b then f a else (f a) + (sigma (a+1,b,f))
in let rec expf : exp -> (int -> int)
= fun expfun -> match expfun with
  X-> (fun x -> x)
| INT a -> (fun x -> a)
| ADD (a,b) -> (fun x -> ((expf a)x)+((expf b)x))
| SUB (a,b) -> (fun x -> ((expf a)x)-((expf b)x))
| MUL (a,b) -> (fun x -> ((expf a)x)*((expf b)x))
| DIV (a,b) -> (fun x -> ((expf a)x)/((expf b)x))
| SIGMA (a,b,f) -> (fun x -> sigma((expf a)x,(expf b)x,(expf f)))
in match exp with
  X -> raise (Failure "We cannot calculate just with it")
| INT a -> a
| ADD (a,b) -> (calculator a) + (calculator b)
| SUB (a,b) -> (calculator a) - (calculator b)
| MUL (a,b) -> (calculator a) * (calculator b)
| DIV (a,b) -> (calculator a) / (calculator b)
| SIGMA (a,b,f) -> sigma (calculator a, calculator b, expf f);; 	
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string;;

  let rec check : exp -> bool
  = fun exp -> let rec checking : exp * (var list) -> bool
= fun (aexp, vlist) -> (match aexp with
  V s -> (match vlist with
				  [] -> false
				| hd::tl -> if s = hd then true else checking (V s, tl))
| P (v,exp1) -> checking (exp1, v :: vlist)
| C (exp1, exp2) -> (checking (exp1, vlist))&&(checking (exp2, vlist)))						 
in checking (exp,[]);;
end


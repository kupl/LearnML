(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
		   | Empty -> t
		   | Node(parent, left, right) -> Node(parent, mirror right, mirror left);;


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
			  | ZERO -> n2
			  | SUCC (x) -> natadd x (SUCC(n2));;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
			   | ZERO -> ZERO
			   | SUCC (x) -> natadd n1 (natmul n1 x);;

let rec natexp : nat -> nat -> nat
= fun n1 n2 -> match n2 with
			   | ZERO -> SUCC ZERO
			   | SUCC (x) -> natmul n1 (natexp n1 x);;


(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat 
= fun f -> let rec env : formula -> string list
			= fun form -> match form with
						| True -> []
						| False -> []
						| Var x -> [x]
						| Neg x -> env x
						| And (x, y) -> (env x)@(env y)
						| Or (x, y) -> (env x)@(env y)
						| Imply (x, y) -> (env x)@(env y)
						| Iff (x, y) -> (env x)@(env y) in
			let rec env2 : string list -> string list
			=fun l -> match l with
			| [] -> []
			| hd::tl -> if List.mem hd tl then env2 tl else hd::(env2 tl)
		in
			let rec mapping : formula -> string -> bool -> formula
			=fun l s b -> match l with
			  | True -> True
			  | False -> False
			  | Var s -> if b then True else False
			  | Neg f -> mapping f s (not(b))
			  | And (f1,f2) -> And (mapping f1 s b, mapping f2 s b)
			  | Or (f1,f2) -> Or (mapping f1 s b, mapping f2 s b)
			  | Imply (f1,f2) -> Imply (mapping f1 s b, mapping f2 s b)
			  | Iff (f1,f2) -> Iff (mapping f1 s b, mapping f2 s b)
			in
			let rec make_flist : formula list -> string -> formula list
			=fun fl s -> match fl with
			| [] -> []
			| hd::tl -> [mapping hd s true;mapping hd s false]@(make_flist tl s)
			in
			let rec pop_slist : formula list -> string list -> formula list
			= fun fl st -> match st with
			| [] -> []
			| hd::tl -> (make_flist fl hd)@(pop_slist fl tl)
			in
			let rec eval : formula -> bool
			=fun l -> match l with
			  | True -> true
			  | False -> false
			  | Var s -> raise (Failure "Error!")
			  | Neg f -> if eval f then false else true
			  | And (f1,f2) -> (eval f1) && (eval f2)
			  | Or (f1,f2) -> (eval f1) || (eval f2)
			  | Imply (f1,f2) -> eval (Or ((Neg f1),f2))
			  | Iff (f1,f2) -> (eval (Imply (f1,f2)))&&(eval (Imply (f2,f1)))
			in
			let rec real_sat : formula list -> bool
			= fun fl -> match fl with
			| [] -> false
			| hd::tl -> if eval hd then true else real_sat tl
		in
		real_sat (pop_slist [f] (env2 (env f)));;



(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec length : aexp list -> int 
=fun lst -> match lst with
| [] -> 0
| hd::tl -> 1 + length tl;;

let diff : aexp * string -> aexp
= fun (e,x) -> 
 let rec diff_exp : aexp * string ->aexp
= fun (e,x) ->
match e with
 	| Const n -> Const 0
 	| Var value -> if x=value then Const 1 else Const 0
 	| Power (value, n) -> if x=value then Times [Const n; Power (value, n-1)] else Const 0
 	| Sum lst -> let rec diff_sum : aexp list * string -> aexp list
 				= fun(sum_lst, key) -> match sum_lst with
 										| [] -> []
 										| hd::tl -> diff_exp(hd, key)::(diff_sum(tl, key)) in
 										Sum(diff_sum(lst, x))
 	| Times lst -> let rec f : aexp list*string*int*int -> aexp list
 		= fun (lst, key, p, q) -> match lst with
 									| [] -> []
 									| hd::tl -> if p=q then diff_exp(hd, key)::f(tl, key, (p+1), q)
 												else hd::f(tl, key, (p+1), q) in
 												let rec diff_time : aexp list * string * int * int -> aexp list
 												= fun (time_lst, key, x, y) -> if x>y then [] else Times(f(time_lst, key, 1, x))::diff_time(time_lst, key, (x+1), y)
 											in Sum(diff_time(lst, x, 1, (length lst))) in diff_exp(e, x);;

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec cal_SIGMA : exp -> int -> int
= fun e n -> match e with
			| X -> n
			| INT a -> a
			| ADD(a,b) -> ((cal_SIGMA a n) + (cal_SIGMA b n))
			| SUB(a,b) -> ((cal_SIGMA a n) - (cal_SIGMA b n))
			| MUL(a,b) -> ((cal_SIGMA a n) * (cal_SIGMA b n))
			| DIV(a,b) -> ((cal_SIGMA a n) / (cal_SIGMA b n))
			| SIGMA(a, b, f) -> let x = (cal_SIGMA a n) in
								let y = (cal_SIGMA b n) in
								if x>y then raise (Failure "Wrong Range") else
									if x< y then
										(cal_SIGMA f x) + (cal_SIGMA (SIGMA( INT(x+1), b, f)) n)
									else (cal_SIGMA f x);;


let rec calculator : exp -> int
= fun e -> match e with
			| X -> raise (Failure "Error")
			| INT n -> n
			| ADD(a,b) -> (calculator a + calculator b)
			| SUB(a,b) -> (calculator a - calculator b)
			| MUL(a,b) -> (calculator a * calculator b)
			| DIV(a,b) -> (calculator a / calculator b)
			| SIGMA(a, b, f) -> let x = (calculator a) in
								let y = (calculator b) in
								if x > y then raise (Failure "Wrong Range") else
									if x < y then 
										((cal_SIGMA f x) + (calculator (SIGMA(INT (x+1), b, f))))
									else (cal_SIGMA f x);;

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec measure_weight: mobile -> int
= fun m -> match m with
			| (SimpleBranch(l_length, l_weight), SimpleBranch(r_length, r_weight)) -> (l_weight + r_weight)
			| (SimpleBranch(l_length, l_weight), CompoundBranch(r_length, r_mobile)) -> (l_weight + (measure_weight r_mobile))
			| (CompoundBranch(l_length, l_mobile), SimpleBranch(r_length, r_weight)) -> ((measure_weight l_mobile) + r_weight)
			| (CompoundBranch(l_length, l_mobile), CompoundBranch(r_length, r_mobile)) -> (measure_weight l_mobile) + (measure_weight r_mobile);;

let rec balanced : mobile -> bool
= fun m -> match m with
			| (SimpleBranch(l_length, l_weight), SimpleBranch(r_length, r_weight)) -> if (l_length*l_weight- r_weight*r_length)=0 then true else false
			
			| (SimpleBranch(l_length, l_weight), CompoundBranch(r_length, r_mobile)) -> 
					if (balanced r_mobile) then 
					balanced ( 
						SimpleBranch(l_length,l_weight) , 
						SimpleBranch(r_length, (measure_weight  r_mobile)) )

					else false

			| (CompoundBranch(l_length, l_mobile), SimpleBranch(r_length, r_weight)) -> 
					if (balanced l_mobile) then
					balanced ( 
						SimpleBranch(l_length, (measure_weight l_mobile)) , 
						SimpleBranch(r_length,r_weight) )
					else false

			| (CompoundBranch(l_length, l_mobile), CompoundBranch(r_length, r_mobile)) -> 
					if (balanced l_mobile) then
						if (balanced r_mobile) then
					balanced ( 
						SimpleBranch(l_length, (measure_weight l_mobile)) , 
						SimpleBranch(r_length, (measure_weight r_mobile)) )
						else false
					else false;;


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin
= fun b1 b2 ->  
				(*길이계산v*)
				let rec bin_length : bin -> int
				= fun b -> match b with
				| [] -> 0
				| hd::tl -> 1+ (bin_length tl) in

				(*거듭제곱v*)
				let rec expt : int -> int -> int
				= fun b n -> match n with
							| 0 -> 1
							| _ -> match n mod 2 with
								|1 -> b* (expt b ((n-1)/2))*(expt b ((n-1)/2)) 
								|_ -> (expt b (n/2))*(expt b (n/2)) in

				(*십진수로v*)
				let rec btod : bin -> int -> int
				= fun b_bin l -> match b_bin with
								| [] -> 0
								| hd::tl -> match hd with
											| ZERO -> (btod tl (l-1)) 
											| ONE -> ((expt 2 (l-1)) + (btod tl (l-1)) ) in

				(*이진수로*)
				let rec dtob : int -> bin ->bin
				= fun dec result -> match dec with
									| 0 -> result
									|_ -> match dec mod 2 with
											| 0 -> (dtob (dec/2) (ZERO::result))
											| 1 -> (dtob (dec/2) (ONE::result)) in
				(*진짜 계산*)
				(dtob ( (btod b1 ((bin_length b1))) * (btod b2 ((bin_length b2))) ) []);;
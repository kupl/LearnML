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
  = fun (exp, var) -> match exp with
    Const con -> Const 0
  | Var str ->
			if str = var then Const 1
		  else Const 0
  | Power (str, num) ->
			if str = var && num > 0 then  Times [Const num; Power (str, num-1)]
      else Const 0
  | Times time -> (match time with
        []     ->  Const 0
      | [hd]   -> diff(hd,var)
      | hd::tl -> 
            Sum [Times (diff(hd, var)::tl); Times[hd; diff(Times tl, var)]] )
  | Sum plus -> (match plus with
        []     -> Const 0
      | [hd]   -> diff(hd,var)
      | hd::tl -> Sum [diff(hd, var); diff(Times tl, var)]);;
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

	let rec branchweight : mobile -> int
  = fun mob -> match mob with
	    (SimpleBranch (al,aw), SimpleBranch (bl, bw))
        -> aw + bw
    | (SimpleBranch (al,aw), CompoundBranch (bl,bm))
				-> aw + branchweight bm
		| (CompoundBranch (al,am), SimpleBranch (bl,bw))
				-> branchweight am + bw
		| (CompoundBranch (al,am), CompoundBranch (bl,bm))
        -> branchweight am + branchweight bm;;

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
		  (SimpleBranch (al,aw), SimpleBranch (bl,bw)) ->
			  if al*aw = bl*bw then true
			  else false
	  | (CompoundBranch (al,am), SimpleBranch (bl,bw)) ->
				if balanced am && al*branchweight am = bl*bw then true
				else false
		| (SimpleBranch (al,aw), CompoundBranch (bl,bm)) ->
				if balanced bm && al*aw = bl*branchweight bm then true
				else false
		| (CompoundBranch (al,am), CompoundBranch (bl,bm)) ->
				if balanced am && balanced bm 
           && al*branchweight am = bl* branchweight bm then true
				else false;; 

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

  let sum = ref 0;; 
  let refer = ref 0 ;;

  let renew k n =
		let save = k := n in
		    save ;;

  let sigma n =
		let sigm = sum := !sum + n in
				sigm ;;
  
  let rec calculator : exp -> int
  = fun exp -> match exp with
    X          -> !refer
	|	INT a      -> a
	| ADD (a, b) -> calculator a + calculator b
	| SUB (a, b) -> calculator a - calculator b
  | MUL (a, b) -> calculator a * calculator b 
	| DIV (a, b) -> if calculator b = 0 then raise NotImplemented
									else calculator a / calculator b
	| SIGMA(a, b, c) -> if calculator a > calculator b
										  then let temp = !sum in
														let () = renew sum 0 in temp
		                  else let () = renew refer (calculator b) in
														let () = sigma (calculator c) in
															calculator (SIGMA (a,INT(calculator b - 1),c));;

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

  let rec comparelist : 'a list -> 'a list -> 'a list
  = fun procdata vardata -> match (procdata, vardata) with
	  (_,[])   -> []
  | (hd1::tl1, hd2::tl2) -> if hd1 = hd2 then comparelist procdata tl2
    else (comparelist [hd1] tl2)@(comparelist tl1 vardata)
	| ([],hd2::tl2)       -> hd2::tl2;; 
	
  let rec varlist : exp -> var list
  = fun exp ->  match exp with
	| V v     -> [v]
  | P (_, a) -> varlist a
  | C (a, b) -> varlist a@varlist b;;

  let rec proclist : exp -> var list
	= fun exp -> match exp with
	| P (v, a) -> v::proclist a
	| C (a, b) -> proclist a@proclist b
  |_         -> [];;

  let check : exp -> bool
  = fun exp -> let list1 = proclist exp in
		            let list2 = varlist exp in
								 let list3 = comparelist list1 list2 in
										if list3 = [] then true
										else false;;
end


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
	let rec rvs bl = match bl with 
	| [] -> []
	| hd::tl -> (rvs tl)@[hd]
	in let tails a = match a with | [] -> [] | hd::tl -> tl
	in let head a = match a with | [] -> [] | hd::tl -> [hd]
	in let rec op a1 a2 = match a2 with
	| [] -> [a1]
	| ONE::tl -> if(a1 = ONE) then ZERO::(op ONE tl) else a2
	| ZERO::tl -> a1::tl
	in let rec ops a3 a4 = match a3 with
	| [] -> a4
	| hd::tl -> head (op hd (a4))@(ops tl (tails (op hd (a4))))
	in let rec mb a1 a4 k = 
	match a1 with 
	| [] -> k
	| hd::tl -> if(hd = ZERO) then mb tl (ZERO::a4) k 
				else mb tl (ZERO::a4) ((a4)::k)
	in let rec mub k = match k with | []->[] | hd::tl -> (ops hd (mub tl))
	in let rec xzero j = match j with | [] -> [ZERO] | hd::tl -> if(hd = ONE) then j else xzero tl
	in let rec a b1 b2 = mub (mb (rvs b1) (rvs b2) []) 
	in xzero ((rvs (a b1 b2)))
(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> 
	let del_tail l = 
		match l with
		| [] -> []
		| _ ->List.rev (List.tl (List.rev l))
	in

	let rec compareVs v1 v2 = 
		match v1, v2 with
		| [], []       -> true
		| [], _
		| _, []        -> false
		| x::xs, y::ys -> x = y && compareVs xs ys
	in

	let rec badd : bin -> bin -> bin -> bin
	= fun b1 b2 x->
		if ((compareVs b1 [ZERO])||(compareVs b1 []))  then
			begin
				if (compareVs x [ZERO]) then b2 else badd b2 x [ZERO]
			end
		else if ((compareVs b2 [ZERO])||(compareVs b2 [])) then
			begin
				if (compareVs x [ZERO]) then b1 else badd b1 x [ZERO]
			end
		else
			let b1_tail = (List.hd (List.rev b1)) in
			let b2_tail = (List.hd (List.rev b2)) in
			let rec counting_one l n
				= match l with
				| [] -> n
				| [ZERO] -> n
				| [ONE] -> n+1
				| hd::tl -> if hd==ONE then counting_one tl n+1 else counting_one tl n
			in
			let cnt = (counting_one ([b1_tail;b2_tail]@x) 0) in
			if cnt==0 then (badd (del_tail b1) (del_tail b2) [ZERO])@[ZERO]
			else if cnt==1 then (badd (del_tail b1) (del_tail b2) [ZERO])@[ONE]
			else if cnt==2 then (badd (del_tail b1) (del_tail b2) [ONE])@[ZERO]
			else (badd (del_tail b1) (del_tail b2) [ONE])@[ONE]
	in

match (List.rev b2) with
| [ONE] -> b1
| [ZERO] -> [ZERO]
| [] -> [ZERO]
| hd::tl ->
	begin
		match hd with
		| ZERO -> (bmul b1 tl)@[ZERO]
		| ONE -> badd ((bmul b1 tl)@[ZERO]) b1 [ZERO]
	end
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina
type tourna = LEAF of team
	| NODE of tourna * tourna

let pptree t =
	let rec depth t_ = match t_ with
		LEAF _ -> 0
		| NODE (t1, t2) -> 
			let d_t1 = depth t1 in
			let d_t2 = depth t2 in
			(if (d_t1 > d_t2) then d_t1 else d_t2)+1
	in
	let rec pchar_iter c i =
		if i == 0 then () else (print_char c; pchar_iter c (i-1))
	in
	let rec pow_2 i =
		if i == 0 then 1 else (pow_2 (i-1))*2
	in
	let entire_depth = depth t in
	let rec pptr t layer target_layer is_end =
		match t with
			LEAF _ ->
				let p2l = pow_2 (entire_depth - layer+1) in
				((pchar_iter ' ' (p2l-1); print_char '|'; pchar_iter ' ' p2l);
				if is_end then print_newline () else ())
	
			| NODE (t1, t2) -> 
				if layer!=target_layer 
				then
					(pptr t1 (layer+1) target_layer false;
					pptr t2 (layer+1) target_layer is_end)
				else
					let p2l = pow_2 (entire_depth - layer) in
					((pchar_iter ' ' (p2l-1); print_char '|';
					pchar_iter '-' (p2l*2-1); print_char '|';
					pchar_iter ' ' p2l);
					if is_end then print_newline () else ())
	in
	let rec pptr_iter t i =
		if i==0 
			then pptr t 1 entire_depth true
			else (pptr t 1 (entire_depth-i) true;
				pptr_iter t (i-1))
	in
	(pchar_iter ' ' ((pow_2 entire_depth) -1);print_char '|';
	print_newline (); 
	if (entire_depth != 0) 
		then pptr_iter t (entire_depth-1)
		else ())

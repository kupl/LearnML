type team 	= Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		| Poland | Portugal | Italy | Germany | Sweden | England
		| Croatia | Argentina
type tourna 	= LEAF of team
		| NODE of tourna * tourna



let rec pptree t = 
	let rec tourna_to_str(current_step, limit_step, total_step, t) = 
		match t with
		| NODE(t1,t2)	->	if (  limit_step-current_step ==0) then
						make_blank(pow(total_step-current_step-1,2)-1)^
						"|"^
						make_dash(pow(total_step-current_step,2)-1)^
						"|"^
						make_blank(pow(total_step-current_step-1,2)-1)
					else
						tourna_to_str(current_step+1, limit_step, total_step, t1)^
						" "^
						tourna_to_str(current_step+1, limit_step, total_step, t2)
		| LEAF(l)	->	make_blank(pow(total_step-current_step,2)-1)^
					"|"^
					make_blank(pow(total_step-current_step,2)-1)
	and  pow(n, x) =
	    if n==0 then 1 else x * pow(n-1, x)
	and make_blank(n) = 
		if n==0 then "" else " "^make_blank(n-1)
	and make_dash(n) = 
		if n==0 then "" else "-"^make_dash(n-1)
	and get_depth(t) =
		match t with 
		| NODE(t1,t2)	->	if (get_depth(t1)>get_depth(t2)) then 1+get_depth(t1)
					else 1+get_depth(t2)
		| LEAF(l)	->	1

	and tourna_print(limit_step,total_step,t) = 		
		if (limit_step==total_step-1) then
			print_endline(tourna_to_str(1,limit_step,total_step,t))
		else
			print_endline(tourna_to_str(1,limit_step,total_step,t));
		if (limit_step!=total_step-1) then
			tourna_print(limit_step+1,total_step,t)


	in
		print_endline(
			make_blank(pow(get_depth(t)-1,2)-1)^
			"|"^
			make_blank(pow(get_depth(t)-1,2)-1)
			);
		if get_depth(t)>1 then
			tourna_print(1,get_depth(t),t)
(*
let a = pptree (LEAF Korea);;
let a = pptree (NODE (LEAF Korea, LEAF Portugal));;
let a = pptree (NODE (NODE (NODE (LEAF Korea, LEAF Portugal), NODE (LEAF Italy, LEAF Usa)), LEAF Brazil));;
let a = pptree (NODE (NODE (NODE (LEAF Korea, LEAF Portugal), NODE (LEAF Korea , NODE (LEAF Italy, LEAF Korea ))), LEAF Brazil));;
let a = pptree (NODE (NODE (NODE (NODE (LEAF Korea, LEAF France), NODE (LEAF Usa, LEAF Brazil)), NODE (NODE (LEAF Japan, LEAF Nigeria), NODE (LEAF Cameroon, LEAF Korea))), NODE (NODE (NODE (LEAF Portugal, LEAF Italy), NODE (LEAF Germany, LEAF Korea)), NODE (NODE (LEAF England, LEAF Korea), LEAF Argentina))));;
*)
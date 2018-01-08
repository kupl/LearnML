(*hw2-1 컴퓨터 공학부 2008-11641 신희식*) 

exception Error
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
			| Poland | Portugal | Italy | Germany | Sweden | England
			| Croatia | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna

let pptree tourna =
	let max (first, second) =
		if first > second then
			first
		else
			second
	in
	let rec cal_dep tree =
		match tree with
		(NODE (a,b)) ->
			(max ((cal_dep a),(cal_dep b))) + 1
		|(LEAF a) ->
			1	
	in

	let rec make_str tr_list cur_stat dep total check=
		let rec make_space n =
			if n>0 then
				(" "^(make_space (n-1)))
			else
				""
		in
		let rec make_dash n =
			if n>0 then
				("-"^(make_dash (n-1)))
			else
				""
		in
		let rec cal_square n =
			if n>0 then
				(2*(cal_square (n-1)))
			else
				1
		in
		let cal_num =
			((cal_square (dep - cur_stat)) - 1)
		in
		match tr_list with
		[] -> ""
		|h::t ->
			(match h with
			 (_,1) ->
			 	(if total = 1 then
				 ((make_space cal_num)^"|"^(make_space cal_num))
				 else
				 (if check = 1 then
				  ((make_dash cal_num)^"|"^(make_space (cal_num+1))^(make_str t cur_stat dep total 0))
				  else
				  ((make_space cal_num)^"|"^(make_dash (cal_num+1))^(make_str t cur_stat dep total 1))
				 )
				)
			 |((LEAF a), n) ->
				(if total = 1 then
				  (raise Error)
				 else
				 (if check = 1 then
				  (raise Error)	
				  else
				  ((make_space (n*(cal_num+1)-1))^"|"^(make_space (n*(cal_num+1)))^
					 (make_str t cur_stat dep total 0))
				 )
				)
			 |_->
			 	(raise Error)
			)
	in
	let rec make_next_trlist trlist =
		match trlist with
		[] -> []
		|h::t ->
			(match h with
			 ((NODE (a,b)), 1) ->
				(List.append [(a,1);(b,1)] (make_next_trlist t))
			 |((LEAF a), n) ->
				(((LEAF a),(2 * n))::(make_next_trlist t))
			 |_->
			 	(raise Error)
			)
	in
	let rec sub_pp tr_list cur_stat dep total =
		if cur_stat > dep then
		""
		else
		(make_str tr_list cur_stat dep total 0)^"\n"^(sub_pp (make_next_trlist tr_list) (cur_stat + 1) dep (2*total))
	in
	print_string (sub_pp [(tourna,1)] 1 (cal_dep tourna) 1) 

exception Error

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna
		let rec check_height t = 
			match t with
			LEAF l -> 0
			| NODE(a , b) -> if ((check_height a) >= (check_height b)) then (check_height a) + 1
						else (check_height b) + 1 
			 
		let rec an n =
			if (n<=0) then 0
			else if (n=1) then 1
			else 2*(an (n-1))+1
			
		let rec space n = 
			if(n=0) then ""
			else " "^(space (n-1))
			 
		let rec underbar n = 
			if(n=0) then ""
			else "-"^(underbar (n-1))
			
		let rec print_tree l = 
			match l with
			[] -> ()
			| h::t -> (print_string h);(print_string "\n");(print_tree t)
			
		let rec make_string t_list height n=
				match t_list with
				[] -> "" 
				| ((h,l)::t) -> ( match h with 
							  LEAF a -> if (List.mem_assoc a l) then (List.assoc a l)^" "^(make_string t height n)
							 			else raise Error
							  | NODE (a, b) -> (space (an (height-n-1)))^"|"^(underbar (an (height-n-1)))^"-"^(underbar (an (height-n-1)))^"|"^(space (an (height-n-1)))^" "^(make_string t height n)

							) 
	
		let rec make_t_list t_list height n= 
		match t_list with
		[] -> []
		| ((h,l)::t) -> (match h with
					LEAF a -> [((LEAF a),l)]@(make_t_list t height n) 
					| NODE(LEAF a, LEAF b) -> [(LEAF a,[(a, (space (an (height-n)))^"|"^(space (an (height-n))))]@l);(LEAF b,[(b, (space (an (height-n)))^"|"^(space (an (height-n))))]@l)]@(make_t_list t height n) 
					| NODE(LEAF a, NODE (b,c)) -> [(LEAF a,([(a, (space (an (height-n)))^"|"^(space (an (height-n))))]@l));(NODE (b,c),l)]@(make_t_list t height n)
					| NODE (NODE (a,b),LEAF c) -> [(NODE (a,b),l);(LEAF c,[(c, (space (an (height-n)))^"|"^(space (an (height-n))))]@l)]@(make_t_list t height n)
					| NODE (NODE (a,b), NODE (c,d)) -> [(NODE (a,b),l);(NODE (c,d),l)]@(make_t_list t height n)
)
		
		let rec check_leaf t_list = 
		match t_list with
		[] -> true
		| ((h,l)::t) -> (match h with
					LEAF a -> check_leaf t 
					| NODE (a,b) -> false) 

		let rec make_string_list t_list height n= 
		if((check_leaf t_list)=true) then []
		else [make_string t_list height n]@(make_string_list (make_t_list t_list height (n+1)) height (n+1))
		
		
let pptree t = 
		match t with
		LEAF a -> print_string "|\n" 
		| NODE (a,b) -> print_tree ([(space (an (check_height t)))^"|"]@(make_string_list [(t,[])] (check_height t) 0))

						

		
						


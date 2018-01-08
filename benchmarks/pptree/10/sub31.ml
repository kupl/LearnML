type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina


type tourna = LEAF of team
 	    | NODE of tourna * tourna


let nations = [(Korea,"Korea");(France,"France");(Usa,"Usa");(Brazil,"Brazil");(Japan,"Japan");(Nigeria,"Nigeria");(Cameroon,"Cameroon");(Poland,"Poland");(Portugal,"Portugal");(Italy,"Italy");(Germany,"Germany");(Sweden,"Sweden");(England,"England");(Croatia,"Croatia");(Argentina,"Argentina")];;



let rec get_level t1 t2 =
	let rec length_t t =
		match t with
		NODE (a,b) -> 1 + (length_t a) + (length_t b)
		|LEAF a -> 0 in
	if ((1 + (length_t t1)) > (1 + (length_t t2))) then (1+(length_t t1))
						       else (1+(length_t t2));; 
let rec get_2 n =
	if (n == 0) then 1 else (2 * (get_2 (n-1)))
	
let rec get_two n =
	(get_2 n) - 1


let rec make_dash n =
	if (n==0) then []
		else "-":: make_dash (n-1)


let rec make_blank n =
	if (n==0) then []
		else " " :: make_blank (n-1)

let rec make_blank_lst n length =
	if (length == 0) then []
		else (make_blank n) :: (make_blank_lst n (length-1))

let rec remove_tail lst n =
	if (n==0) then []
		else match lst with
			h::t -> h::(remove_tail t (n-1))
			|[] -> []

let rec remove_head lst n =
	if (n==0) then lst
		else match lst with
			[] -> []
			|h::t -> remove_head t (n-1)

let rec append a_list b_list =
	match a_list with
	[] -> b_list
	|h::t -> h::(append t b_list)
	

let rec append_list a_list b_list =
	match (a_list,b_list) with
	([],[]) -> []
	|(h1::t1,h2::t2) -> (append h1 h2)::(append_list t1 t2)
	| _ -> []

let rec append_3 a_lst b_lst c_lst =
	(append a_lst (append b_lst c_lst))

let rec append_3_list a_lst b_lst c_lst =
	append_list a_lst (append_list b_lst c_lst)

let rec make_a_str lst =
	match lst with
	h::t -> h^(make_a_str t)  
	|[]-> ""

let rec make_a_str_list lst =
	match lst with
	h::t -> (make_a_str h)::(make_a_str_list t)
	| [] -> []

let rec print_str_list lst =
	match lst with
	h::t -> (print_string h) ; (print_string "\n") ; print_str_list t
	|[] -> (print_string "")

let rec print_tlist lst =
	print_str_list (make_a_str_list lst)	


let rec make_leaflst n =
	if (n==0) then [" "]::[]
		else ["|"] :: (make_leaflst (n-1)) 


let rec make_treelst tourna n =
	match tourna with
	LEAF a -> (make_leaflst n)
	|NODE (a,b) -> append (make_blank (((get_2 (get_level a b)) + (List.length (List.hd (List.tl (make_treelst a (get_level a b))))) - 1)/2)) ["|"]
      
   :: append_3 (List.hd (make_treelst a (get_level a b))) 
		    (make_dash ((get_2 (get_level a b)) - 1)) 
		    (remove_head (List.hd (make_treelst b (get_level a b))) ((List.length (List.hd (List.tl (make_treelst b (get_level a b))))) /2))
   
   
   :: append_3_list (List.tl (make_treelst a (get_level a b)))
   		    (make_blank_lst ((get_2 (get_level a b)) - 
		    		      (((List.length (List.hd (List.tl (make_treelst a (get_level a b))))) +  (List.length (List.hd (List.tl (make_treelst b (get_level a b))))))/2)) 
		    		    (List.length (List.tl (make_treelst a (get_level a b)))))
		    (List.tl (make_treelst b (get_level a b))) 		



let rec pptree tourna =
	print_tlist (make_treelst tourna 1) ;;



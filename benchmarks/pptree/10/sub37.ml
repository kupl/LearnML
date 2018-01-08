exception Error of string
type team = 
Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = 
LEAF of team | NODE of tourna * tourna

type bintree = 
	  SEED
	| FRUIT of string * bintree
	| BRANCH of bintree * string * bintree

let rec getspan n =  
	if (n = 0) then 1 
	else
	   (getspan (n-1))+(int_of_float (2.0**(float n)))

let makebar n = 
	let trim str =
		match str with
			"|" -> str 
	   		|_ -> "|"^str 
	in
	let rec pbar n =
		if (n = 0) then "|"
		else "-"^(pbar (n-1))
	in
		trim (pbar n)

let rec levelchecker t lv =
	let max a b = if (a >= b) then a else b in
	match t with 
		LEAF _ -> lv
	  | NODE (t1, t2) -> max (levelchecker t1 (lv + 1)) (levelchecker t2 (lv + 1))

let rec conversion t level = 
	match t with 
		LEAF _ -> if (level = 0) then SEED
				  else FRUIT ("|", conversion t (level -1))
	  | NODE (t1, t2) -> BRANCH ( conversion t1 (level -1) , makebar (getspan (level - 1)), conversion t2 (level -1))	

		
let strGET t level =
	let rec strGet t level index =  
		match t with 
			BRANCH (b1, bar, b2) -> 
			(	if (level = index) then [bar]
				else (strGet b1 level (index + 1))@(strGet b2 level (index + 1)))
	  	| FRUIT (bar, br) -> 
	  		(	if (level = index) then [bar]
				else (strGet br level (index + 1)) )
	  	| SEED -> [] 
	in
		strGet t level 0

let fstlevelStr l_of_tree =
	let rec space c =
		if (c = 0) then "|"
		else " "^space (c - 1)
	in
		if (l_of_tree = -1) then "|" 
		else space (getspan l_of_tree)  

let rec barnumlist str index =
	if (String.length str = index) then []
	else
		( if ((String.get str index) = '|') then index::(barnumlist str (index + 1))
		  else 
		  	barnumlist str (index + 1)
		)

let rec generalstring strlst indexlst ref_str = (* indexlst := bar indexlst of upper level *)
	match indexlst with
		[] -> ref_str
	  | i::indexlst' -> 
	  	(match strlst with
			[] -> raise (Error "index num != str num")
		  | s::strlst' ->
		  	let rec modifier s i ref_str  =
				if ((String.length ref_str)+((String.length s)/2) < i) 
					then modifier s i (ref_str^" ")
				else
					ref_str^s
			in
				generalstring strlst' indexlst' (modifier s i ref_str)
		 )		
				
let strlstGET t tourna =
	let level = levelchecker tourna 0 in
	let rec strlstGet t level index =
		if (level = index) then []
		else
			[strGET t index]@(strlstGet t level (index + 1))
	in
		strlstGet t level 0

let finalSTREVAL strlstlst bar_index_lst l_of_tree =
	let rec finalStrEval strlstlst bar_index_lst newstrlst = 
		match strlstlst with
			[] -> newstrlst
		| strlst::strlstlst' ->
			let perf_str = generalstring strlst bar_index_lst "" in
			(finalStrEval strlstlst' (barnumlist perf_str 0) (newstrlst@[perf_str])) 
	in
		finalStrEval strlstlst bar_index_lst [(fstlevelStr l_of_tree)]

let rec pstr strlst str= 
	match strlst with
		[] -> print_string (str^"\n")
      | h::t -> pstr t (str^"\n"^h)

let rec getbarindex str index  = 
	if (String.get str index) = '|' then (index)
	else
		getbarindex str (index + 1)

let trimStrlst strlst = 
	let index = getbarindex (List.hd (List.rev strlst)) 0 in
	let rec trim_in strlst index = 
		match strlst with
	 	[] -> []
   		| s::strlst' -> (String.sub s index ((String.length s)-index))::(trim_in strlst' index)
	in
		trim_in strlst index



let pptree t =
 let level = levelchecker t 0 in
 let br = conversion t level in
	pstr (trimStrlst (finalSTREVAL (strlstGET br t) (barnumlist (fstlevelStr level) 0) level)) ""








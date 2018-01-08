type team = Korea|France|Usa|Brazil|Japan|Nigeria|Cameroon|Poland|
	    Portugal|Italy|Germany|Sweden|England|Croatia|Argentina
type tourna = LEAF of team | NODE of tourna * tourna 

type position = RIGHT | LEFT | CENTER

exception Over_Boundary
exception Empty_List
exception Empty_StringList

let rec deep(tourna, tallest) =
	match tourna with
	|LEAF a -> tallest
	|NODE(t1, t2) -> 
		begin
			let tallest1 = deep(t1, tallest+1)
			in
			let tallest2 = deep(t2, tallest+1)
			in
			if tallest1 > tallest2 then tallest1
			else tallest2
		end

	(* li[n] += str *)	
let rec nthlist(li , n, str, result) =   
	match li with
	|(l::r) ->  begin
			if n=0 then (result @ [str^l]@r)
		    else if n<0 then raise (Over_Boundary)
			else nthlist(r, n-1, str, result@[l])
			end
	|[] -> if n=0 then (result@ [str])
		   else (print_string(str);raise (Empty_List)) 

let rec number(i)=
	let rec power2(n) =
		if n=0 then 1
		else 2*power2(n-1)
	in
	power2(i)-1

let rec dotourna(tourna, height, rank, stringlist, po) =
	let rec insertdash(stringlist, height, n)=
			if n =0 then nthlist(stringlist, height, "", []) 
			else insertdash(nthlist(stringlist, height, "-", []),height, n-1)
	in
	
	let rec insertspace(stringlist, height, n)=
			if n=0 then nthlist(stringlist, height, "",[])
			else insertspace(nthlist(stringlist, height, " ",[]),height, n-1)
	in

	let rec nodeprocess(stringlist, height, rank, po) =
				match po with
		|LEFT -> insertspace(nthlist(insertdash(stringlist, height, number(rank-height)), height, " |", []), height, number(rank-height))
		|RIGHT -> insertdash(nthlist(insertspace(stringlist, height, number(rank-height)), height, "-|", []), height, number(rank-height))
		|CENTER -> insertspace(nthlist(stringlist, height, " |", []), height, number(rank-height))
	in
	
	let rec selfleafprocess(stringlist, n, height) =
		if height=rank then insertspace(nthlist(insertspace(stringlist, height, n), height, " |", []), height, n)
		else selfleafprocess(insertspace(nthlist(insertspace(stringlist, height, n), height, " |", []), height, n), n, height+1)
	in
	
	match (tourna, po) with
	| (LEAF a, LEFT) -> if height < rank then selfleafprocess(insertspace(nthlist(insertdash(stringlist, height, number(rank-height)), height, " |", []), height, number(rank-height)), number(rank-height), height+1)
						else nthlist(stringlist, height, " |", [])
	| (LEAF a, RIGHT) -> if height < rank then selfleafprocess(insertdash(nthlist(insertspace(stringlist, height, number(rank-height)), height, "-|", []), height, number(rank-height)), number(rank-height), height+1)
						 else nthlist(stringlist, height, "-|", [])
	| (LEAF a, CENTER) -> nthlist(stringlist, height, "|", [])
	| (NODE(t1, t2), LEFT) -> dotourna(t1, height+1, rank, dotourna(t2, height+1, rank, nodeprocess(stringlist, height, rank, LEFT), RIGHT), LEFT)  
	| (NODE(t1, t2), RIGHT) -> dotourna(t1, height+1, rank, dotourna(t2, height+1, rank, nodeprocess(stringlist, height, rank, RIGHT), RIGHT), LEFT)  
	| (NODE(t1, t2), CENTER) -> dotourna(t1, height+1, rank, dotourna(t2, height+1, rank, nodeprocess(stringlist, height, rank, CENTER), RIGHT), LEFT) 

	

let rec printresult(li) =
		match li with
		|(t::l) -> if l =[] then print_string(t)
				   else print_string(t); print_newline();printresult(l)
		|[] -> print_newline()

let rec lastresult(li, height, rank)=
	let rec insertspace(stringlist, height, n)=
		if n=0 then nthlist(stringlist, height, "",[])
		else insertspace(nthlist(stringlist, height, " ",[]),height, n-1)
	in
	if height = rank-1 then nthlist(li, height, " ", [])
	else if height > rank then raise( Over_Boundary)
	else insertspace(lastresult(li, height+1, rank), height, number(rank-height-1))

let rec lastresult1(li, rank)=
	nthlist(li, rank, " ", [])

let rec pptree(tourna) =
	let rank = deep(tourna,0)
	in
	printresult(dotourna(tourna,0, rank,[], CENTER));


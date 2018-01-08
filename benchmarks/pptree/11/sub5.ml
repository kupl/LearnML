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
		   else raise (Empty_List)

let rec number(i)=
	let rec power2(n) =
		if n=1 then 1
		else 2*power2(n-1)
	in
	-1+4*power2(i)

let rec dotourna(tourna, height, rank, stringlist, po) =
	
	
(*	(* when you meet a node, attach "  " in every Strings which is in front of the node*)
	let rec nodeprocess(result, height)=
		if height =0 then nthlist(result, 0, "  ", [])
		else if height<0 then raise(Over_Boundary)
		else nodeprocess(nthlist(result,height,"  ",[]), height-1)
	in

	let rec leafsubprocess(n, po) =
		match po with
		|LEFT ->
		if n=0 then "|  "
		else if n=100 then "|---"
		else " "^leafsubprocess(n-1, LEFT)
		|RIGHT|CENTER ->
		if n=0 then "|"
		else if n=100 then "---|"
		else " "^leafsubprocess(n-1,RIGHT)

	in
	let rec leafprocess(li, height, rank, po) =
		match po with
		|LEFT -> if height < rank-3 then leafprocess( nthlist(li, height, leafsubprocess(100, LEFT),[]), height+1, rank, LEFT)
				 else if height< rank then  leafprocess( nthlist(li, height, leafsubprocess(rank-height, LEFT),[]), height+1, rank, LEFT)
				 else nthlist(li, height, "| ", [])
		|RIGHT|CENTER ->  if height < rank-3 then leafprocess( nthlist(li, height, leafsubprocess(100, RIGHT),[]), height+1, rank, LEFT)
				 else if height< rank then  leafprocess( nthlist(li, height, leafsubprocess(rank-height, RIGHT),[]), height+1, rank, LEFT)
				 else nthlist(li, height, " |", [])


	in

	let rec leaflastprocess(result, height)=
  		if height =0 then nthlist(result, 0, " ", [])
			else if height<0 then raise(Over_Boundary)
			else nodeprocess(nthlist(result,height," ",[]), height-1)
	in


	match (tourna,po) with
	| (LEAF a, LEFT) ->if height < rank then  leaflastprocess(leafprocess(stringlist, height, rank, LEFT), height-1)
				       else leaflastprocess(nthlist(stringlist, height, "|-", []), height-1)
	| (LEAF a, RIGHT) 
	| (LEAF a, CENTER) -> if height < rank then  leaflastprocess(leafprocess(nthlist(stringlist, height, "|", []), height+1, rank, RIGHT), height-1)
						  else leaflastprocess(nthlist(stringlist, height, "|", []), height-1)
	|(NODE(t1, t2), LEFT) -> dotourna(t1, height+1, rank, dotourna(t2, height+1, rank, nodeprocess(nthlist(stringlist, height, "|---", []), height-1), RIGHT), LEFT)
	|(NODE(t1, t2), RIGHT) -> dotourna(t1, height+1, rank,  dotourna(t2, height+1, rank, nodeprocess(nthlist(stringlist, height, "---|", []), height-1), RIGHT), LEFT)
	|(NODE(t1, t2), CENTER) ->	dotourna(t1, height+1, rank, dotourna(t2, height+1, rank, nthlist(stringlist,height, "|", []), RIGHT), LEFT)
*)
	
	let rec nodeprocess(stringlist, height, rank, po) =
		let rec insertdash(stringlist, height, n)=
			if n =0 then nthlist(stringlist, height, "", []) 
			else insertdash(nthlist(stringlist, height, "-", []),height, n-1)
		in
		let rec insertspace(stringlist, height, n)=
			if n=0 then nthlist(stringlist, height, "",[])
			else insertspace(nthlist(stringlist, height, " ",[]),height, n-1)
		in
		match po with(*
		|LEFT -> insertspace(nthlist(stringlist, height, "|", []), height, number(rank-height))
		|RIGHT -> insertdash(nthlist(stringlist, height, "|", []), height, number(rank-height)) 
		|CENTER -> insertspace(nthlist(stringlist, height, "|", []), height, number(rank-height)) 
	*)
		|LEFT -> nthlist(insertdash(stringlist, height, number(rank-height)), height, "|", [])
		|RIGHT -> nthlist(insertspace(stringlist, height, number(rank-height)), height, "|", [])
		|CENTER -> nthlist(stringlist, height, "|", [])
	in
	match (tourna, po) with
	| (LEAF a, LEFT) -> if height < rank then nthlist(stringlist, height, "|", [])
						else nthlist(stringlist, height, "|-", [])
	| (LEAF a, RIGHT) -> if height < rank then nthlist(stringlist, height, "|", [])
						 else nthlist(stringlist, height, "| ", [])
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


let rec pptree(tourna) =
	let rank = deep(tourna,0)
	in
	printresult(lastresult(dotourna(tourna,0, rank,[], CENTER), 0, rank));
	(*
	printresult(dotourna(tourna, 0, rank, [], CENTER))*)

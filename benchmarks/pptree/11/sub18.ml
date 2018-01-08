type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina 
type tourna = LEAF of team
| NODE of tourna * tourna

exception CANNOT


	let rec getHeight t =
		match t with
			| NODE(n1, n2) -> 1+(max (getHeight n1) (getHeight n2))
			| LEAF le -> 1

	let rec makeSeqList height =
		let sub addh sublist=
			List.append (List.map (fun x -> x+1) sublist) (List.map (fun x -> x+addh) sublist)
		in	
		if height=1 then [1]::[]
		else if height>1 then [1]::(List.map (sub (int_of_float (2.0**(float_of_int (height-1))))) (makeSeqList(height-1)))
		else raise CANNOT

	let rec stringtree (t,hei) = (* reverse of tour(complete binary tree(include space))'s DFS list *) 
		let rec ff(hhei) =
			if hhei = 1 then " "::[]
			else if hhei> 1 then List.append (ff(hhei-1)) (List.append (ff(hhei-1)) (" "::[]))
			else raise CANNOT
		in
		
		match (t,hei) with
			| (NODE(n1,n2),hei) -> 
				(List.append  ("|"::[]) (List.append (stringtree (n1,hei-1)) (stringtree (n2,hei-1))))
			| (LEAF le, hei) -> 
				if hei=1 then "|"::[]
				else if hei>1 then
					(List.append  ("|"::[]) (List.append (stringtree (LEAF le,hei-1)) (ff (hei-1))))
				else raise CANNOT

let rec pptree tour = 
  let height = getHeight tour in
	let treeSeqList = makeSeqList height in
	let compTreeList = stringtree (tour, height)  in
	
	  (*1st level*)
	for i=1 to (int_of_float (2.0**(float_of_int (height-1))))-1 do
		print_string " ";
	done;
	print_string (List.nth compTreeList ((List.nth (List.nth treeSeqList 0) 0)-1));
	print_newline();
			
	if height>=2 then(
			(*2nd level*)
			for i=1 to (int_of_float (2.0**(float_of_int (height-2))))-1 do
				print_string " ";
			done;
			print_string (List.nth compTreeList ((List.nth (List.nth treeSeqList 1) 0)-1));
			for i=1 to (int_of_float (2.0**(float_of_int (height-1))))-1 do
				print_string "-";
			done;
			print_string (List.nth compTreeList ((List.nth (List.nth treeSeqList 1) 1)-1));
			print_newline();
		
		if height>=3 then(
		(*3rd level*)	
		for level=3 to height do
			for k=1 to (int_of_float (2.0**(float_of_int (level-2)))) do
				for i=1 to (int_of_float (2.0**(float_of_int (height-level))))-1 do
					print_string " ";
				done;
		
				if (List.nth compTreeList ((List.nth (List.nth treeSeqList (level-1)) (2*k-1))-1))<>" " then(
					print_string (List.nth compTreeList ((List.nth (List.nth treeSeqList (level-1)) (2*k-1-1))-1));
					for i=1 to (int_of_float (2.0**(float_of_int (height-level+1))))-1 do
						print_string "-";
					done;
					print_string (List.nth compTreeList ((List.nth (List.nth treeSeqList (level-1)) (2*k-1))-1));
					for i=1 to (int_of_float (2.0**(float_of_int (height-level))))-1 do
						print_string " ";
					done;
					print_string " ";)
				else (
						for i=1 to (int_of_float (2.0**(float_of_int (height-level)))) do
							print_string " ";
						done; 
						print_string (List.nth compTreeList ((List.nth (List.nth treeSeqList (level-1)) (2*k-1-1))-1));
						for i=1 to (int_of_float (2.0**(float_of_int (height-level)))) do
							print_string " ";
						done; 
						for i=1 to (int_of_float (2.0**(float_of_int (height-level))))-1 do
							print_string " ";
						done;
						print_string " ";);
					
					
					(*
					for i=1 to (int_of_float (2.0**(float_of_int (height-level)))) do
						print_string " ";
					done; 
					if (List.nth compTreeList ((List.nth (List.nth treeSeqList (level-1)) (2*k))-1))!=" " then(
						print_string (List.nth compTreeList ((List.nth (List.nth treeSeqList (level-1)) (2*k-1-1))-1));
						for i=1 to (int_of_float (2.0**(float_of_int (height-level)))) do
							print_string " ";
						done; 
						for i=1 to (int_of_float (2.0**(float_of_int (height-level))))-1 do
							print_string " ";
						done;
						print_string " ";)
					else(
						let rre = ref 1;
						while (List.nth compTreeList ((List.nth (List.nth treeSeqList (level-1)) (2*!rre))-1))!=" " ||
						(List.length (List.nth treeSeqList (level-1)))>(2*!rre) do
							rre := !rre+1;
						done;
						(* kkkkkkkkkkkkkkkkkkkk *)
						);
						);*)
		  
			done;
			
			print_newline();
		done;		
	););
print_newline()

let a = LEAF Korea 
let b = LEAF Japan 
let c = NODE (a, b) 
let d = NODE (c, a) 
let e = NODE (b, c) 
let f = NODE (c, c) 
let g = NODE (d, a) 
let h = NODE (a, d) 
let i = NODE (a, g) 
let j = NODE (h, d) 
let k = NODE (i, j) 
let l = NODE (c, c) 
let m = NODE (l, l) 
let n = NODE (m, m) 
let o = NODE (n, n) 
let p = NODE (o, o) 

let _ = pptree p

		





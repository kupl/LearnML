type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina
type tourna = LEAF of team
	| NODE of tourna * tourna

let pptree : tourna -> unit =
	let rec noSpace str =
		if (String.get str 0) = ' ' then noSpace (String.sub str 1 ((String.length str)-1))
		else str in
	let mkSpace n =
		let rec iter (str,n)=
			if n = 0 then str
			else iter ((String.concat "" [" ";str]),n-1) in
		iter ("",n) in
	let mkVar n =
		let rec iter (str,n)=
			if n = 0 then str
			else iter ((String.concat "" ["-";str]),n-1) in
		iter ("",n) in
	let varToSpace str =
		let rec iter (str,n) =
			if n = (String.length str) then str
			else if (String.get str n) = '-' then iter (String.concat " " [(String.sub str 0 n);(String.sub str (n+1) ((String.length str)-n-1))],n+1)
			else iter (str,n+1) in
		iter (str,0) in
	let space str =
		let rec iter (str,n) = 
			if (String.get str 0) = ' ' then iter ((String.sub str 1 ((String.length str)-1)),n+1)
			else n in
		iter (str, 0)in
	let width lst =
		let rec f (str, n) = 
			if String.length str = 0 then n
			else (match (String.get str 0) with '-' -> f ((String.sub str 1 ((String.length str)-1)),n+1)
				|_ -> f ((String.sub str 1 ((String.length str)-1)),n)) in
		if ((List.length lst) < 2) then 1 else
		f ((List.nth lst 1),0) in 
	let merge (lst1,lst2) = 
		let rec iter (lst1,lst2,mode,n,c1,c2)=
			match (lst1,lst2) with
				([h1],[h2]) -> if (mode = 0) then [(String.concat (mkVar n) [h1;(noSpace h2)])]
						else [(String.concat (mkSpace (n - (String.length h1)+ c1 - c2 + 1)) [h1;h2])] 
				|(h1::t1,h2::t2) -> if (mode = 0) then (String.concat (mkVar n) [h1;(noSpace h2)]) ::  iter ((if t1=[] then [varToSpace h1] else t1),(if t2=[] then [varToSpace h2] else t2),1,n,c1,c2)
						   else (String.concat (mkSpace (n -(String.length h1) + c1 - c2 + 1 )) [h1;h2]) ::  iter ((if t1=[] then [varToSpace h1] else t1),(if t2=[] then [varToSpace h2] else t2),1,n,c1,c2) 
				|(_,_) -> [] in
		if (List.length lst1 = 1) && (List.length lst2 = 1) then [" |";"|-|"] else
		(String.concat "" [mkSpace ((space (List.nth lst1 0)) + ((width lst1) + (width lst2) + 1)/2 +1);"|"]) :: iter (lst1,lst2,0,(width lst1) + (width lst2) + 1,(space (List.nth lst1 0))+1,(space (List.nth lst2 0)+1)) in
	let rec mklst : tourna -> string list =
		fun t -> match t with LEAF a -> ["|"]
			|NODE (t1,t2) -> merge (mklst t1,mklst t2) in
	let printer lst = 
		Printf.printf "%s" (String.concat "\n" lst);
		Printf.printf "\n" in
	fun t -> printer (mklst t)

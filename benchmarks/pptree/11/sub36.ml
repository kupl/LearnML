type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let pptree tr =

	let getmax(x, y) = if(x>y)then x else y in
	let rec copystr (str, n) = if(n==0)then "" else (str^(copystr(str,n-1))) in

	let f lst n =
		let rec g str =
			let strn = (String.length str) in
			if(strn==0) then ""
			else(
				if((String.compare (String.sub str 0 1) "-")==0)then (" "^(g (String.sub str 1 (strn-1))))
				else ((String.sub str 0 1)^(g (String.sub str 1 (strn-1))))
			)
		in
		let rec lline lst = (match lst with [] -> "|" | [h] -> h | h::t -> (lline t)) in
		let last_str=(g (lline lst)) in
		let rec h (lst, str, n) =
			if(n==0)then lst
			else (List.append (h (lst,str,n-1)) [str])
		in
		h (lst, last_str, n)
	in

	let rec sumlr (lstr, rstr, sp) = match (lstr,rstr) with
		([], _) -> []
		| (_, []) -> []
		| (l::lb, r::rb) -> (l^(copystr (" ", sp))^r)::(sumlr (lb, rb, sp))
	in

	let rec treestr tr =
		match tr with
		LEAF x -> (["|"],0,0)
		| NODE (l, r) -> (let (tlstr,l1,l2)=(treestr l) in
				  let (trstr,r1,r2)=(treestr r) in
				  match (tlstr, trstr) with
				  ([], _) -> ([],0,0) | (_,[]) -> ([],0,0)
				  | (lt::lstr, rt::rstr) ->(
				  let cl=getmax(l2,r1) in
				  let ld=(List.length lstr) in
				  let rd=(List.length rstr) in
				  let md=getmax(ld,rd) in
				  ( (copystr(" ",l1+1+cl)^"|"^copystr(" ",cl+1+r2))::(
				(copystr(" ",l1)^"|"^copystr("-",cl*2+1)^"|"^copystr(" ",r2))::(sumlr((f lstr (md-ld)),(f rstr (md-rd)),1+cl+cl-l2-r1)))
				, l1+1+cl, cl+1+r2)
				))
	in

	let (anslst,_,_)=(treestr tr) in

	print_endline(String.concat "\n" anslst)

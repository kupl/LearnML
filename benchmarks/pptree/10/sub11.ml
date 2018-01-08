type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Sweden | England
          | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna


(* returns (width, root position) *)
let rec getWidth tree = match tree with
  LEAF _ -> (1, 0)
| NODE(t1, t2) ->
	let (w1, pos1) = getWidth t1 in
	let (w2, pos2) = getWidth t2 in
	let gap = (w1 - pos1 + pos2) / 2 * 2 + 1 in
	(pos1 + gap + w2 - pos2 + 1, pos1 + gap / 2 + 1)


(* type queue = (tourna * int "line number" * int "starting position of tree") list *)
let rec makeList (list, queue) =
	let rec addList (list, n, x) =
		if n = 0 then (x::(List.hd list))::(List.tl list)
		else (List.hd list)::(addList (List.tl list, n-1, x))
	in
	
	match queue with
	  [] -> list
	| (tree, line, pos)::nextQ -> match tree with
		  LEAF _ ->
			if line = (List.length list) then
				makeList (list, nextQ)
			else
				let newList = addList (list, line, (pos, pos + 1)) in
				let newQ = nextQ@[(tree, line + 1, pos)] in
				makeList (newList, newQ)
		| NODE(t1, t2) ->
			let (width1, pos1) = getWidth t1 in
			let (width2, pos2) = getWidth t2 in
			let gap = 2 - ((width1 - pos1 + pos2) mod 2) in
			let newList = addList (list, line, (pos + pos1, pos + width1 + gap + pos2 + 1)) in
			let newQ = nextQ@[(t1, line + 1, pos); (t2, line + 1, pos + width1 + gap)] in
			
			makeList (newList, newQ)


let printFirstLine tree =
	let (_, sp) = getWidth tree in (
	print_string (String.make sp ' ');
	print_endline "|")

let printLine list =
	let rec pLine (list, i) = match list with
	  [] -> print_endline ""
	| (a, b)::next -> (
		print_string (String.make (a - i) ' ');
		print_string "|";
		if b = a + 1 then print_string ""
		else (
			print_string (String.make (b-a-2) '-');
			print_string "|");
		pLine (next, b))
	in
	pLine (List.rev list, 0)


let pptree tree =
	let rec emptyList n =
		if n <= 0 then [] else []::(emptyList (n-1))
	in
	
	let rec getHeight tree = match tree with
	  LEAF _ -> 1
	| NODE(t1, t2) ->
		let d1 = getHeight t1 in
		let d2 = getHeight t2 in
		if d1 >= d2 then d1 + 1
		else d2 + 1
	in
	
	let h = getHeight tree in (
	printFirstLine tree;
	List.iter printLine (if h > 1 then makeList (emptyList(h - 1), [(tree, 0, 0)])
						 else []))
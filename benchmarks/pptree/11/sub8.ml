(* 2006-11867 Jo, Dong-Chul *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina

type tourna = LEAF of team
| NODE of tourna * tourna

let two'spow : int -> int = fun n ->
	int_of_float(2. ** (float_of_int n))

let rec makestr : string * int -> string = fun (str, n) ->
	if n < 0 then raise (Invalid_argument "n of makestr is less than 0")
	else if n = 0 then ""
	else str^(makestr (str, n-1))

let tournastr : tourna * int -> string = fun (tour, height) ->
	match tour with
	| LEAF _ ->
		if height >= 1 then makestr(" ", (two'spow height)-1)^"|"^makestr(" ", (two'spow height)-1)
		else(* if height <=0 then *)raise (Invalid_argument "height of tournastr equal less than 0")
	| NODE (_, _) ->
		if height >= 1 then makestr(" ", (two'spow (height-1))-1)^"|"^makestr("-", (two'spow (height-1))-1)^"-"^makestr("-", (two'spow (height-1))-1)^"|"^makestr(" ", (two'spow (height-1))-1)
		else(* if height <=0 then *)raise (Invalid_argument "height of tournastr equal less than 0")

let rec replace : 'a list * int * 'a -> 'a list = fun (l,n,e) ->
	match l with
	| [] -> []
	| el::rl ->
		if n < 0 then raise(Invalid_argument "n of replace is smaller than 0")
		else if n >= List.length l then raise(Invalid_argument "n of replace is larger than length of list")
		else if n = 0 then e::rl
		else el::replace(rl,n-1,e)

let rec tourna_to_strlist : tourna * string list * int * int -> string list = fun (tour, l, height, num_of_height) ->
	match tour with
	| LEAF _ ->
		if height < 0 then raise (Invalid_argument "height of tourna_to_strlist is less than 0")
		else if height = 0 then l
		else if (String.length (List.nth l (height-1))) = 0 then tourna_to_strlist(tour, replace(l, height-1, tournastr(tour, num_of_height)), height-1, num_of_height)
		else tourna_to_strlist(tour, replace(l, height-1, (List.nth l (height-1))^" "^tournastr(tour, num_of_height)), height-1, num_of_height)
	| NODE (lt, rt) ->
		if (String.length (List.nth l (height-1))) = 0 then tourna_to_strlist(rt, tourna_to_strlist(lt, replace(l, height-1, tournastr(tour, num_of_height)), height-1, num_of_height-1), height-1, num_of_height-1)
		else tourna_to_strlist(rt, tourna_to_strlist(lt, replace(l, height-1, (List.nth l (height-1))^" "^tournastr(tour, num_of_height)), height-1, num_of_height-1), height-1, num_of_height-1)

let pptree : tourna -> unit = fun tour ->
	(* treeheight return height of tree which has tourna as root *)
	let rec treeheight : tourna -> int = fun root ->
		match root with
		| LEAF _ -> 0
		| NODE (lt, rt) ->
			if (treeheight lt) > (treeheight rt) then (treeheight lt)+1
			else (treeheight rt)+1
	in
	(
		let rec makestrlist : string list * int -> string list = fun (l,n) ->
			if n < 0 then raise (Invalid_argument "n of makestrlist is less than 0")
			else if n = 0 then l
			else makestrlist(""::l,n-1)
		in
		(
			let h = treeheight tour
			in
			(
				let list = makestrlist([], h)@(makestr(" ", (two'spow h)-1)^"|"^makestr(" ", (two'spow h)-1))::[]
				in
				(
					let resultlist = tourna_to_strlist(tour, list, h, h)
					in
					(
						List.iter (fun x -> print_newline(print_string x)) (List.rev resultlist)
					)
				)
			)

		)
	)

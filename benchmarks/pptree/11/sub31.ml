(* 2009-11824 Jieun-Jeong HW2-1 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina
type tourna = LEAF of team
	| NODE of tourna * tourna


let pptree tr =

	let rec make_str length flag =
		if length = 0	then ""
		else if flag = 0 then " " ^ (make_str (length - 1) flag)
		else "-" ^ (make_str (length - 1) flag)
	in
	let max a b = if b > a then b else a
	in
	let rec mulmul a b = if b = 0 then 1 else a*(mulmul a (b-1))
	in
	let rec merge_sublist input = (* merge_sublist : 두 리스트를 받아 새로운 string 리스트를 생성 *)
		match input with
		|(cd, dl, al::ll, dr, ar::lr)	-> 	
											let margin_length = ((mulmul 2 (cd - 1)) - 1) in
											let margin_length2 = ((String.length ar) - (String.length al)) / 2 in
											let margin_length3 = ((String.length al) - (String.length ar)) / 2 in
											let ch str	= if (String.length str) = (String.length "|-|") then "| |" else str in
											if dl = 1 && dr = 1 
												then [(make_str margin_length 0) ^ al ^ (make_str (2*(margin_length) + 1) 1) ^ ar ^ (make_str margin_length 0)]
											else if dr > dl   
												then (((make_str margin_length2 0)^(ch al)^(make_str margin_length2 0))^" "^ar)::(merge_sublist (cd, dl, al::ll, (dr-1), lr))
											else if dl > dr 
												then (al^" "^((make_str margin_length3 0)^(ch ar)^(make_str margin_length3 0)))::(merge_sublist (cd, (dl-1), ll, dr, ar::lr))
											else if (String.length ar) > (String.length al) 
												then (((make_str margin_length2 0)^al^(make_str margin_length2 0))^" "^ar)::(merge_sublist (cd, (dl - 1), ll, (dr - 1), lr))
											else if (String.length al) > (String.length ar)
												then (al^" "^((make_str margin_length3 0)^ar^(make_str margin_length3 0)))::(merge_sublist (cd, (dl - 1), ll, (dr - 1), lr))
											else
												(al ^ " " ^ ar)::(merge_sublist (cd, (dl - 1), ll, (dr - 1), lr))	

		|_	-> raise (Invalid_argument "merge_sublist")
	in
	let rec list_to_tree tr =
		match tr with
		|LEAF _			-> (1, ["|"])
		|NODE (l, r)	-> let (dl, ll) = list_to_tree l in
						   let (dr, lr) = list_to_tree r in
						   let child_depth = (max dl dr) in
						   ((child_depth + 1), ((merge_sublist (child_depth, dl, ll, dr, lr))@["|"]))
	in
	let rec printlst lst depth count=
		match lst with
		|a::l			-> if count = 1
								then 
									let first_line = (make_str ((mulmul 2 (depth - 1)) - 1) 0) ^ "|" ^ (make_str ((mulmul 2 (depth - 1)) - 1) 0) in
									let _ = print_string first_line in
									let _ = print_newline() in
									(printlst l depth (count + 1))
								else
									let _ = print_string a in
						   			let _ = print_newline() in
									(printlst l depth (count + 1))
		|[]			    -> () 
	in
	let (depth, str_lst) = (list_to_tree tr)
	in
	(printlst (List.rev str_lst) depth 1)

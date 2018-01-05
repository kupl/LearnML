let rec merge : (int list * int list) -> int list = fun pairList ->
	if fst pairList == [] then snd pairList
	else if snd pairList == [] then fst pairList
	else if List.hd (fst pairList) > List.hd (snd pairList) then
		List.hd (fst pairList) :: merge(List.tl(fst pairList), snd pairList)
	else 
		List.hd (snd pairList) :: merge(fst pairList, List.tl(snd pairList))

(*let rec print_list : int list -> unit = fun li ->
	let _ = print_endline (List.hd li);print_list (List.tl li)*)

(*let _ = print_list (merge ([5;3;1], [7;2;0]))*)

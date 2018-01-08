type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let pptree t =
	let rec tree_line ts =
		let rec all_leaf tts =
			match tts with
				[] -> true
				| (h,_,_,_)::t -> (match h with NODE _ -> false | LEAF _ -> all_leaf t)
		in
			let rec children tts =
				match tts with
					[] -> []
					| (h,d,_,_)::t -> (match h with NODE (t1,t2) -> (t1,d-1," ","-")::(t2,d-1,"-"," ")::(children t) | LEAF t1 -> (h,d," "," ")::(children t))
				in
					let rec pp_tourna_list tts =
						let rec string_repeat s n =
							if (n<=0) then ""
							else s^(string_repeat s (n-1))
						in
							let rec pow2 n =
								if (n<=0) then 1
								else 2*(pow2 (n-1))
							in
								match tts with
									[] -> print_endline ""
									| (h,d,s1,s2)::t -> ((print_string ((string_repeat s1 (pow2 d))^"|"^(string_repeat s2 ((pow2 d)-1))));(pp_tourna_list t))
					in
						((pp_tourna_list ts);(if (all_leaf ts) then () else (tree_line (children ts))))
	in
		let rec depth tt =
			match tt with
				LEAF _ -> 0
				| NODE (t1,t2) -> max (depth t1) (depth t2)+1
		in
			tree_line [(t,depth t," "," ")]

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
              | Poland | Portugal | Italy | Germany | Norway | Sweden | England
              | Argentina
type tourna = LEAF of team
			| NODE of tourna * tourna
			
let rec parenize t =
	match t with
		| LEAF t_ -> 
			(match t_ with
				| Korea -> "Korea"
				| France -> "France"
				| Usa -> "Usa"
				| Brazil -> "Brazil"
				| Japan -> "Japan"
				| Nigeria -> "Nigeria"
				| Cameroon -> "Cameroon"
				| Poland -> "Poland"
				| Portugal -> "Portugal"
				| Italy -> "Italy"
				| Germany -> "Germany"
				| Norway -> "Norway"
				| Sweden -> "Sweden"
				| England -> "England"
				| Argentina -> "Argentina")
		| NODE (t_1, t_2) -> "(" ^ (parenize t_1) ^ " " ^ (parenize t_2) ^ ")"

(* TEST SET *)
(*
let _ =
    print_string "parenize Test Set\n";
    print_string (parenize (LEAF Korea));
    print_newline ();
    print_string (parenize (NODE (LEAF Korea, LEAF Portugal)));
    print_newline ();
    print_string (parenize (NODE (NODE (NODE (LEAF Korea, LEAF Portugal), LEAF Usa), LEAF Brazil)));
    print_newline ();
    print_string (parenize (NODE (NODE (NODE (LEAF Korea, LEAF Portugal), NODE (LEAF Italy, LEAF Usa)), LEAF Brazil)));
    print_newline ();
    print_string (parenize (NODE (NODE (NODE (LEAF Korea, LEAF Portugal), NODE (LEAF Poland, NODE (LEAF Italy, LEAF Usa))), LEAF Brazil)));
    print_newline (); print_newline()
*)
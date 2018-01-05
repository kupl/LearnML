(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-4 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let get_string_from_team s = match s with Korea -> "Korea" | France -> "France" | Usa -> "Usa" | Brazil -> "Brazil" | Japan -> "Japan" | Nigeria -> "Nigeria" | Cameroon -> "Cameroon" | Poland -> "Poland" | Portugal -> "Portugal" | Italy -> "Italy" | Germany -> "Germany" | Sweden -> "Sweden" | England -> "England" | Croatia -> "Croatia" | Argentina -> "Argentina"

let rec parenize t = match t with LEAF s -> get_string_from_team s
			| NODE (a, b) -> String.concat (String.concat " " [parenize a; parenize b]) ["("; ")"]

(*
(* test code *)

let _ = print_endline (parenize (NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)))
*)

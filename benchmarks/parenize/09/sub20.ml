(* Department: EE
 * Student No.: 2009-20769
 * Name: Kim, Seongjun
 * Exercise 1
 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
            | Poland | Portugal | Italy | Germany | Sweden | England
            | Croatia | Argentina

type tourna = LEAF of team
              | NODE of tourna * tourna

let rec toParen t =
  let string_of_team t =
    match t with
	Korea -> "Korea"
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
      | Sweden -> "Sweden" 
      | England -> "England"
      | Croatia -> "Croatia" 
      | Argentina -> "Argentina"
  in
    match t with
	LEAF team -> string_of_team team
      | NODE (t1, t2) -> "(" ^ toParen t1 ^ " " ^ toParen t2 ^ ")"

(*
;;
assert (toParen(LEAF Korea) = "Korea");;
assert (toParen(NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)) = "((Korea Portugal) Brazil)");;
*)

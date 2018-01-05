(* Department: EE
 * Student No.: 2009-20769
 * Name: Kim, Seongjun
 * Exercise 2
 *)

(* 1.ml *)
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
(* 1.ml end *)

exception NullTree

let drop (tourna, team) =
  let rec drop_team (tourna:tourna) (team:team) = 
    match tourna with
	NODE (l, r) -> (
	  try
	    let dropped_l_tree = drop_team l team in
	      try
		let dropped_r_tree = drop_team r team in
		  NODE (dropped_l_tree, dropped_r_tree)
	      with NullTree ->   (* right sub-tree is Null *)
		dropped_l_tree
	  with NullTree -> 
	    try    (* left sub-tree is Null *)
	      let dropped_r_tree = drop_team r team in
		dropped_r_tree
	    with NullTree ->  (* both sub-trees are Null *)
	      raise NullTree
	)

      | LEAF t -> 
	  if t = team then
	    raise NullTree
	  else
	    LEAF t
  in
    try
      toParen (drop_team tourna team)
    with NullTree ->
      ""

(*
;;
assert (drop (NODE(LEAF Korea, LEAF Portugal), Brazil) = "(Korea Portugal)");;

assert (drop (NODE(LEAF Korea, LEAF Portugal), Portugal) = "Korea");;

assert (drop ((NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)), Korea) = "(Portugal Brazil)");;

assert (drop ((NODE(NODE(LEAF Korea, LEAF Brazil), LEAF Korea)), Korea) = "Brazil");;

assert (drop ((NODE(NODE(LEAF Korea, LEAF Korea), (NODE (LEAF Korea, LEAF Korea)))), Korea) = "");;
*)

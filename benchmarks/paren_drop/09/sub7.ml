type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		| Poland | Portugal | Italy | Germany | Sweden | England
		| Croatia | Argentina
type tourna = LEAF of team
		| NODE of tourna * tourna

let rec toParen tn =
	let tostring t =
		match t with
			| Korea -> "Korea" | France -> "France" | Usa -> "Usa" | Brazil -> "Brazil"
			| Japan -> "Japan" | Nigeria -> "Nigeria" | Cameroon -> "Cameroon" | Poland -> "Poland"
			| Portugal -> "Portugal" | Italy -> "Italy" | Germany -> "Germany" | Sweden -> "Sweden" | England -> "England"
			| Croatia -> "Croatia" | Argentina -> "Argentina" in
	match tn with
		| LEAF t -> tostring( t )
		| NODE( a, b ) -> "(" ^ ( toParen( a ) ) ^ " " ^ ( toParen( b ) ) ^")";;

let drop ( tn, t ) =
	let rec dropRec( tn, t ) =
		match tn with
			| NODE( a, b ) ->
				if ( ( dropRec( a, t ) = t ) && ( dropRec( b, t ) = t ) ) then t
				else if( ( dropRec( a, t ) ) = t ) then dropRec( b, t )
				else if( ( dropRec( b, t ) ) = t ) then dropRec( a, t )
				else NODE( dropRec( a, t ), dropRec( b, t ) ) 
			| LEAF a -> tn in
	let res = dropRec( tn, LEAF t ) in
	if( res = LEAF t ) then ""
	else toParen ( res );;

(*print_string ( drop(NODE(NODE(LEAF Brazil, LEAF Brazil), LEAF Korea), Korea) );;*)
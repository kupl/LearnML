exception Error of string
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna

let rec drop (tourna, team) =
	let rec toParen tour=
		match tour with
			| NODE(t1, t2) -> "("^(toParen t1)^" "^(toParen t2)^")"
			| LEAF(t) -> 
				match t with
					| Korea->"Korea"
					| France->"France"
					| Usa->"Usa"
					| Brazil->"Brazil"
					| Japan->"Japan"
					| Nigeria->"Nigeria"
					| Cameroon->"Cameroon"
					| Poland->"Poland"
					| Portugal->"Portugal"
					| Italy->"Italy"
					| Germany->"Germany"
					| Sweden->"Sweden"
					| England->"England"
					| Croatia->"Croatia"
					| Argentina->"Argentina"
	in
	let rec drop_sub (tourna_, team_) =
		match tourna_ with
			| LEAF(t1) ->raise (Error "what?")
			| NODE(NODE(t11, t12), NODE(t21, t22)) ->NODE(drop_sub(NODE(t11, t12), team_), drop_sub(NODE(t21, t22),team_))
			| NODE(NODE(t11, t12), LEAF(t23))->if t23=team_ then drop_sub(NODE(t11, t12), team_)
														else NODE(drop_sub(NODE(t11, t12), team_) , LEAF(t23))
			|NODE(LEAF(t13), NODE(t21, t22))-> if t13=team_ then drop_sub (NODE(t21, t22), team_)
																	else NODE(LEAF(t13), drop_sub(NODE(t21, t22), team_))
			|NODE(LEAF(t13), LEAF(t23)) -> if t13=team_ then LEAF(t23)
														else if t23=team_ then LEAF(t13)
														else NODE(LEAF(t13), LEAF(t23))
			
	in
	match tourna with
		| LEAF(t1) ->if t1=team then ""
									else toParen(tourna)
		| NODE(t1, t2)->
	if(toParen (drop_sub (tourna, team))=toParen(tourna)) then toParen(tourna)
	else drop(drop_sub(tourna, team), team)
		
	

		
	
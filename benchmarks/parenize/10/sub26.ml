(* Exercise 4 *)
type team =
	Korea
	| France
	| Usa
	| Brazil
	| Japan
	| Nigeria
	| Cameroon
	| Poland
	| Portugal
	| Italy
	| Germany
	| Sweden
	| England
	| Croatia
	| Argentina

type tourna =
	LEAF of team
	| NODE of tourna * tourna

let string_of_team team =
	match team with
		Korea ->
			"Korea"
		| France ->
			"France"
		| Usa ->
			"Usa"
		| Brazil ->
			"Brazil"
		| Japan ->
			"Japan"
		| Nigeria ->
			"Nigeria"
		| Cameroon ->
			"Cameroon"
		| Poland ->
			"Poland"
		| Portugal ->
			"Portugal"
		| Italy ->
			"Italy"
		| Germany ->
			"Germany"
		| Sweden ->
			"Sweden"
		| England ->
			"England"
		| Croatia ->
			"Croatia"
		| Argentina ->
			"Argentina"

let rec parenize t =
	match t with
		LEAF a ->
			(string_of_team a)
		| NODE (a, b) ->
			"(" ^ (parenize a) ^ " " ^ (parenize b) ^ ")"

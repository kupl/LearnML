type nat = ZERO | SUCC of nat

let rec natadd (form, lat)=
	match form with
	|ZERO -> lat
	|SUCC(formsucc) ->
		match lat with
			|ZERO -> form
			|_ -> natadd(formsucc, SUCC(lat))

let rec natmul (form, lat)=
	match form with
	|ZERO -> ZERO
	|SUCC(ZERO) -> lat
	|SUCC(formsucc)->
		match lat with
			|ZERO -> ZERO
			|_ -> natadd(lat, natmul(formsucc, lat))

	

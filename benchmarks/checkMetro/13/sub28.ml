type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro met=
	let rec return met lst=
		let rec find a lst=
			match lst with
			|[]->false
			|head::tail->
				if head=a then true
				else find a tail
		in
		match met with
		|STATION a-> find a lst
		|AREA(a,b)-> return b (a::lst)
		|CONNECT(a,b)->(return a lst)&&(return b lst)
	in
	return met []	

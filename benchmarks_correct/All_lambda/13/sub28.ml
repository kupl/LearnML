type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check met=
	let rec return met lst=
		let rec find a lst=
			match lst with
			|[]->false
			|head::tail->
				if head=a then true
				else find a tail
		in
		match met with
		|V a-> find a lst
		|P(a,b)-> return b (a::lst)
		|C(a,b)->(return a lst)&&(return b lst)
	in
	return met []	

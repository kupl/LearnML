type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let check = fun lambda ->
	let rec checkhelper = fun met nlist ->
		match met with
		|P(id, m) -> (checkhelper m (id::nlist))
		|V n ->
			(if (List.mem n nlist) then true
			else false)
		|C(m1, m2) -> (checkhelper m1 nlist) && (checkhelper m2 nlist)
	in (checkhelper lambda [])

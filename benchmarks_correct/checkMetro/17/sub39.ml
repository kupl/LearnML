type lambda = 	V of var
		| P of var * lambda
		| C of lambda * lambda
and var = string

let rec check m =
	let rec checkProxy n areaList =
		match n with
		| V x -> List.mem x areaList
		| P (x, y) -> checkProxy y (x :: areaList)
		| C (x, y) -> (checkProxy x areaList) && (checkProxy y areaList)
	in
	checkProxy m []


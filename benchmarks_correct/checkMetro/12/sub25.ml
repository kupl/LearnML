type lambda =
	V		of var
	|P		of var * lambda
	|C	of lambda * lambda
and var = string

let rec check met =
	
	let rec checkTmp areaList metTemp =
		match metTemp with
		V a	-> List.mem a areaList
		|P (a, b)	-> checkTmp (a::areaList) b
		|C (a, b)	-> (checkTmp areaList a) && (checkTmp areaList b)
	in

	match met with
	V a	-> false
	|P (a, b)	-> checkTmp [a] b
	|C (a, b)	-> (check a) && (check b)

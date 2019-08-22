type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let check _lambda =
	let rec checkRec (_lambda, _areaList) =
		match _lambda with
		V(_var) -> List.mem _var _areaList
		| P(_var, __lambda) -> checkRec(__lambda, _var::_areaList)
		| C(_lambda1, _lambda2) -> checkRec(_lambda1, _areaList) &&  checkRec(_lambda2, _areaList)
	in

	checkRec(_lambda, [])


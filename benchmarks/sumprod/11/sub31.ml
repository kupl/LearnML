(* 2009-13384, CHO Hyunik *)



let sumprod(matrix, n, k) =

(* LOCAL FUNCTIONS *)

	(* calculate sum of single column's elements *)
	let rec rowsSigma(rowNum, n) =
		match n>0 with
		false -> 0.0
		| true -> (rowNum n) +. rowsSigma(rowNum, n-1) in

	(* calculate product of single row's elements *)
	let rec productSigma(columnNum, k) =
		match k>0 with
		false -> 1.0
		| true -> (columnNum k) *. productSigma(columnNum, k-1) in

(* LOCAL FUNCTIONS END *)

	match (n>0, k>0) with
	(true, _) -> 0.0
	| (false, false) ->
		let productResult row = productSigma((fun x -> matrix row x), k) in
		(* productResult : product of each rows' elements *)
		rowsSigma(productResult, n)
	| (false, true) -> 0.0
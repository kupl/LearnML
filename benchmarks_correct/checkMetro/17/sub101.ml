(* 2015-11380 박찬양 HW2_4 *)

type lambda = V of var
						| P of var * lambda
						| C of lambda * lambda
	and var = string

let check: lambda -> bool = fun mtr ->
	let rec checkIn mtrr arealist =
	 match mtrr with
	 | P (a,b) -> checkIn b (a::arealist)
	 | V a -> (List.mem a arealist)
	 | C (a,b) -> ((checkIn a arealist) && (checkIn b arealist))
	in checkIn mtr []

	
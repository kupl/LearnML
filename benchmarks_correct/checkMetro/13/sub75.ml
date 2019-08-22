(* file var : ex4.ml *)
(* author : Jisoon Park (jspark@ropas.snu.ac.kr) *)
(* date : 2013-09-13 *)
(* Exercise 4 *)
type lambda = V of var
						| P of var * lambda
						| C of lambda * lambda
and var = string

let rec checkSub : lambda * string list -> bool
 = fun (mtr, areaList) ->
	match mtr with
	| V st -> List.exists (fun x -> x = st) areaList
	| P (ar, mtr2) -> checkSub(mtr2, ar::areaList)
	| C (mtr1, mtr2) -> checkSub(mtr1, areaList) && checkSub(mtr2, areaList) 

let check : lambda -> bool
 = fun mtr ->
	checkSub (mtr, [])

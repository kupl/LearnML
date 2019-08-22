
type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let check met =
	let rec sub_check (checklist, metr) =
		match metr with
		(V a) ->
			(List.mem a checklist)
		|(P (a,b)) ->
			(sub_check ((a::checklist), b))
		|(C (a,b)) ->
			((sub_check (checklist,a)) 
			 && (sub_check (checklist, b)))
	in
	(sub_check ([], met))

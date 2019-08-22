type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
			and var = string

let check lambda =
		let rec loop mt li =
			match mt with
			| V var -> List.exists (fun x -> x=var) li
			| P (var, lambda) -> loop lambda (li @ [var])
			| C (mt1, mt2) -> (loop mt1 li) && (loop mt2 li) in
				loop lambda []


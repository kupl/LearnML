let rec merge = (fun x -> match x with
				| ([], []) -> []
				| (h:: t, []) | ([], h:: t) -> h:: (merge ([], t))
				| (a:: b, c:: d) -> (if a > c then a:: (merge (b, c:: d))
							else c:: (merge (a:: b, d)))
	);;
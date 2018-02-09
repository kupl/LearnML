let rec filter pred lst =
	fold (fun x l -> if pred x then x::l else l) lst []

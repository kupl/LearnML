let rec merge a b =
	match a with
	|[]->
		(match b with
		|[]->[]
		|hd::tl-> hd :: merge a tl)
	|hd::tl->
		match b with
		|[]-> hd :: merge tl b
		|bhd::btl ->
			if (hd > bhd)
				then hd :: merge tl b
				else bhd :: merge a btl



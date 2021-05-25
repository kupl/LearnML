let rec filter pred lst =
	match lst with
		|[] ->[]
		|h::t -> match pred h with
							|true -> h::filter pred t
							|false -> filter pred t;;  
let rec max l =
	match l with
		[]->0
		|hd::[] -> hd
		|hd::t1 -> if hd >= max t1 then hd else max t1;;
 
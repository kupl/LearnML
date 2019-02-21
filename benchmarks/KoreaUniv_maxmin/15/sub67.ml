let rec max l =
	match l with
		[]->0
		|hd::[] -> hd
		|hd::t1 -> if hd >= max t1 then hd else max t1;;

let rec min l =
	match l with
		[]->0
		|hd::[] -> hd
		|hd::t1 -> if hd <= min t1 then hd else min t1;;

{
	1 -> false;
	2 -> true;
	18 -> false;
	769 -> true;
	270 -> false;
	991 -> true;
}
let rec f n =
	if (n = 1 || n = 0) then false
	else let m = n in
	let rec is_devided d =
		if ((d*d) > m) then true
		else if ((m mod d) = 0) then false
		else is_devided (d+1) in is_devided 2
;;
let rec max : int list -> int
=fun l -> match l with (* TODO *)
[] -> 0
|hd::tl -> if (tl=[]) then hd
else if (hd>=(max tl)) then hd
else max tl;;
 
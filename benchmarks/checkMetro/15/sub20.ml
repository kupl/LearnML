type metro = 
	STATION of name
	|AREA of name * metro
	|CONNECT of metro * metro
and name = string

let rec checkMetro mt = ckM mt []
and ckM mt l =
	match mt with
	STATION(nm)				-> if_cont nm l
	|AREA(nm, mt_a)			-> ckM mt_a (nm::l)
	|CONNECT(mt_a, mt_b)	-> (ckM mt_a l) && (ckM mt_b l)
and if_cont nm l = List.exists (equal nm) l
and equal nm_a nm_b = if nm_a = nm_b then true else false

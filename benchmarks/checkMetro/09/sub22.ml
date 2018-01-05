
type metro = STATION of name
  		| AREA of name * metro
  		| CONNECT of metro * metro
  and name = string
 
let checkMetro metroin = 
	let arealist = ref [] in
	let rec isarea(listin,name) =
		match listin with
			a::sublist -> if a = name then true else isarea(sublist,name)
			| _ -> false
	in
	let rec check met =
		match met with
			AREA(a,submet) -> arealist := a::!arealist; check submet
			| CONNECT(submet1,submet2) -> if (check(submet1) && check(submet2)) then true else false
			| STATION(a) -> if isarea(!arealist,a) then true else false
	in
	check metroin;;
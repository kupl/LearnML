type metro = STATION of name
			|AREA of name * metro
			|CONNECT of metro * metro
and name = string

let rec findStringinList ((l : string list),(name : string)) : bool =
	match l with
		[] -> false
		|hd::tl -> if((hd = name)) then true else findStringinList(tl,name)

let rec checkMetrowithList ((m : metro),(l : string list)) : bool =
	match m with
				STATION(name_) -> if(findStringinList(l,name_)) then true else false
				|AREA(name_,metro_) -> checkMetrowithList(metro_ , name_::l)
				|CONNECT(metro_1,metro_2) -> checkMetrowithList(metro_1,l)&&checkMetrowithList(metro_2,l)

let checkMetro (m : metro) : bool =
		 match m with
		 STATION(name_) -> false
		|AREA(name_,metro_) -> checkMetrowithList(metro_,[name_])
		|CONNECT(metro_1,metro_2) -> checkMetrowithList(metro_1,[])&&checkMetrowithList(metro_2,[])


type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string
let rec com(l,n) =
	match l with
	| []->false
	| head::tail -> ((head = n) || com(tail,n))
let rec cMetro(l, m) = 
	match m with
	| STATION n -> com(l,n)
	| AREA(n,mm) -> cMetro(n::l,mm)
	| CONNECT(m1,m2) -> (cMetro(l,m1)) && (cMetro(l,m2))
let rec checkMetro(m)=
	cMetro([],m)

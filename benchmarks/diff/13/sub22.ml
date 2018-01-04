type ae=
CONST of int
|VAR of string
|POWER of string*int
|TIMES of ae list
|SUM of ae list

exception InvalidArgument

let rec diff (a, x)=
match a with
|CONST a->CONST(0)
|VAR a->if a=x then CONST(1) else CONST(0)
|POWER(a,b) ->if a=x then TIMES(CONST(b)::POWER(a,b-1)::[]) else CONST(0)
|TIMES a->(match a with
		|[]->raise InvalidArgument
		|hd::tl->if tl=[] then diff(hd, x) else
			SUM((TIMES((diff(hd, x))::tl)) :: (TIMES(hd::(diff(TIMES(tl),x))::[])) :: [])
		)
|SUM a->(match a with
		|[]->raise InvalidArgument
		|hd::tl->if tl=[] then diff(hd, x) else
			SUM( diff(hd, x):: diff(SUM(tl), x) :: [])
		)

type aexp=
Const of int
|Var of string
|Power of string*int
|Times of aexp list
|Sum of aexp list

exception InvalidArgument

let rec diff (a, x)=
match a with
|Const a->Const(0)
|Var a->if a=x then Const(1) else Const(0)
|Power(a,b) ->if a=x then Times(Const(b)::Power(a,b-1)::[]) else Const(0)
|Times a->(match a with
		|[]->raise InvalidArgument
		|hd::tl->if tl=[] then diff(hd, x) else
			Sum((Times((diff(hd, x))::tl)) :: (Times(hd::(diff(Times(tl),x))::[])) :: [])
		)
|Sum a->(match a with
		|[]->raise InvalidArgument
		|hd::tl->if tl=[] then diff(hd, x) else
			Sum( diff(hd, x):: diff(Sum(tl), x) :: [])
		)

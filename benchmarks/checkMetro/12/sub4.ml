type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro : metro->bool = fun( met )->
  let rec checkName : string list*metro -> bool = fun( l, m )->
	  match m with
		  AREA(id, mt)-> checkName( id::l, mt )
		  |STATION id -> List.mem id l
			|CONNECT(m1, m2) -> checkName(l, m1)&&checkName(l, m2) in
  checkName([], met)
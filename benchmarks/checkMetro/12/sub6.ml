
type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string


let checkMetro tak =
  	let rec makeList a lst = 
  		match a with
  			STATION s -> List.mem s lst
  			|AREA(x,y) -> makeList y (x::lst)
  			|CONNECT(x,y) -> (makeList x lst) && (makeList y lst)
  	in
  	makeList tak []
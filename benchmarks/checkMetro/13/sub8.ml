type metro = 
    STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro m =
  let isSame a b = 
    if a = b then true
    else false in
  let addItem l i =
    if exists (isSame i) l then l
    else i::l in
  let rec checkMetroWithList m ml = 
    match m with
    | AREA (a, n) -> 
	(checkMetroWithList n (addItem ml a))
    | CONNECT (n, l) -> 
	(checkMetroWithList n ml && checkMetroWithList l ml)
    | STATION s -> 
	if exists (isSame s) ml then true
	else false in
  checkMetroWithList m []
	  

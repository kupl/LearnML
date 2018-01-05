type name = string
type metro = STATION of name
              | AREA of name * metro
							| CONNECT of metro * metro

let rec isContain (st : name) (l : name list) : bool = 
	match l with
	| [] -> false
	| hd::tl -> (if st = hd then true else (isContain st tl))            
		


let rec subCheckMetro (m : metro) (l : name list) : bool =
	match m with
	| STATION n -> isContain n l
	| AREA (nm, mtr) -> subCheckMetro mtr (List.append [nm] l)
	| CONNECT (m1, m2) -> (subCheckMetro m1 l) && (subCheckMetro m2 l) 



let checkMetro (m : metro) : bool =
	subCheckMetro m []
	

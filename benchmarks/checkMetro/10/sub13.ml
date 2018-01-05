type metro = 
STATION of name 
| AREA of name * metro
| CONNECT of metro * metro
and name = string


let rec check (metro, alist) =
	match metro with
		STATION id -> List.mem id alist
		| AREA (id, m) -> check (m, id::alist)
		| CONNECT (m1, m2) -> (check (m1, alist))&&(check (m2, alist));;

let checkMetro metro = check (metro, []);;

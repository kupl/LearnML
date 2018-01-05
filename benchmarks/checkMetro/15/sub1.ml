(* C:\OCaml\lib\CheckMertoMap.ml *)

type name = string

type metro = STATION of name
  |AREA of name * metro
  |CONNECT of metro * metro

let rec check_list (list, name) = 
  match list with
  |[] -> false
  |head::tail ->
      (if head = name then true
	  else (check_list (tail, name)))
  
let rec list_make (list, metro) =
  match metro with
  |STATION a -> check_list (list, a)
  |AREA (a, b) -> list_make (a::list, b)
  |CONNECT (a, b) -> list_make (list, a) && list_make (list, b)

let rec checkMetro metro =
  match metro with
  |STATION a -> false
  |AREA (a, b) -> list_make(a::[], b)
  |CONNECT (a, b) -> (checkMetro a) && (checkMetro b)
      

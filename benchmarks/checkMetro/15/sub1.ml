(* C:\OCaml\lib\CheckMertoMap.ml *)

type name = string

type metro = STATION of name
  |AREA of name * metro
  |CONNECT of metro * metro

let rec check_list (lst, name) = 
  match lst with
  |[] -> false
  |head::tail ->
      (if head = name then true
	  else (check_list (tail, name)))
  
let rec list_make (lst, metro) =
  match metro with
  |STATION a -> check_list (lst, a)
  |AREA (a, b) -> list_make (a::lst, b)
  |CONNECT (a, b) -> list_make (lst, a) && list_make (lst, b)

let rec checkMetro metro =
  match metro with
  |STATION a -> false
  |AREA (a, b) -> list_make(a::[], b)
  |CONNECT (a, b) -> (checkMetro a) && (checkMetro b)
      

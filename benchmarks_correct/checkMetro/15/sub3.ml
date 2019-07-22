(*
    PL 2-3
    2008-11609 박성원
*)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro metro =
  let rec findInList l v =
    match l with
    | [] -> false
    | h :: l -> (h = v) || (findInList l v)
  in
  let rec checkImpl metro areaNames =
    match metro with
    | STATION name -> findInList areaNames name
    | AREA (name, metro) -> checkImpl metro (name :: areaNames)
    | CONNECT (metro1, metro2) -> (checkImpl metro1 areaNames) && (checkImpl metro2 areaNames)
  in
  checkImpl metro []

type metro = STATION of name
| AREA of name*metro
| CONNECT of metro*metro
and name = string

let rec check metro list =
  match metro with
    | STATION(name) -> List.exists (fun x -> x=name) list
    | CONNECT(a,b) -> (check a list)&&(check b list)
    | AREA(n,m) -> check m (n::list)

let checkMetro metro =
  check metro []

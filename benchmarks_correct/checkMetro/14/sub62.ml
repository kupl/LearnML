type metro = STATION of name
| AREA of name*metro
| CONNECT of metro*metro
and name = string

let rec check metro lst =
  match metro with
    | STATION(name) -> List.exists (fun x -> x=name) lst
    | CONNECT(a,b) -> (check a lst)&&(check b lst)
    | AREA(n,m) -> check m (n::lst)

let checkMetro metro =
  check metro []

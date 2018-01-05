type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string


let checkMetro (m:metro):bool =
  let rec withList ((l:string list), (mm:metro)):bool =
    match mm with
    |STATION(name) -> List.mem name l
    |AREA(id, m1) -> withList(id::l,m1)
    |CONNECT(m1, m2)-> withList(l, m1) && withList(l, m2)
  in
  withList([], m)

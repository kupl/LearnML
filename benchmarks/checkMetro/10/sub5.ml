(* 컴퓨터공학부 / 2005-11721 / 김재경 / 숙제1-7 *)
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name=string
let rec check(list,metro) =
    match metro with
      STATION(name) -> List.mem name list
    | AREA(name,m1) -> check(name::list,m1)
    | CONNECT(m1,m2) -> check(list,m1) & check(list,m2)
let checkMetro metro = check([],metro)
(* ��ǻ�Ͱ��к� / 2005-11721 / ����� / ����1-7 *)
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name=string
let rec check(lst,metro) =
    match metro with
      STATION(name) -> List.mem name lst
    | AREA(name,m1) -> check(name::lst,m1)
    | CONNECT(m1,m2) -> check(lst,m1) && check(lst,m2)
let checkMetro metro = check([],metro)

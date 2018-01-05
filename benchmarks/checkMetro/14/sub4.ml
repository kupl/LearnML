module S = Set.Make(String)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec check m s = 
        match m with
          STATION(id) -> if S.mem id s then true
                         else false               
        | AREA(id, m1) -> check m1 (S.add id s)
        | CONNECT(m1, m2) -> (check m1 s) && (check m2 s)        

let checkMetro m = check m S.empty        

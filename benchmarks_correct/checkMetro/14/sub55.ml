type metro = STATION of name
            |AREA of name * metro
            |CONNECT of metro * metro
and name = string

let checkMetro arg1 = 
    let rec check arg2 my_list =
        match arg2 with
            |STATION n -> List.mem n my_list
            |AREA (a,b) -> check b (List.append my_list (a::[]))
            |CONNECT (c,d) -> (check c my_list) && (check d my_list)
    in check arg1 []

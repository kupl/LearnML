type metro=STATION of name|AREA of name * metro|CONNECT of metro * metro
and name=string
let rec check(m,id_list)=(match m with
                |STATION(id)->(List.mem id id_list)
                |AREA(id,m1)->if (List.mem id id_list) then check(m1,id_list)
                else check(m1,id::id_list)
                |CONNECT(m1,m2)->check(m1,id_list)&check(m2,id_list)
                )
let rec checkMetro mtr=check(mtr,[])

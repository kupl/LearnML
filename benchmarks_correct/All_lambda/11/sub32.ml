type lambda=V of var|P of var * lambda|C of lambda * lambda
and var=string
let rec check(m,id_list)=(match m with
                |V(id)->(List.mem id id_list)
                |P(id,m1)->if (List.mem id id_list) then check(m1,id_list)
                else check(m1,id::id_list)
                |C(m1,m2)->check(m1,id_list)&check(m2,id_list)
                )
let rec check mtr=check(mtr,[])

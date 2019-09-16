type lambda=V of var|P of var * lambda|C of lambda * lambda
and var=string
(*
 * manually edited
 * checkMetro -> check 
 * check -> check_sub
 *)
let rec check_sub(m,id_list)=(match m with
                |V(id)->(List.mem id id_list)
                |P(id,m1)->if (List.mem id id_list) then check_sub(m1,id_list)
                else check_sub(m1,id::id_list)
                |C(m1,m2)->check_sub(m1,id_list)&check_sub(m2,id_list)
                )
let rec check mtr=check_sub(mtr,[])

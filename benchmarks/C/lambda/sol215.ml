type lambda = V of var
            |P of var * lambda
            |C of lambda * lambda
and var = string

let check arg1 = 
    let rec check2 arg2 my_list =
        match arg2 with
            |V n -> List.mem n my_list
            |P (a,b) -> check2 b (List.append my_list (a::[]))
            |C (c,d) -> (check2 c my_list) && (check2 d my_list)
    in check2 arg1 []

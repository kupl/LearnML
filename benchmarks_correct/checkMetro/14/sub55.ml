type lambda = V of var
            |P of var * lambda
            |C of lambda * lambda
and var = string

let check arg1 = 
    let rec check arg2 my_list =
        match arg2 with
            |V n -> List.mem n my_list
            |P (a,b) -> check b (List.append my_list (a::[]))
            |C (c,d) -> (check c my_list) && (check d my_list)
    in check arg1 []

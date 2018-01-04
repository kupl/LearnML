exception InvalidArgument

type ae = CONST of int
                | VAR of string
                | POWER of string*int
                | TIMES of ae list
                | SUM of ae list



let rec diffSub ae str =
    let rec diff_times lst str =
        match (List.length lst) with
        | 1 -> [diffSub (List.hd lst) str]
		| _-> [TIMES ([(diffSub (List.hd lst) str)]@(List.tl lst))] @ [TIMES ([(List.hd lst)]@[(diffSub (TIMES (List.tl lst)) str)])]
            
                in let rec diff_sum lst str =
	                    match lst with
	                    | hd::tl -> List.append [(diffSub hd str)] (diff_sum tl str)
	                    | [] -> []
                in match ae with
                | CONST _ -> (CONST 0)
                | VAR s -> if (str=s) then (CONST 1)
                           else (CONST 0)
                | POWER (s,i) -> if (str=s) then TIMES([(CONST i);POWER(s,i-1)])
                                 else (CONST 0)
                | TIMES [] -> raise InvalidArgument
                | SUM [] -> raise InvalidArgument
                | TIMES l-> SUM (diff_times l str)
                | SUM l -> SUM (diff_sum l str)

let diff (ae,str) =
    (diffSub ae str)


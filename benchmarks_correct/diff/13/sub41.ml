exception InvalidArgument

type aexp = Const of int
                | Var of string
                | Power of string*int
                | Times of aexp list
                | Sum of aexp list



let rec diffSub aexp str =
    let rec diff_times lst str =
        match (List.length lst) with
        | 1 -> [diffSub (List.hd lst) str]
		| _-> [Times ([(diffSub (List.hd lst) str)]@(List.tl lst))] @ [Times ([(List.hd lst)]@[(diffSub (Times (List.tl lst)) str)])]
            
                in let rec diff_sum lst str =
	                    match lst with
	                    | hd::tl -> List.append [(diffSub hd str)] (diff_sum tl str)
	                    | [] -> []
                in match aexp with
                | Const _ -> (Const 0)
                | Var s -> if (str=s) then (Const 1)
                           else (Const 0)
                | Power (s,i) -> if (str=s) then Times([(Const i);Power(s,i-1)])
                                 else (Const 0)
                | Times [] -> raise InvalidArgument
                | Sum [] -> raise InvalidArgument
                | Times l-> Sum (diff_times l str)
                | Sum l -> Sum (diff_sum l str)

let diff (aexp,str) =
    (diffSub aexp str)


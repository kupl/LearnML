
(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
    let rec sub_bmul: bin -> bin -> bin -> bin
    = fun sb1 sb2 sum ->
        match sb1 with
        | [] -> sum
        | ZERO :: tl -> sub_bmul tl sb2 (sum @ [ZERO]) 
        | ONE :: tl -> 
                let rec badd: bin -> bin -> bin
                = fun bb1 bb2 ->
                    let rec brev: bin -> bin
                    = fun b ->
                        match b with
                        | [] -> []
                        | hd :: tl -> (brev tl) @ [hd]
                    in if bb1 = [] then bb2 
                       else if bb2 = [] then bb1
                            else match (brev bb1, brev bb2) with
                                 | (ZERO :: t1, ZERO :: t2) -> ((badd (brev t1) (brev t2)) @ [ZERO])
                                 | (ZERO :: t1, ONE :: t2) -> ((badd (brev t1) (brev t2)) @ [ONE])
                                 | (ONE :: t1, ZERO :: t2) -> ((badd (brev t1) (brev t2)) @ [ONE])
                                 | (ONE :: t1, ONE :: t2) -> ((badd (badd (brev t1) (brev t2)) [ONE]) @ [ZERO])
                                 | _ -> raise(Failure "Error")
                in (sub_bmul tl sb2 (badd (sum @ [ZERO]) sb2))
    in if b2 = [ZERO] then [ZERO]
       else match b1 with
            | [] -> []
            | [ZERO] -> [ZERO]
            | [ONE] -> b2
            | hd :: tl -> sub_bmul tl b2 b2;;

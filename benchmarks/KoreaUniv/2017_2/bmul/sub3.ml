(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> let trans_digit a = ( match a with
                                  | ZERO -> 0
                                  | ONE -> 1)
            in let rec length bin = ( match bin with
                                    | hd::tl -> 1+length tl
                                    | []-> 0)
            in let rec binary_exp num = if num=1 then 1 else 2*binary_exp (num-1)
            in let rec eval exp square = (match exp with
                                          | hd::tl -> (trans_digit hd)*square + eval tl (square/2)
                                          | []->0)
            in let rec modular num1 num2 = if num2*2 > num1 then num2 else modular num1 (num2*2)
            in let rec trans_bin num1 num2 = if num2=1 then if num1=1 then [ONE] else if num1=0 then [ZERO] else if modular num1 1 != num2 then [ZERO]@(trans_bin num1 (num2/2)) else [ONE]@(trans_bin (num1-num2) (num2/2))
           
            
          in let v1 = eval b1 binary_exp (length b1)and v2 = eval b2 binary_exp (length b2)
          in trans_bin (v1*v2) (modular (v1*v2) 1)

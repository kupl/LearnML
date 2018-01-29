(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> let eval_digit a =( match a with
                                | ZERO -> 0
                                | ONE -> 1)
              in let rec rev a = match a with
                                | hd::tl -> rev tl@[hd]
                                | [] -> []
              in let rec eval exp num = (match exp with
                                    | hd::tl -> (eval_digit hd)*num+eval tl (num*2)
                                    | [] -> 0)
              in let rec rep num = if num = 0 then [ZERO]
              
                                  else if num - (num/2)*2 = 1 then if num/2 = 0 then ONE::[] else ONE::rep (num/2)
  else if num/2 = 0 then ZERO::[] else ZERO::rep (num/2)
             in rev (rep ((eval (rev b1) 1) * (eval (rev b2) 1)))


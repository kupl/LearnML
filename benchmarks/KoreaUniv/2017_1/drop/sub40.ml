(* problem 6 *) 
let rec drop
= fun l n -> match l with
      | [] -> []
      | hd::tl -> if (n = 1) then tl else (drop tl (n-1)) 
(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> let rec func lst c = if c = 0 then 1
                                          else if c < 0 then 0 
                                           else match lst with
                                              [] -> 0
                                              |hd::tl -> (match tl with
                                                          [] -> func lst (c-hd)
                                                          | _ -> (func lst (c-hd)) + (func tl c))
  in func coins amount

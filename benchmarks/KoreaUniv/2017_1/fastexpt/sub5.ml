(* problem 1*)
let fastexpt : int -> int -> int
= fun b n -> let rec a b n = if n=0 then 1 
                                    else if (n mod 2 = 0) then a (b*b) (n/2)
                                                  else (a b (n-1)) * b
  in a b n
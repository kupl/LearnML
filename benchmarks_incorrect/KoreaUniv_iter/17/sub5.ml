
(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n, f) -> if n = 0 then f else
                let rec a n f x= if n>0 then f ( a (n-1) f x) 
                                        else x
               in a n f
(* problem 4 *)
let rec product 
= fun f a b -> match b with
          | _ -> if (b = a) then (f a) else (f b)*(product f a (b-1)) 

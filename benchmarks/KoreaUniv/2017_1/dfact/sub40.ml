(* problem 1 *)
let rec fastexpt
= fun b n ->
    match n with
    | 0 -> 1
    | _ -> if (n mod 2 = 1) then b*(fastexpt b (n-1)) else (fastexpt b (n/2))*(fastexpt b (n/2))

(* problem 4 *)
let rec product 
= fun f a b -> match b with
          | _ -> if (b = a) then (f a) else (f b)*(product f a (b-1)) 

(* problem 5 *)
let rec dfact
= fun n -> match n with
       | 1 -> 1
       | 2 -> 2
       | _ -> if (n mod 2 = 0) then (fastexpt 2 (n/2))*(product (fun x -> x) 2 (n/2))
              else (product (fun x-> x) 1 n)/(dfact (n-1))
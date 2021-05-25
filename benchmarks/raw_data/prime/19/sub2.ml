let rec isMod x n=
  match n with
    | 1-> true
    | _-> if x mod n = 0 then false else isMod x (n-1);;

let isPrime x =
  isMod x (x-1);;

let prime : int -> bool
= fun n -> 
  match n with
    | 1-> false
    | _-> isPrime n;;



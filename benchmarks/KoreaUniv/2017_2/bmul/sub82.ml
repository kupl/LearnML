(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec cnt : bin -> int
= fun l -> 
    match l with
    [] -> 0
    | hd::tl -> 1+ cnt tl
;;

let rec fastexpt : int -> int -> int
= fun b n -> if n = 0 then 1
else if (n mod 2 = 0) then (fastexpt b (n/2))*(fastexpt b (n/2))
else b*(fastexpt b (n-1))
;;

let rec biToDe bi =
    match bi with
    [] -> 0
    | hd::tl->
        match hd with
        | ZERO -> biToDe tl
        | ONE -> (fastexpt 2 ((cnt bi)-1)) + (biToDe tl)
;;

let rec deToBi de = 
    if de = 0 then []
    else  if ((de mod 2)=1) then (deToBi ((de-1)/2))@[ONE]
    else (deToBi (de/2))@[ZERO]
;;

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
    deToBi ((biToDe b1)*(biToDe b2))
;;
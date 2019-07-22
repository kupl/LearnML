(* 컴퓨터공학부 2013-11425 이창영 hw1_3 *)

let rec iter ((n : int), f) x =
    if n==0 then x
    else iter (n-1, f) (f x)

(* 컴퓨터공학부 2013-11425 이창영 hw1_2 *)

let rec sigma ((a : int), (b : int), (f : int->int)) : int =
    if a > b then 0
    else if a==b then f a
    else    f a + sigma(a+1, b, f)

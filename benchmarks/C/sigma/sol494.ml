(* 컴퓨터공학부 2013-11425 이창영 hw1_2 *)

let rec sigma f a b =
    if a > b then 0
    else if a==b then f a
    else    f a + sigma f (a+1) b

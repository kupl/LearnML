
(*
    컴퓨터공학부 2012-11270 장선웅
    hw 1 - Exercise 2
*)


let rec sigma f a b =
    if a>b then 0
    else if a=b then f(b)
    else sigma f (a+1) b + f(a)


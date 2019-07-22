
(*
    컴퓨터공학부 2012-11270 장선웅
    hw 1 - Exercise 2
*)


let rec sigma (a, b, f)  = 
    if a>b then 0
    else if a=b then f(b)
    else sigma(a+1,b,f) + f(a)


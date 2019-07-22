
(*
    컴퓨터 공학부 2012-11270 장선웅
    hw 1 - Exercise 3
*)


let rec iter(n,f) x = 
    if n <= 0 then x
    else iter(n-1,f)( f(x))




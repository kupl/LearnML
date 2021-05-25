(* 컴퓨터공학부 / 2005-11721 / 김재경 / 숙제1-1 *)
let rec sigma (a,b,f) =
    if a>b then 0
    else f(a) + sigma (a+1,b,f)
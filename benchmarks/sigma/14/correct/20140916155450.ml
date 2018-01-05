(*컴퓨터공학부 2010-11779 박진영 1.1*)
let sigma (a, b, f) = 

  let rec cal (a, b, f, rs) =
    if a > b then rs
    else (
     cal (a+1, b, f, rs+(f a))
    )
    in

    if (a > b) then 0
    else (
      cal(a, b, f, 0)
    )
  

  

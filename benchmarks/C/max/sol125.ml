let rec max lst = 
match lst with
| [] -> 0
| x::[] -> x
| x::xs ->
     let v = max xs in
       if x < v then
            v
       else
           x
 
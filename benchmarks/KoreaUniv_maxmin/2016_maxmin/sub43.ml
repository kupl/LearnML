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



let rec min lst =
match lst with
| [] -> 0
| x::[] -> x
| x::xs ->
     let v = min xs in
       if x < v then 
            x
       else
          v 

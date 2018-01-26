let rec prime n =
   let rec loop i =
   if n mod i = 0 && n>2 then false
   else if n = i+1 then true
   else if n = 2 then true
   else loop (i+1) 
    in loop 2

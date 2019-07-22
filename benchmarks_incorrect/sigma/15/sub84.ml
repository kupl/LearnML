let rec sigma : (int * int * (int -> int) -> int ) = fun (a, b, f) ->
match b with
| 0 -> 0
| _ -> 
   if b < a
   then 0
   else if b = a
   then f a
   else 
   f a + sigma (a+1, b, f)



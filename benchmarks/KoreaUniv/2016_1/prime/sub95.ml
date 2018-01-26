let rec calc x n =
if (x mod n) <> 0 then
 match n with 2 -> true | _-> calc x (n-1)
else false;;

let rec prime n = match n with 0 | 1->false | 2->true | _-> calc n (n-1)

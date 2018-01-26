let rec prime : int -> bool
= fun n -> let rec is_prime a = if (a = 1) then 1
else if (n mod a) = 0 then 0
else if ((is_prime (a-1)) = 1) then 1
else 0 in
if ((is_prime (n-1)) = 1 ) then true
else false
let rec prime : int -> bool
= fun n -> let rec div : int-> int =fun d ->if d<n&&n mod d=0 then 0 else if d<n&&n mod d>0 then div(d+1) else if d>n||d=n then 1 else 2 in if n=2 then true else if div(2)<>0 then true else false;;

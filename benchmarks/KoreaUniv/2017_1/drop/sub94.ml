let rec drop a b =
if b = 0 then a
else drop (List.tl a) (b-1);;


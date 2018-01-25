let rec dfact a =
if (a <= 2) then a
else a * dfact (a-2);;

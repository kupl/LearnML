let rec sigma f i_start i_end = 
 if (i_start = i_end)
 then (f i_start)
 else ((f i_start) + (sigma f (i_start+1) i_end));;

let rec sigma (i_start, i_end, f) = 
 if (i_start = i_end)
 then (f i_start)
 else ((f i_start) + (sigma ((i_start + 1), i_end, f)));;

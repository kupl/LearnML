let rec iter (n,f) num = if (n==0) then num else (iter (n-1 , f) (f num));;

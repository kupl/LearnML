let rec sigma func a n = if (n <= a) then func a else func n + sigma func a (n-1);;


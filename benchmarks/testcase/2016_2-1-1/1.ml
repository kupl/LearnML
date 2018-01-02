let rec f l = 
match l with 
|[] -> 0
|hd::tl-> if(hd>=f(tl)) then hd
else f(tl)

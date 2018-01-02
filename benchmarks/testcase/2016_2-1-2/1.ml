let rec f l =
match l with
|[] -> 10000
|hd::tl-> if(hd<=f(tl)) then hd
else f(tl)

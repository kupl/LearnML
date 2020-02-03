let rec max l = 
 match l with
 [] -> -1
 | [a] -> a
 | h::t -> if h>max t then h else max t;;
 
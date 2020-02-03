
let rec max : int list -> int =fun l -> match l with
  [] -> 0
  |h::t-> 
  if h>max t then h
  else max t
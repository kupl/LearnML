let __s5 (__s6 : int) (__s7 : int) : int = if __s6 > __s7 then __s6 else __s7

let rec max (l : int list) : int =
  match l with [] -> 0 | hd :: tl -> if tl = [] then hd else __s5 hd (max tl)

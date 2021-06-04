let __s5 (__s6 : int) (__s7 : int) : int = if __s6 > __s7 then __s6 else __s7

let rec max (l : int list) : int =
  match l with [] -> 0 | h :: t -> if t = [] then h else __s5 h (max t)

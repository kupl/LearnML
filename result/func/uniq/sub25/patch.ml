let rec __s5 __s6 (__s7 : 'a list) : 'a list =
  match __s7 with
  | [] -> []
  | __s8 :: __s9 ->
      if __s6 = __s8 then __s5 __s6 __s9 else __s8 :: __s5 __s6 __s9


let rec uniq (lst : 'c list) : 'b list =
  match lst with [] -> [] | hd :: tl -> hd :: uniq (__s5 hd tl)


and search s (l : 'c list) : 'b list =
  match l with [] -> [] | hd :: tl -> if s = hd then tl else hd :: search s tl


let (_ : int list) = uniq [ 5; 6; 5; 4 ]

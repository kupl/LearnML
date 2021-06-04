let rec __s1 (__s2 : 'a list) __s3 : 'a list =
  match __s2 with
  | [] -> []
  | __s8 :: __s9 ->
      if __s8 = __s3 then __s1 __s9 __s3 else __s8 :: __s1 __s9 __s3


let rec uniq (lst : 'c list) : 'b list =
  match lst with [] -> [] | hd :: tl -> hd :: uniq (__s1 tl hd)


and search s (l : 'c list) : 'b list =
  match l with [] -> [] | hd :: tl -> if s = hd then tl else hd :: search s tl


let (_ : int list) = uniq [ 5; 6; 5; 4 ]

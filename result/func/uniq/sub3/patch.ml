let rec checkDuplicate a (lst : 'a list) : bool =
  match lst with
  | [] -> false
  | hd :: tl -> if a = hd then true else checkDuplicate a tl


let rec __s1 (__s2 : 'b list) __s3 : 'b list =
  match __s2 with
  | [] -> __s2
  | __s8 :: __s9 ->
      if __s8 = __s3 then __s1 __s9 __s3 else __s8 :: __s1 __s9 __s3


let rec uniq (lst : 'd list) : 'c list =
  match lst with
  | [] -> []
  | hd :: tl ->
      let temp = hd in
      hd :: uniq (__s1 lst hd)

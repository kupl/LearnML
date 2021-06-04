let rec filter (f : 'a -> bool) (__fun__ : 'a list) : 'a list =
  match __fun__ with
  | [] -> []
  | h :: t -> if f h then h :: filter f t else filter f t


let rec __s3 (__s4 : 'b list) __s5 : 'b list =
  match __s4 with
  | [] -> []
  | __s10 :: __s11 ->
      if __s10 != __s5 then __s10 :: __s3 __s11 __s5 else __s3 __s11 __s5


let rec uniq (lst : 'd list) : 'c list =
  match lst with
  | [] -> []
  | __s7 :: __s8 :: __s9 ->
      if __s9 = lst then uniq (__s7 :: __s9)
      else __s7 :: __s3 (uniq (__s8 :: __s9)) __s7
  | h :: t ->
      if filter (fun x -> x != h) t = t then h :: uniq t
      else h :: filter (fun x -> x != h) t


let (_ : int list) = uniq [ 5; 6; 5; 4 ]

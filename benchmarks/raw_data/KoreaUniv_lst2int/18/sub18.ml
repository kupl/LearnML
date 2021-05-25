let lst2int : int list -> int
= fun lst -> let rec step lst = match lst
with [] -> 0
| he::ta -> he*(let rec len lst = match lst with [] ->1 | he ::ta -> 10* len(ta) in len ta) + step ta in step lst;;
  
lst2int [3;2;4;5];;
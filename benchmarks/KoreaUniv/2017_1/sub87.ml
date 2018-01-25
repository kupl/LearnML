let rec fastexpt b n =
  match n with
  |0 -> 1
  |_ -> match (n mod 2) with
    |0 -> (fastexpt b (n/2))*(fastexpt b (n/2))
    |1 -> (fastexpt b (n-1)) * b
let smallest_devisor n =
  let rec smde n b =
    if (n < b*b) then n
    else (if (n mod b == 0) then b
        else smde n (b+1))
    in smde n 2
let rec iter (n, f) =
  match n with
  |0 -> fun x-> x
  |_ -> fun x -> f (iter (n-1, f) x)
let rec product f a b =
  match (b - a) with
  |0 -> f b
  |_ -> (f a) * (product f (a+1) b)
let rec dfact n =
  match n with
  0 | 1 -> 1
  |_ -> n * dfact (n-2)
let rec drop l n =
  match n with
  |0 -> l
  |_ -> match l with
    |[] -> []
    |_::tl -> drop tl (n-1)
let rec unzip lst =
  let fst a = match a with (x,_) -> x in
  let snd a = match a with (_,x) -> x in
  match lst with
  |hd::tl -> ((fst hd)::(fst (unzip tl)), ((snd hd)::(snd (unzip tl))))
  |_ -> ([],[])
let rec change coins amount =
  if amount == 0 then 1
  else if amount < 0 then 0
    else match coins with
      |[]->0
      |hd::tl -> change coins (amount - hd) + change tl amount
      

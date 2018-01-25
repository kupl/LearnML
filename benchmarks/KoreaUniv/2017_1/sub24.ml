(* HW1 *)
  let rec fastexpt b n = 
  if n = 0 then 1
  else (if n mod 2 = 0 then (fastexpt b (n/2))*(fastexpt b (n/2)) else b*(fastexpt b (n-1)))

(* HW2 *)
  let rec  modi n b =
  if b*b>n then n
  else
  if n mod b = 0 then b else modi n (b+1)

  let  smallest_divisor n = 
    modi n 2

(* HW3 *)
  let rec iter n func i =
  if n > 1 then func (iter (n-1) func i)
  else func i

(* HW4 *)
  let rec product f a b = 
  if a=b then a
  else f a (product f (a+1) b)

(* HW5 *)
  let rec dfact a =
  if a > 2 then a * dfact (a-2)
  else a

(* HW6 *)
  let rec length l =
  match l with
  | [] -> 0
  | hd::tl -> 1 + length tl

  let rec drop l n =
  if n = 0 then l
  else match l with
  | [] -> []
  | hd::tl -> drop tl (n-1)

(* HW7 *)
  let fst p = match p with (x,_) -> x;;
  let snd p = match p with (_,x) -> x;;
  
  let rec hdapp l =
  match l with 
  | [] -> []
  | hd::tl -> (fst hd)::(hdapp tl)

  let rec tlapp l =
  match l with
  | [] -> []
  | hd::tl -> (snd hd)::(tlapp tl)

  let rec unzip l =
  match l with
  | [] -> []
  | hd::tl ->[hdapp l,tlapp l]
   
(* HW8 *)
  let rec change l n =
  if n=0 then 1
  else if n < 0 then 0
  else
  match l with 
  | [] -> 0
  | hd::tl -> (change l (n-hd)) + (change tl n)
  

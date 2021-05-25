let rec lst2int : int list -> int
= fun lst ->
  let rec length list =
    match list with
       [] -> 0
      |_::xs -> 1 + length xs in
    let rec powerOfTen n = if n == 0 then 1 else 10 * powerOfTen (n - 1) in
  match lst with
    [] -> 0
    |(x::xs) -> x * powerOfTen (length xs) + lst2int xs;;
    
lst2int [2;3;4;5];;
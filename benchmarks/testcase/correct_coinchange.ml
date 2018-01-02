{
  [1;5;10], 12 -> 4;
  [1;5;10;25;50], 100 -> 292;
  [2;5;3;6], 10 -> 5; 
}
let rec change : int list -> int -> int
= fun coins amount ->
  if (amount < 0 || coins = []) then 0
  else
    let rec sum lst =
    match lst with
    |[] -> 0
    |hd::tl -> (change lst (amount-hd)) + (sum tl)
  in
  sum coins ;;

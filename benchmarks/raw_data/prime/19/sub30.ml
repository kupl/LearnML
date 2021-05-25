exception NotPositive;;

let divi : int * int -> bool 
= fun (n, d) -> if d = 1 then false else if (n mod d) = 0 then true else false;;

let rec divisible: int * int -> bool
= fun (n, d) -> if d=0 then false else divi (n, d) || divisible (n, d-1);;

let prime : int -> bool
= fun n -> if n<=0 then raise NotPositive else if divisible (n, n-1) then false else true;;(*TODO*)






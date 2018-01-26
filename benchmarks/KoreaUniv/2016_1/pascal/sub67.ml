let rec pascal : int * int -> int
= fun (n1, n2) -> match n2 with
| 0 -> 1
| n2 -> if n1==n2 then 1 else pascal(n1-1,n2-1)+pascal(n1-1,n2)

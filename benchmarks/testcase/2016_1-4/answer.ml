{
  (fun x -> x+1), 2, 4 -> 12;
  (fun x -> 0), 1, 1000 -> 0;
  (fun x -> (x*x)+x), 3, 6 -> 104;
  (fun x -> x mod 3), 1, 10 -> 10;
}
let rec sigma func n1 n2 = if(n1>n2) then 0 else (func n1)+(sigma func (n1+1) n2);;

{
   1 -> 1;
   2 -> 2;
   3 -> 6;
   4 -> 24;
   5 -> 120;
}
let rec f n = if(n=0) then 1 else n * (f (n-1));; 
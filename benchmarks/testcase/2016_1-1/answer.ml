{
  0 -> 0;
  1 -> 1;
  2 -> 1;
  3 -> 2;
  7 -> 13;
}
let rec f n=
  if(n=0) then 0 else if (n=1) then 1 else (f (n-1))+(f (n-2));;

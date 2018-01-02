let rec testPrime i n =
if i * i > n then true
else if (n mod i) = 0 then false
else testPrime (i+1) n;;


let rec f n = testPrime 2 n;;

let rec pascal pair =
let first = match pair with (x,_) -> x in
let second = match pair with (_,y) -> y in
if second == 0 then 1
else if second == first then 1
else pascal (first-1, second-1) + pascal (first-1, second);;

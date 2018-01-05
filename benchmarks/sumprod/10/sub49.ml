exception Error of string;;

let rec prod m i j k = if j=k then (m (i, k))
else (m (i, j)) *. (prod m i (j+1) k);;

let rec sum m i n j k = if i=n then (prod m n j k)
else (prod m i j k) +. (sum m (i+1) n j k);;

let sumprod (m, n, k) = if n<0 then raise(Error "Input wrong n value")
else if k<0 then raise(Error "Input wrong k value")
else (sum m 1 n 1 k);;

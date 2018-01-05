
let rec sigma a b f =
  if a = b then f a
  else f a +. sigma (a+1) b f;;

let rec sumprod mtx n k =
  if n = 1
  then sigma 1 k (mtx 1)
  else sigma 1 k (mtx n) +. sumprod mtx (n-1) k



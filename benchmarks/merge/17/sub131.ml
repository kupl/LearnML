let mycmp x y = if (x == y) then 0 else if(x < y) then 1 else -1
let merge (x, y) = (List.merge (mycmp) (x) (y))

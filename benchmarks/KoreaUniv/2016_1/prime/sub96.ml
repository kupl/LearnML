let countz z = if z=2 then true else false;;
let rec divide x y z = if x<y then countz z
											 else if x mod y = 0 then divide x (y+1) (z+1)
											 else divide x (y+1) z;;
let prime x = divide x 1 0;;


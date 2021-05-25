let rec sigma f a b = 
if a < b then ((f b) + (sigma f a (b-1)))
else f b;;

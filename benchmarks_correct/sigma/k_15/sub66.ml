let rec sigma ff a b =
if a<b then ((ff b) + (sigma ff a (b-1)))
else ff b;;

let rec sigma (tmp : int -> int) a b =
if a > b then 0
else tmp a + sigma tmp (a + 1) b;;
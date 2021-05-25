let prime a =
if a > = 4 then if a mod 2 = 0 || a mod 3 = 0 then false else true else
  match a with
|0 -> false
|1-> false
|2->true
|3->true

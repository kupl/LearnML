let prime a =
if a == 1 then true
else
let i = ref 2 in
while a mod !i != 0 do
i := !i + 1
done;
if !i == a then true
else false;;


let rec zipperN l=

let rec zip1 l=
match l with
[]->[]|
h::t->
	match h with
	[]->zip1 t|
	hh::ht->(hh+0)::zip1 t
in
let rec zipO l=
match l with
[]->[]|
h::t->
	match h with
	[]->zipO t|
	hh::ht->ht::zipO t
in
if l=[] then
[]
else
(zip1 l)@zipperN (zipO l)
let rec zipper (a,b)=
match a,b with
[],r->r|
l,[]->l|
lh::lt,rh::rt->[lh+0;rh]@(zipper (lt,rt))
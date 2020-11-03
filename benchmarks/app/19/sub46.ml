let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *);;

delete_list = ['a','b']
l = [0,'a',1,2,'b','a',3,4]
l = [x for x in l if x not in delete_list]
print(l)
>>[0,1,2,3,4]
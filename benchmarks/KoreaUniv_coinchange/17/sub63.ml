(*p8*)

let change : int list->int->int
=fun coins amount-> let rec reverse coins1=
match coins1 wuth
|[]->[]
|h::t->(reverse t)@[h] in coins1=coins in coins=reverse coins1
if (*총합에서 잔돈중 최고 큰 것을 최대한으로 뺍니다.(빼는 잔돈의 양보다 작아질때 까지)->더이상 뺄 수 없다면 남은 총합에서 그 다음크기의 잔돈을 최대한으로 뺍니다.-> 이것을 반복합니다.-> 가장 작은 잔돈이 더이상 뺄 수 없을 때 남은 총합이 0이라면 카운팅하고 0이 아니라면 카운팅하지 않습니다.->*)
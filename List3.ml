(*1*)

let rec listIloczyn(list1) =
	if list1 = [] then 0
	else if List.tl list1 = [] then List.hd list1
	else List.hd list1 * listIloczyn(List.tl list1);;

listIloczyn([1;2;3;4]);;
listIloczyn([2;3;4;5]);;
listIloczyn([1]);;

(*3*)

let rec czyNieujemne(list1) = 
	if list1 = [] then true
	else (if List.hd list1 >= 0 then czyNieujemne(List.tl list1) else false);;

czyNieujemne([1;2;3;-4]);;
czyNieujemne([1;2;3;-5]);;
czyNieujemne([1;2;3;0]);;

(*2*)
let rec silnia(number) = 
	if(number < 0) then raise (Failure "zla liczba")
	else if number = 0 then 1
	else number * silnia(number-1);;

silnia(5);;
silnia(0);;
silnia(2);;
silnia(4);;


(*4*)
let rec lacznik(lista, znak, sep) = 
	if lista = [] then ""
	else if (List.tl lista = []) then List.hd lista ^ lacznik(List.tl lista, znak, sep)
	else (List.hd lista ^ sep ^ lacznik(List.tl lista, znak, sep));;
	
lacznik(["lista";"jest";"polaczona"], ".", ";");;
lacznik(["test";"proba"], "!", ".");;
lacznik(["proba";"test"], ".", ";");;
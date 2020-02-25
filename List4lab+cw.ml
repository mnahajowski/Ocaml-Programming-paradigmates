(*3*)

let rec replicate(text, count) = 
	if count <= 0 then []
	else text :: replicate(text, count-1);;

let rec replicate2(text, count) = 
	if count < 0 then raise (Failure "ujemny argument")
	else if count = 0 then []
	else text :: replicate(text, count-1);;

replicate2("la", -2);;
replicate("la", 3);;

(*2*)

let rec count(word, text) = 
	if text = [] then 0
	else (if List.hd text = word then 1 else 0) + count(word, List.tl text);;
	
count(1, [1;2;3;1]);;

(*6*)

let rec listLength(word) = 
	if word = [] then 0
	else 1 + listLength(List.tl word);;

listLength([1;2;3;4;5;2]);;

(*1*)

let rec flatten (list1) = 
	if list1 = [] then []
	else List.hd list1 @ flatten (List.tl list1);;

flatten([[5;6];[1;2;3]]);;

(*4*)

let rec sqrList(list1) = 
	if list1 = [] then []
	else List.hd list1 * List.hd list1 :: sqrList(List.tl list1);;
	
sqrList([1;2;3;4;5]);;


(*5*)

let rec palindrome(list1) = 
	List.rev list1 = list1;;
	
palindrome([1;2;1]);;

(*7*)

let rec recursive(number) = 
	if number = 1 then 1
	else recursive (int_of_float(log (float_of_int(number)))) + recursive (number/2);;

recursive(2);;


(*Magda1*)

let rec listIloczyn(list1) =
	if list1 = [] then 1
	else List.hd list1 * listIloczyn(List.tl list1);;

listIloczyn([1;2;3;4]);;

(*Magda3*)

let rec czyDodatnie(list1) = 
	if list1 = [] then true
	else (if List.hd list1 >= 0 then czyDodatnie(List.tl list1) else false);;

czyDodatnie([1;2;3;-4]);;

let rec silnia(number) = 
	if number = 0 then 1
	else number * silnia(number-1);;

silnia(5);;

let rec lacznik(lista, znak, sep) = 
	if lista = [] then znak
	else (List.hd lista ^ sep ^ lacznik(List.tl lista, znak, sep));;
	
	lacznik(["lista";"jest";"polaczona"], ".", ";");;

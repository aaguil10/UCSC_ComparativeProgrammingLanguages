#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/ocaml/bin/ocaml
open Printf

let car       = List.hd
let cdr       = List.tl

let x = 8;;
let y = 4;;

let mylist = [1;2;3;4;5] in
(*let () = List.iter (printf "%d\n") (cdr mylist)*)
 print_int (car mylist);;
print_string "\n";;

let ext_state x =
 car x;;

if x > y 
	then print_int (ext_state [3;5;7])
	else print_int y;;
print_string "\n";;
(*x;;
print_int x;;*)
print_string "\n";;

let rec contains f l = match l with
	| [] -> "false"
	| car1::cdr1 -> if (f car1)
					then "true"
					else (contains f cdr1);;




print_string (contains (x = 3) [1;2;3;4;5])
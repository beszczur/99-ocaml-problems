open Printf;;
open Array;;

(* ocamlc sudoku.ml; ./a.out *)

type field =
	{ value: int; possibilities: int list};;

let range a b =
	let rec f a b accum = 
		if a > b then 
			accum
		else 
			f a (b-1) (b::accum)
	in
		f a b [];;

let my_int_of_char c =
	match c with
		| '0' -> 0
		| '1' -> 1
		| '2' -> 2
		| '3' -> 3
		| '4' -> 4
		| '5' -> 5
		| '6' -> 6
		| '7' -> 7
		| '8' -> 8
		| '9' -> 9
		| _ -> failwith "int_of_char called with a non-digit character"

let print_list l = 
	let rec f l =
		match l with
			| [] -> ""
			| head::[] -> string_of_int head
			| head::tail -> string_of_int head^" | "^f tail
	in
		printf "%s" ( "[ "^(f l)^" ]");;
		
let rec remove_value value l =
	match l with
	| [] -> []
	| head :: tail -> if head = value then tail else head :: remove_value value tail;;

let print_board board =
	for i = 0 to 8 do
		printf "|";
		for j = 0 to 8 do
			if board.(i).(j).value == 0 then
			printf "   |"
(* 			print_list board.(i).(j).possibilities *)
			(*printf "%s" (" "^(print_list board.(i).(j).possibilities)^" |")*)
			else 
			printf " %s |" (string_of_int (board.(i).(j).value)); 
		done;
		printf "\n"
	done;; 

let read_cell board n c =
	match c with
		| '_' 	 -> ()
		| '0' .. '9' -> board.(n/9).(n mod 9) <- { board.(n/9).(n mod 9) with
		value = my_int_of_char c }
		| _ 	 -> ()

let of_file fname board =
	let scanbuf = Scanf.Scanning.from_file fname in
	for n = 0 to 80 do
		Scanf.bscanf scanbuf "%c " (read_cell board n)
	done;;

let isValueAllowedInRow board x y value =
	for i = 0 to 8 do
		if board.(x).(i).value == value then
			printf "false\n"
	done;;

let isValueAllowedInColumn board x y value =
	for i = 0 to 8 do
		if board.(i).(y).value == value then
			printf "false\n"
	done;;

let isValueAllowedInSquare board x y value =
	for i = 3*(x/3) to 3*(x/3)+2 do
		for j = 3*(y/3) to 3*(y/3)+2 do
			if board.(i).(j).value == value then
				printf "false\n"
		done;
	done;;

let initial_fileld = { value = 0; possibilities = range 1 9 };;
let board = make_matrix 9 9 initial_fileld;;
let example = range 1 9;;

of_file "sudoku1.sd" board;;

print_list example;;
printf "\n";;
let example2 = remove_value 2 example;;
print_list example2;;
printf "\n";;

printf "ROW: ";;
isValueAllowedInRow board 0 2 8;;
printf "COLUMN: ";;
isValueAllowedInColumn board 0 2 8;;
printf "SQUARE: ";;
isValueAllowedInSquare board 0 2 8;;

(* :( *)

board.(0).(2).possibilities = remove_value 8 board.(0).(2).possibilities;;
print_list board.(0).(2).possibilities;;
printf "\n";;

print_board board;;

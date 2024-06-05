type word = Int of int<32> | Triplet of int<32>

type rib = word * word * word

(* allocate 4096 bytes for the VM's memory *)

let size_ram = 4096;;
let static ram = 0^4096;;

(* use one cell array as reference *)
let stack = 4096^1 (* stack pointer starts at the end *)
let stack_limit = size_ram / 2 - 1
let pc = 0^1

(* #f constant ? symbol table ? *)

let main i =
	print_string "HELLO";
	print_newline ();;

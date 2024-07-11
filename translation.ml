let code = Array.make 22 0
let load_code () =
  code.(0) <- 42;
  code.(1) <- 59;
  code.(2) <- 39;
  code.(3) <- 108;
  code.(4) <- 33;
  code.(5) <- 39;
  code.(6) <- 39;
  code.(7) <- 107;
  code.(8) <- 40;
  code.(9) <- 108;
  code.(10) <- 126;
  code.(11) <- 64;
  code.(12) <- 107;
  code.(13) <- 94;
  code.(14) <- 122;
  code.(15) <- 33;
  code.(16) <- 40;
  code.(17) <- 58;
  code.(18) <- 108;
  code.(19) <- 107;
  code.(20) <- 108;
  code.(21) <- 121;;

let print_endline (s : string) =
  print_string s;
  print_newline ();;

(*let failwith (s : string) =
  print_string "Fatal error : ";
  print_endline s;
  let rec forever () = forever () in
  forever ();;*)

type word = Triplet of int | Int of int;;
type rib = word * word * word

let pair_type = Int 0;;
let procedure_type = Int 1;;
let symbol_type = Int 2;;
let string_type = Int 3;;
let vector_type = Int 4;;
let singleton_type = Int 5;;

let _nil = (Int 0, Int 9, singleton_type);;
let _false = (Int 0, Int 9, singleton_type);;
let _true = (Int 0, Int 9, singleton_type);;

let size_ram = 50000;;
let fh_start = 4;;
let fh_end = size_ram / 2;;
let sh_start = fh_end;;
let sh_end = size_ram;;
let limit = Array.make 1 0;;

let ram = Array.make size_ram (Int 10, Int 10, Int 10);;
let stack = Array.make 1 0;;
let heap = Array.make 1 0;;
let symtbl = Array.make 1 0;;
let pc = Array.make 1 0;;
let pos = Array.make 1 0;;
let brk = Array.make 1 0;;

let nil_rib = Triplet 0;;
let true_rib = Triplet 1;;
let false_rib = Triplet 2;;

type state = Rib of rib | Word of word;;

(*let rec show_state_cps
  ((prof, s, k) : int * state * (unit -> unit))
  : unit =
  match s with
  Rib (car, cdr, ty) ->
    print_string "(";
	let _ = show_state_cps (prof, Word car, (fun () ->
	print_string ", ";
	let _ = show_state_cps (prof, Word cdr, (fun () ->
	print_string ", ";
	let _ = show_state_cps (prof, Word ty, (fun () ->
	print_string ")";
	k (); ()
	)) in ())) in ())) in ()
  | Word w ->
    match w with
	Triplet i ->
	  if prof = 0
	  then print_string "X"
	  else show_state_cps (prof-1, Rib (ram.(i)), k)
	| Int i -> print_int i; k ()
	
  ;;*)

let int_of_triplet (w : word) : int =
  match w with
  Triplet i -> i
  | Int _ -> failwith "can't int_of_triplet"
  ;;

let int_of_Int (w : word) : int =
  match w with
  Int i -> i
  | Triplet _ -> failwith "can't int_of_Int"
  ;;

let get_rib (w : word) : rib =
  match w with
  Triplet i -> ram.(i)
  | Int _ -> failwith "can't get_rib"
  ;;

let is_rib (w : word) : bool =
  match w with
  Triplet i -> i >= 0 && i < size_ram
  | Int _ -> false
  ;;

let equal_word ((w1, w2) : word * word) : bool =
  match w1 with
  Triplet i ->
    (match w2 with
	Triplet j -> i = j
	| Int _ -> false
	)
  | Int i ->
    (match w2 with
	Int j -> i = j
	| Triplet _ -> false
	)
  ;;

let equal_rib ((r1, r2) : rib * rib) : bool =
  let (a1, a2, a3) = r1 in
  let (b1, b2, b3) = r2 in
  equal_word(a1, b1) && equal_word(a2, b2) && equal_word(a3, b3);;

let is_eqv ((w1, w2) : word * word) : bool =
  match w1 with
  Triplet i ->
    (match w2 with
	Triplet j -> equal_rib(get_rib w1, get_rib w2)
	| _ -> false
	)
  | Int i ->
    (match w2 with
	Int j -> i = j
	| _ -> false
	)
  ;;

let is_false (w : word) : bool = equal_word(w, false_rib);;

let field0_rib ((x, y, z) : rib) = x;;
let field1_rib ((x, y, z) : rib) = y;;
let field2_rib ((x, y, z) : rib) = z;;

let field0_word (w : word) : word = field0_rib (get_rib w);;
let field1_word (w : word) : word = field1_rib (get_rib w);;
let field2_word (w : word) : word = field2_rib (get_rib w);;

let field0_set ((w, v) : word * word) =
  match w with
  Triplet i ->
    ram.(i) <- (v, field1_word w, field2_word w)
  | Int _ -> failwith "can't field0_set"
  ;;

let field1_set ((w, v) : word * word) =
  match w with
  Triplet i ->
    ram.(i) <- (field0_word w, v, field2_word w)
  | Int _ -> failwith "can't field1_set"
  ;;

let field2_set ((w, v) : word * word) =
  match w with
  Triplet i ->
    ram.(i) <- (field0_word w, field1_word w, v)
  | Int _ -> failwith "can't field2_set"
  ;;

let is_pair (w : word) =
  is_rib w && equal_word(field2_word w, pair_type);;

let cons (car, cdr) = (car, cdr, pair_type);;
let get_car (w : word) : word = field0_word w;;
let get_cdr (w : word) : word = field1_word w;;

let not_enough_space () = limit.(0) - brk.(0) < 50;;

(* garbage collector *)
let rec move (w, next, s, e, ns, ne) =
  match w with
  Int _ -> (w, next)
  | Triplet i ->
    if i >= s && i < e
	then
	  (let replace () =
	    ram.(next) <- ram.(i);
		field0_set (w, Triplet next);
		(Triplet next, next+1)
	  in match get_car w with
	  Triplet j ->
	    if j >= ns && j < ne
		then (w, next)
		else replace ()
	  | Int _ -> replace ()
	  )
	  else (w, next)
	;;

let same_half (i,j) =
  (i >= fh_start && i < fh_end && j >= fh_start && j < fh_end)
      ||
  (i >= sh_start && i < sh_end && j >= sh_start && j < sh_end);;

(*let collect () =
  print_endline "collect";
  let (s, e, ns, ne) =
    if same_half (brk.(0), fh_start)
	then (fh_start, fh_end, sh_start, sh_end)
	else (sh_start, sh_end, fh_start, fh_end)
  in limit.(0) <- ne;
  let next = ns in
  let (new_stack, next) = move (Triplet(stack.(0)),next,s,e,ns,ne) in
  let (new_symtbl, next) = move (Triplet(symtbl.(0)),next,s,e,ns,ne) in
  let (new_pc, next) = move (Triplet(pc.(0)),next,s,e,ns,ne) in
  stack.(0) <- int_of_triplet new_stack;
  symtbl.(0) <- int_of_triplet new_symtbl;
  pc.(0) <- int_of_triplet new_pc;
  let rec loop (scan, next) =
    if next >= ne then failwith "memory is full";
	if scan < next
	then
	  (let (f0, f1, f2) = ram.(scan) in
	  let (new_f0, next) = move (f0,next,s,e,ns,ne) in
	  let (new_f1, next) = move (f1,next,s,e,ns,ne) in
	  let (new_f2, next) = move (f2,next,s,e,ns,ne) in
	  ram.(scan) <- (new_f0, new_f1, new_f2);
	  loop (scan+1, next))
	else scan
  in brk.(0) <- loop(ns, next);
  if not_enough_space () then failwith "not enough memory";;*)
let collect () = print_endline "GC not implemented. Not enough memory. exiting."; ();;


let get_next_i () : int =
  if brk.(0) = limit.(0) - 1
  then (collect (); brk.(0))
  else
	(brk.(0) <- brk.(0) + 1;
	brk.(0));;

let push (w : word) : unit =
  let i = get_next_i () in
  let new_tos = cons (w, Triplet (stack.(0))) in
  stack.(0) <- i;
  ram.(stack.(0)) <- new_tos;;

let pop () : word =
  let (car, cdr, _) = ram.(stack.(0)) in
  stack.(0) <- int_of_triplet cdr;
  car;;

let alloc_rib (r : rib) : word =
  let i = get_next_i () in
  heap.(0) <- i;
  ram.(heap.(0)) <- r;
  Triplet (heap.(0));;

let tos () = Triplet (stack.(0));;

let rec list_tail ((w, i) : word * int) : word =
  if 0 < i
  then list_tail (get_cdr w, i-1)
  else w;;

let get_byte () =
  (*let c = vect_nth (inp, pos.(0)) in
  pos.(0) <- pos.(0) + 1;
  c;;*)
  let c = code.(pos.(0)) in
  pos.(0) <- pos.(0) + 1;
  c;;

let get_code () =
  let x = (get_byte ()) - 35 in
  if x < 0 then 57 else x;;

let rec get_int n =
  let x = get_code () in
  let y = n * 46 in
  if x < 46
  then x + y
  else get_int (y + x - 46);;

let top_symtbl () = Triplet (symtbl.(0));;

let len (w : word) : int =
  let rec len_aux (w, acc) =
    if is_pair w
	then len_aux (get_cdr w, acc+1)
	else acc
  in len_aux (w, 0);;

let string_of_list (w : word) : rib =
  (w, Int (len w), string_type);;

let end_of_string (w : word) : rib =
  (false_rib, w, symbol_type);;

let alloc_sym (sym : rib) =
  let i = get_next_i () in
  symtbl.(0) <- i;
  ram.(symtbl.(0)) <- sym;
  Triplet (symtbl.(0));;

let add_symbol (chars : word) =
  let top = top_symtbl () in
  let str_rib = alloc_sym (string_of_list chars) in
  let end_rib = alloc_sym (end_of_string str_rib) in
  alloc_sym (cons (end_rib, top));;

let build_symtbl () =
  let rec loop1 n =
    if 0 < n
	then
	  (let _ = add_symbol nil_rib in
	  loop1 (n-1))
	else
	  (let rec loop2 chars =
	    let x = get_byte () in
		if x = 44
		then let _ = add_symbol chars in loop2 nil_rib
		else
		  if x = 59
		  then let _ = add_symbol chars in ()
		  else
			(let c = cons (Int x, chars) in
			loop2 (alloc_rib c))
		in loop2 nil_rib)
  in loop1 (get_int 0);;

let decode () =
  let _ = build_symtbl () in
  let codes = [|20;30;0;10;11;4|] in
  let sym n = get_car (list_tail (top_symtbl (), n)) in
  let add_instruction (op, opnd) =
    let top = tos () in
	let new_instr = (op, opnd, get_car top) in
	let new_rib = alloc_rib new_instr in
	field0_set (top, new_rib)
  in
  let rec decode_loop () =
    let x = get_code () in
	(let rec loop (op, n, x) =
	  let d = codes.(op) in
	  if d +2 < n
	  then loop (op+1, n-(d+3), x)
	  else
		(if 90 < x
		then
		  (let opnd = pop () in
		  add_instruction (Int 4, opnd);
		  decode_loop ())
		else
		  (if op = 0 then push (Int op);
		  let opnd =
		    if n < d
			then
			  (if op < 3 then sym n else (Int n))
			else if n = d
			then Int (get_int 0)
			else sym (get_int (n-d-1))
		  in
		  if 4 < op
		  then
		    (let ty = pop () in
			let code_proc = (opnd, Int 0, ty) in
			let code_proc_rib = alloc_rib code_proc in
			let proc = (code_proc_rib, nil_rib, procedure_type) in
			let proc_rib = alloc_rib proc in
			if is_rib (tos ())
			then
			  (add_instruction (Int 3, proc_rib);
			  decode_loop ()))
		  else
			(let op = if 0 < op then op-1 else 0 in
			add_instruction (Int op, opnd);
			decode_loop ())))
	in loop (0, x, x))
  in decode_loop ();;
		
let get_cont () =
  let rec loop w =
    print_string "x";
    if is_rib (field2_word w)
	then (print_newline ();w)
	else loop (get_cdr w)
  in loop (tos ());;

let get_var (opnd : word) =
  match opnd with
  Triplet _ -> field0_word opnd
  | Int i -> field0_word (list_tail (tos (), i))
  ;;

let set_var (opnd, v) =
  match opnd with
  Triplet _ -> field0_set (opnd, v)
  | Int i -> field0_set (list_tail (tos (), i), v)
  ;;

let prim0 f = push (f ());;
let prim1 f = push (f (pop ()));;
let prim2 f =
  let x = pop () in
  let y = pop () in
  push (f (x, y));;

let prim3 f =
  let x = pop () in
  let y = pop () in
  let z = pop () in
  let r = alloc_rib (f (x, y, z)) in
  push r;;

let to_bool x = if x then true_rib else false_rib;;
let getchar () = failwith "get_char is not available";;
let putchar c = print_int c; c;;

let prim2_int f =
  let f_rib = fun (y, x) ->
    match y with
	Int b ->
	  (match x with
	  Int a -> Int (f (a, b))
	  | _ -> failwith "not integer"
	  )
	| Triplet _ -> failwith "not integer"
	
  in prim2 f_rib;;

let close x = alloc_rib (get_car x, tos (), procedure_type);;

let call_primitive i =
  match i with
  0 -> prim3 (fun (z, y, x) -> (x, y, z))
  | 1 -> prim1 (fun x -> x)
  | 2 -> let _ = pop () in ()
  | 3 -> prim2 (fun (y, x) -> y)
  | 4 -> prim1 close
  | 5 -> prim1 (fun x -> to_bool (is_rib x))
  | 6 -> prim1 field0_word
  | 7 -> prim1 field1_word
  | 8 -> prim1 field2_word
  | 9 -> prim2 (fun (y, x) -> field0_set (x, y); y)
  | 10 -> prim2 (fun (y, x) -> field1_set (x, y); y)
  | 11 -> prim2 (fun (y, x) -> field2_set (x, y); y)
  | 12 -> prim2 (fun (y, x) -> to_bool (is_eqv (x, y)))
  | 13 -> prim2 (fun (y, x) -> match y with
			Int b ->
			  (match x with
			  Int a -> to_bool (a < b)
			  | _ -> failwith "not integer"
			  )
			| _ -> failwith "not integer"
			)
  | 14 -> prim2_int (fun (x, y) -> x + y )
  | 15 -> prim2_int (fun (x, y) -> x - y)
  | 16 -> prim2_int (fun (x, y) -> x * y)
  | 17 -> prim2_int (fun (x, y) -> x / y)
  | 18 -> prim0 (fun () -> Int (getchar ()))
  | 19 -> prim1 (fun x -> match x with
  			Int c -> Int (putchar c)
			| _ -> failwith "can't putchar_prim"
			)
  | _ -> failwith "invalid primitive identifier"
  ;;

let next_pc () =
  pc.(0) <- int_of_triplet (field2_word (Triplet (pc.(0))));;

let rec run () =
  let (instr, opnd, next) = get_rib (Triplet (pc.(0))) in
  match instr with
  Int i ->
  (match i with
  0 -> (* jump/call *)
    print_endline "jump/call";
    if not_enough_space () then collect ();
	let (_, opnd, next) = get_rib (Triplet (pc.(0))) in
	let proc = get_var opnd in
	let code = get_car proc in
	(match code with
	Int i -> (* calling primitive *)
	  print_endline "calling primitive";
	  call_primitive i;
	  if is_rib next
	  then next_pc ()
	  else
		(let cont = get_cont () in
		field1_set (tos (), field0_word cont);
		pc.(0) <- int_of_triplet (field2_word cont));
	  run ()
	| Triplet i -> (* calling lambda *)
	  print_endline "calling lambda";
	  if is_rib next
	  then print_endline "jump"
	  else print_endline "call";
	  let c2 = (Int 0, proc, pair_type) in
	  let c2_rib = alloc_rib c2 in
	  let nargs = int_of_Int (get_car code) in
	  let rec loop (n, s) =
	    if n = 0
		then s
		else
		  (let new_s = (pop (), s, pair_type) in
		  loop (n-1, alloc_rib new_s))
		in let s = loop (nargs, c2_rib) in
		if is_rib next
		then
		  (field0_set (c2_rib, tos ());
		  field2_set (c2_rib, next))
		else
		  (let k = get_cont () in
		  field0_set (c2_rib, field0_word k);
		  field2_set (c2_rib, field2_word k));
		stack.(0) <- int_of_triplet s;
		pc.(0) <- int_of_triplet (field2_word code);
		run ()
	)
  | 1 -> (* set *)
    print_endline "set";
    set_var (opnd, pop ());
	next_pc ();
	run ()
  | 2 -> (* get *)
    print_endline "get";
    let v = get_var opnd in
	push v;
	next_pc ();
	run ()
  | 3 -> (* const *)
    print_endline "const";
    push opnd;
	next_pc ();
	run ()
  | 4 -> (* if *)
    print_endline "if";
    if is_false (pop ())
	then next_pc ()
	else pc.(0) <- int_of_triplet opnd;
	run ()
  | 5 ->
    print_newline ();
    print_endline "HALT!"
  | _ -> failwith "not implemented yet"
  )
  | _ -> failwith "not implemented yet"
  ;;

let set_global v =
  field0_set (get_car (top_symtbl ()), v);
  symtbl.(0) <- int_of_triplet (get_cdr (top_symtbl ()));;

let start_vm () : unit =
  print_endline "RVM";
  load_code ();
  (* init constants *)
  stack.(0) <- -1;
  heap.(0) <- -1;
  symtbl.(0) <- -1;
  pos.(0) <- 0;
  brk.(0) <- fh_start;
  (* SET IT BACK !!!*)
  limit.(0) <- size_ram;
  decode ();
  let start_rib = field2_word (field0_word (Triplet (heap.(0)))) in
  pc.(0) <- int_of_triplet start_rib;
  let main = (Int 0, top_symtbl (), procedure_type) in
  let main_rib = alloc_rib main in
  set_global main_rib;
  set_global false_rib;
  set_global true_rib;
  set_global nil_rib;
  let halt = (Int 5, Int 0, pair_type) in
  let i = get_next_i () in
  let j = get_next_i () in
  ram.(i) <- halt;
  ram.(j) <- (Int 0, Int 0, Triplet i);
  stack.(0) <- j;
  run ();;

let _ =
  let rec forever () = forever () in
    start_vm ();
    forever ();;

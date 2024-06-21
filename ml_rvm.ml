let input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"

type word =
  Triplet of int
  | Int of int
  | Nil

type rib = word * word * word

let pair_type = Int 0
let procedure_type = Int 1
let symbol_type = Int 2
let string_type = Int 3
let vector_type = Int 4
let singleton_type = Int 5

let _nil = (Int 0, Int 0, singleton_type)
let _false = (Int 0, Int 0, singleton_type)
let _true = (Int 0, Int 0, singleton_type)

let size_ram = 90000
let ram = Array.make size_ram _nil
let symtbl_mem = Array.make 200 _nil
let stack = ref size_ram
let heap = ref (-1)
let pc = ref 0
let np = ref (-1)
let pos = ref 0

let rec show_rib (car, cdr, ty) =
  print_string "(";
  show_word car;
  print_string ", ";
  show_word cdr;
  print_string ", ";
  show_word ty;
  print_string ")"

and show_word = function
  Triplet i ->
    show_rib ram.(i)
  | Int i -> print_int i
  | Nil -> print_string "nil"
  
(* init constants *)
let nil_rib = Triplet 0
let true_rib = Triplet 2
let false_rib = Triplet 3

let int_of_triplet (w : word) : int =
  match w with
  Triplet i -> i
  | Int _ | Nil -> failwith "can't int_of_triplet"

let int_of_Int (w : word) : int =
  match w with
  Int i -> i
  | Triplet _ | Nil -> failwith "can't int_of_Int"

let get_rib (w : word) =
  match w with
  Triplet i -> ram.(i)
  | Int _ | Nil -> failwith "can't get_rib"

let is_rib (w : word) : bool =
  match w with
  Triplet i -> i >= 0 && i < size_ram
  | Int _ | Nil -> false

let is_eqv (w1 : word) (w2 : word) : bool =
  match w1, w2 with
  Int i, Int j -> i = j
  | Triplet i, Triplet j -> (get_rib w1) = (get_rib w2)
  | _ -> false

let is_false (w : word) = is_eqv w false_rib

let make_rib x y z = (x, y, z)

let field0_rib (x, y, z) = x
let field1_rib (x, y, z) = y
let field2_rib (x, y, z) = z

let field0_word (w : word) = get_rib w |> field0_rib
let field1_word (w : word) = get_rib w |> field1_rib
let field2_word (w : word) = get_rib w |> field2_rib

let field0_set (w : word) (v : word) =
  match w with
  Triplet i ->
    ram.(i) <- (v, field1_word w, field2_word w)
  | Int _ | Nil -> failwith "can't field0_set"

let field1_set (w : word) (v : word) =
  match w with
  Triplet i ->
    ram.(i) <- (field0_word w, v, field2_word w)
  | Int _ | Nil -> failwith "can't field0_set"

let field2_set (w : word) (v : word) =
  match w with
  Triplet i ->
    ram.(i) <- (field0_word w, field1_word w, v)
  | Int _ | Nil -> failwith "can't field0_set"

let instance_of ty =
  fun (w  : word) -> is_rib w && field2_word w = ty

let is_pair (w : word) = instance_of pair_type w
let cons car cdr = make_rib car cdr pair_type
let get_car (w : word) = field0_word w
let get_cdr (w : word) = field1_word w

let pop () : word =
  let (car, cdr, _) = ram.(!stack) in
  stack := int_of_triplet cdr;
  car

let push (w : word) : unit =
  let get_next_i () =
    (* should be responsible for
       triggering garbage collection *)
    stack := !stack - 1
  in
  let new_tos = cons w (Triplet !stack) in
  get_next_i ();
  ram.(!stack) <- new_tos

let alloc_rib (r : rib) : word =
  let get_next_i () =
    (* should trigger GC aswell *)
    heap := !heap + 1
  in
  get_next_i ();
  ram.(!heap) <- r;
  Triplet !heap

let tos () = Triplet !stack

let rec list_tail (w : word) (i : int) : word =
  if 0 < i
  then list_tail (get_cdr w) (i-1)
  else w

let rec pair_length (w : word) =
  if is_pair w
  then 1 + pair_length (get_cdr w)
  else 0

let get_byte () =
  let c = String.get input (!pos) in
  pos := !pos + 1;
  int_of_char c

let eb = 46

let get_code () =
  let x = (get_byte ()) - 35 in
  if x < 0 then 57 else x

let rec get_int n =
  let x = get_code () in
  let y = n * eb in
  if x < eb
  then y + x
  else get_int (y + (x - eb))


(* symbol table *)
let symtbl = ref 3
let top_symbtl () =
    Triplet !symtbl

(* length from w till Nil *)
let rec length (w : word) : int =
  if is_pair w
  then 1 + (length (get_cdr w))
  else 0

let string_of_list (w : word) : rib =
  make_rib w (Int (length w)) string_type

let end_of_string (w : word) : rib =
  make_rib false_rib w symbol_type

let alloc_sym (sym : rib) =
  let get_next_i () =
    (* should trigger GC aswell *)
    symtbl := !symtbl + 1
  in
  get_next_i ();
  ram.(!symtbl) <- sym;
  Triplet !symtbl

let add_symbol (chars : word) =
  let top = top_symbtl () in
  let str_rib = string_of_list chars |> alloc_sym in
  let end_rib = end_of_string str_rib |> alloc_sym in
  let sym = cons end_rib top in
  alloc_sym sym

let build_symbtl () =
  let rec loop1 n =
    if 0 < n
    then begin
      let _ = add_symbol nil_rib in
      loop1 (n-1)
    end
    else loop2 ()
  and loop2 () =
    loop3 nil_rib
  and loop3 chars =
    let x = get_byte () in
    if x = 44
    then let _ = add_symbol chars in loop2 ()
    else if x = 59
    then let _ = add_symbol chars in ()
    else begin
      let c = cons (Int x) chars in
      let c_rib = alloc_sym c in
      loop3 c_rib
    end
  in loop1 (get_int 0)

let decode () =
  let _ =
    build_symbtl ();
    heap := !symtbl + 10;
  in
  let codes = [| 20; 30; 0; 10; 11; 4|] in
  let sym n =
    list_tail (top_symbtl ()) n |> get_car
  in
  let add_instruction op opnd =
    let top = tos () in
    let new_instr = make_rib op opnd (get_car top) in
    let new_rib = alloc_rib new_instr in
    field0_set top new_rib
  in
  let rec decode_loop () =
    let x = get_code () in
    loop 0 x x
  and loop op n x =
    let d = codes.(op) in
    if d+2 < n
    then loop (op+1) (n-(d+3)) x
    else begin
      if 90 < x
      then begin
        let opnd = pop () in
        add_instruction (Int 4) opnd;
        decode_loop ()
      end
      else begin
        if op = 0
        then push (Int op);
        let opnd =
          if n < d
          then
            if op < 3 then sym n else (Int n)
          else if n = d
          then Int (get_int 0)
          else sym (get_int (n-d-1))
        in if 4 < op
        then begin
          let ty = pop () in
          let code_proc = make_rib opnd (Int 0) ty in
          let code_proc_rib = alloc_rib code_proc in
          let proc = make_rib code_proc_rib nil_rib procedure_type in
          let proc_rib = alloc_rib proc in
          if is_rib (tos ())
          then begin
            add_instruction (Int 3) proc_rib;
            decode_loop ()
          end;
        end
        else begin
          let op = if 0 < op then op-1 else 0 in
          add_instruction (Int op) opnd;
          decode_loop ()
          end
        end
      end
  in decode_loop ()


let get_cont () =
  let rec loop w =
    if is_rib (field2_word w)
    then w
    else loop (get_cdr w)
  in loop (tos ())

let get_var (opnd : word) =
  match opnd with
  Triplet _ -> field0_word opnd
  | Int i -> list_tail (tos ()) i
  | Nil -> failwith "can't get_var of Nil"

let set_var opnd v =
  match opnd with
  Triplet _ -> field0_set opnd v
  | Int i -> field0_set (list_tail (tos ()) i) v
  | Nil -> failwith "can't set_var of Nil"


let prim0 f = fun () -> f () |> push
let prim1 f = fun () -> pop () |> f |> push
let prim2 f = fun () ->
  let x = pop () in
  let y = pop () in
  f x y |> push

let prim3 f = fun () ->
  let x = pop () in
  let y = pop () in
  let z = pop () in
  let r = alloc_rib (f x y z) in
  push r

let to_bool x = if x then true_rib else false_rib

let getchar () =
  try input_char stdin |> int_of_char
  with End_of_file -> -1

let putchar (c : int) : int =
  if c = 0
  then Printf.printf "0"
  else char_of_int c |> print_char;
  flush stdout; c


let prim2_int (f : int -> int -> int) =
  let f_rib = fun y x ->
    match y, x with
    Int b, Int a -> Int (f a b)
    | _ -> failwith "not integers"
  in prim2 f_rib

let close () =
  let x = pop () in
  let _ = alloc_rib (x, tos (), procedure_type) in
  ()

let primitives = [|
  prim3 (fun z y x -> make_rib x y z);
  prim1 (fun x -> x);
  (fun () -> pop () |> ignore);
  prim2 (fun y x -> y);
  close;
  prim1 (fun x -> to_bool (is_rib x));
  prim1 field0_word;
  prim1 field1_word;
  prim1 field2_word;
  prim2 (fun y x -> field0_set x y; y);
  prim2 (fun y x -> field1_set x y; y);
  prim2 (fun y x -> field2_set x y; y);
  prim2 (fun y x -> to_bool (is_eqv x y));
  prim2 (fun y x -> match y, x with
    Int b, Int a -> to_bool (a < b)
    | _ -> failwith "not integers");
  prim2_int ( + );
  prim2_int ( - );
  prim2_int ( * );
  prim2_int ( / );
  prim0 (fun () -> Int (getchar ()));
  prim1 (function Int c -> Int (putchar c)
                  | _ -> failwith "can't putchar_prim");
  |]

let rec run () =
  let (instr, opnd, next) = get_rib (Triplet !pc) in
  match instr with
  Int 0 ->
    (* print_string "jump/call\n"; *)
    (*print_stack (tos ());*)
    let proc = get_var opnd in
    let code = field0_word proc in
    begin match code with
    Int i -> (* calling primitive *)
      (* print_string "calling primitive ";
      print_int i;
      print_newline (); *)
      primitives.(i) ();
      if is_rib next
      then pc := int_of_triplet next
      else begin
        (* not a tail call *)
        let cont = get_cont () in
        field1_set (tos ()) (field0_word cont);
        pc := int_of_triplet (field2_word cont)
      end;
      run ()
    | Triplet i -> (* calling a lambda *)
      (* print_string "calling lambda\n"; *)
      let new_cont = make_rib (Int 0) proc pair_type in
      let new_cont_rib = alloc_rib new_cont in
      let nargs = int_of_Int (field0_word code) in
      let ncall = get_car code in
      let ncall = int_of_Int ncall in
      let vari = if nargs = 0 then 0 else 1 in
      (* arity-check *)
      let b1 = vari = 0 && nargs != ncall in
      let b2 = vari = 1 && nargs > ncall in
      if b1 || b2 then failwith "arity-check failed";
      if vari = 1
      then begin
        let rec rest_loop rest i =
          if 0 < i
          then begin
            let car = pop () in
            let new_rest = cons car rest in
            let new_rest_rib = alloc_rib new_rest in
            rest_loop new_rest_rib (i-1)
          end
          else (np := nargs +1; push rest)
        in rest_loop nil_rib (ncall-nargs);
        let rec loop nargs new_stack =
          if 0 < nargs
          then begin
            let car = pop () in
            let new_stack = cons car new_stack in
            let new_stack_rib = alloc_rib new_stack in
            loop (nargs-1) new_stack_rib
          end
          else begin
            if is_rib next
            then begin
              field0_set new_cont_rib (tos ());
              field2_set new_cont_rib next;
              let k = get_cont () in
              field0_set new_cont_rib (field0_word k);
              field2_set new_cont_rib (field2_word k)
            end;
            pc := int_of_triplet (field2_word code);
            stack := int_of_triplet new_stack;
            run ()
          end
        in loop nargs new_cont_rib
      end
    | Nil -> failwith "can't call Nil"
    end
  | Int 1 -> (* set *)
    (* print_string "set\n"; *)
    set_var opnd (pop ());
    pc := int_of_triplet next;
    run ()
  | Int 2 -> (* get *)
    (* print_string "get\n"; *)
    let v = get_var opnd in
    push v;
    pc := int_of_triplet next;
    run ()
  | Int 3 -> (* const *)
    (* print_string "const\n"; *)
    push opnd;
    pc := int_of_triplet next;
    run ()
  | Int 4 -> (* if *)
    (* print_string "if\n"; *)
    let cond = pop () in
    if is_false cond
    then pc := int_of_triplet next
    else pc := int_of_triplet opnd;
    run ()
  | Int 5 -> (* halt *)
    (* print_string "HALT!!\n" *)
          ()
  | _ -> failwith "not implemented yet"

let set_global v =
  field0_set (get_car (top_symbtl ())) v;
  symtbl := int_of_triplet (get_cdr (top_symbtl ()))

let _ =
  ram.(0) <- _nil;
  ram.(2) <- _true;
  ram.(3) <- _false;
  decode ();
  pc := !heap-2;
  let main = make_rib (Int 0) (top_symbtl ()) (Int 1) in
  let main_rib = alloc_rib main in
  set_global main_rib;
  set_global false_rib;
  set_global true_rib;
  set_global nil_rib;
  let halt = make_rib (Int 5) (Int 0) (Int 0) in
  ram.(!stack-1) <- halt;
  ram.(!stack-2) <- (Int 0, Int 0, Triplet (!stack-1));
  stack := !stack-2;
  run ()

[@@@warning "-8"] (* todo *)

open Ast
open Special_subst

let flag_bus_proba = ref 10 ;;

Random.self_init ();;

(* let heap : (l,c array) Hashtbl.t = Hashtbl.create 10 *)

type r = {
  mu : e SMap.t ;
  statics : c array SMap.t ;
  heap : (l,c array) Hashtbl.t
}

let r_init = {
  mu = SMap.empty ;
  statics = SMap.empty ;
  heap = Hashtbl.create 10
}


let add_r x v r =
  { r with mu = SMap.add x v r.mu }

let list_update n v' l =
  let rec aux i = function
  | [] -> []
  | v::r -> if i = 0 then v'::r else v::aux (i-1) r
  in aux n l

let check_bounds ~index:i ~size:n =
if i < 0 || i >= n then
  Prelude.Errors.error ~error_kw:"Runtime error"
    (fun fmt -> Format.fprintf fmt "index out of bounds")

let error_cannot_be_reduced e =
  (* should not happen since the language must be type safe *)
  Prelude.Errors.error (fun fmt ->
                       Format.fprintf fmt "expression %a cannot be reduced."
                       Ast_pprint.pp_exp e)


let set_buffer x e1 e2 r =
  match e1 with
  | E_const(Int(n,_)) ->
      (match SMap.find_opt x r.statics, e2 with
      | Some a, E_const c ->
          a.(n) <- c; (* todo: avoid the side effect *)
          r
      | _ -> assert false (* ill typed *)
    )
  | _ -> assert false (* ill typed *)

let buffer_get x e r =
  match SMap.find_opt x r.statics, e with
  | Some a, E_const(Int (i,_)) ->
      check_bounds ~index:i ~size:(Array.length a);
      a.(i)
  | _ -> assert false (* ill typed *)


let buffer_length x r =
  match SMap.find_opt x r.statics with
  | Some a ->
      Int(Array.length a,Types.new_size_unknown())
  | None -> assert false (* ill typed *)

let rec eq_const c1 c2 =
  match c1,c2 with
  | (Int (n,_)),(Int (m,_)) -> n = m
  | Bool _, Bool _
  | String _, String _ -> c1 = c2
  | C_tuple(cs),C_tuple cs' -> List.compare_lengths cs cs' = 0 
                               && List.for_all2 eq_const cs cs'
  | _ -> false

let app_const e e2 r =
  let c = match e with
          | E_const c -> c
          | _ -> error_cannot_be_reduced @@ E_app(e,e2) in
  match c with
  | Op op -> begin
      match op,e2 with
      | Runtime(Add),  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz'; E_const (Int (n+m,tz)), r
      | Runtime(Sub),  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz'; E_const (Int (n-m,tz)), r
      | Runtime(Mult), E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz';  E_const (Int (n*m,tz)), r
      | Runtime(Div),  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz';  E_const (Int (n/m,tz)), r
      | Runtime(Mod),  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz';  E_const (Int (n mod m,tz)), r
      | Runtime(Lt),   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz';  E_const (Bool (n<m)), r
      | Runtime(Le),   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz';  E_const (Bool (n<=m)), r
      | Runtime(Gt),   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz';  E_const (Bool (n>m)), r
      | Runtime(Ge),   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz';  E_const (Bool (n>=m)), r
      | Runtime(Eq),   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz';  E_const (Bool (n==m)), r
      | Runtime(Neq),  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> Typing.unify_size ~loc:Prelude.dloc tz tz';  E_const (Bool (n!=m)), r
      | Runtime(And),  E_tuple [E_const (Bool a); E_const (Bool b)] -> E_const (Bool (a&&b)), r
      | Runtime(Or),  E_tuple  [E_const (Bool a); E_const (Bool b)] -> E_const (Bool (a||b)), r
      | Runtime(Not),  E_const (Bool b) -> E_const (Bool (not b)), r
      | Runtime(Abs),  E_const (Int (n,tz)) -> E_const (Int (abs n,tz)), r
      | Runtime(Tuple_of_int n),  E_const (Int (l,tz)) ->
          (* https://stackoverflow.com/questions/14328600/ocaml-looking-at-a-specific-digit-of-an-int *)
          let digits n = (* Assuming n is a non-negative number *)
              let rec loop n acc =
                  if n = 0 then acc
                  else loop (n/10) (Bool (n mod 10 = 1)::acc) in
              match n with
              | 0 -> [Bool false]
              | _ -> loop n []
          in
          let ls = digits l in
          let rec loop acc l n =
            match l,n with
            | _,0 -> List.rev acc 
            | x::xs,n -> loop (x::acc) xs (n-1)
            | [],n -> loop (Bool false::acc) l (n-1)
          in 
          let vs = loop [] ls n in
          E_tuple (List.map (fun c -> E_const c) vs), r
      | Runtime(Resize_int k),  E_const (Int (l,tz)) -> E_const (Int (l,Types.Sz_lit k)), r
      | Runtime(Print_string),E_const (String s) ->
         assert (evaluated e);
         Format.fprintf Format.std_formatter "%s" s; flush stdout; E_const Unit, r
      | Runtime(Print_int),E_const (Int(n,_)) ->
         assert (evaluated e);
         Format.fprintf Format.std_formatter "%d" n; flush stdout; E_const Unit, r
      | Runtime(Print_char),E_const (Int(n,_)) ->
         assert (evaluated e);
         Format.fprintf Format.std_formatter "%d" n; flush stdout; E_const Unit, r
      | Runtime(Print_newline),E_const Unit ->
         assert (evaluated e);
         Format.fprintf Format.std_formatter "\n"; flush stdout; E_const Unit, r
  
      | Runtime(Print),e ->
         assert (evaluated e);
         Format.fprintf Format.std_formatter "==> %a\n" Ast_pprint.pp_exp e; flush stdout; E_const Unit, r
      | GetTuple{pos=i;arity=n}, E_tuple vs ->
          check_bounds ~index:i ~size:n;
          List.nth vs i, r
      | Runtime(Assert),E_const (Bool b) -> assert b; E_const Unit, r
      | Wait 0,v ->
          v, r
      | Wait n,v ->
          E_app(E_const(Op(Wait (n-1))),v), r
      | Runtime(String_length),E_const(String s) -> E_const(Int (String.length s,Sz_lit 32)), r
      | TyConstr _, v -> v, r
      | _ -> error_cannot_be_reduced (E_app(e,e2))
    end
  | Inj _ -> E_app(e,e2),r
  | (Unit|Bool _|Int _|String _|V_loc _|C_tuple _) ->
      error_cannot_be_reduced (E_app(e,e2))

let rec find_case c hs =
  match hs with
  | [] -> None
  | (cs,e)::hs' -> if List.exists (eq_const c) cs then Some (e) else
                   find_case c hs'


open Format
let fmt = Format.std_formatter

let rec red (e,r) =
  (* fprintf fmt "%a\n" Ast_pprint.pp_exp  e ;
   (fprintf fmt "i: ";
             SMap.iter (fun x e -> fprintf fmt "    (%s %a)\n" x Ast_pprint.pp_exp e) r.mu;
             fprintf fmt "@,"); *)
  let rec red_list es r =
    let rec aux acc es r =
      match es with
      | [] -> List.rev acc,r
      | e1::es' -> let e1',r1 = red (e1,r) in
                   aux (e1 :: acc) es' r1
    in aux [] es r
  in
  match e with
  | E_deco(e1,_) -> red (e1,r)
  | E_const _ | E_fun (_, _) | E_fix (_, _) ->
      assert (evaluated e);
      (e,r)
  | E_var x -> (match SMap.find x r.mu with
                | v -> v,r
                | exception Not_found ->
                    Prelude.Errors.raise_error ~msg:("unbound variable "^x) ())
  | E_if(e,e1,e2) ->
     (match red (e,r) with
     | E_const (Bool true),r' ->
        (* [If-true] *)
        red (e1,r')
     | E_const (Bool false),r' ->
        (* [If-false] *)
        red (e2,r')
     | (e',r') ->
        (* [If-pause] *)
        E_if(e',e1,e2),r')
  | E_case(e,hs,e_els) ->
     (match red (e,r) with
      | (E_const c,r') ->
         (* [Case-select] *)
         (match find_case c hs with
          | None -> red (e_els,r')
          | Some ei -> red (ei,r'))
      | (e',r') ->
         (* [Case-pause] *)
         E_case(e',hs,e_els),r')
  | E_match(e,hs,eo) ->
     (match red (e,r) with
      | (E_app(E_const (Inj ctor),(E_const _ as v)),r') ->
         (* [Match-select] *)
         (match List.assoc_opt ctor hs with
          | None -> (match eo with
                     | None -> Prelude.Errors.raise_error ~msg:"match failure" ()
                     | Some e' -> (e',r))
          | Some (p,ei) -> red ((subst_p_e p v ei),r'))
      | (e',r') ->
         (* [Match-pause] *)
         E_match(e',hs,eo),r')
  | E_letIn(p,e1,e2) ->
    let e1',r' = red (e1,r) in
    if evaluated e1'
    then (* [Let-val] *)
      red (subst_p_e p e1' e2,  r')
    else (* [Let-pause] *)
      (E_letIn(p,e1',e2),r')
  | E_tuple es ->
      let es',r' = List.fold_right (fun e (acc,r) -> let e',r' = red (e,r) in (e'::acc),r') es ([],r) in
      E_tuple es',r'
  | E_app(e1,e2) ->
      let e2',r' = red (e2,r) in
      if not (evaluated e2')
      then (* [App-pause] *)
        (E_app(e1,e2'), r')
      else
        let v = e2' in
        let e1',r'' = red (e1,r') in
        (match e1' with
         | E_const c -> app_const e1' v r''
         | E_fun(p,e) ->
             red (subst_p_e p v e,r'')
         | (E_fix(g,(p,e))) as w ->
            (E_letIn(p,v,subst_e g w e)),r''  
            (** we do not substituate [p] by [v] in [e] directly as the redex must not be a value *)
         | _ -> assert false)
  | E_reg((p,e1),e0,l) ->
        
  (*     Ast_pprint.pp_exp Format.std_formatter e; 
    Printf.printf "================\n"; *)
      (* [Reg] *)
      let v0,r = red (e0,r) in
      let v = match SMap.find_opt l r.mu with
              | None -> v0
              | Some v -> v in
      let v',r = red (E_letIn(p,v,e1),r) in
      v', add_r l v' r
  (* | E_set(e1,e2) ->
     (* [Set] *)
     let v1,r1 = red (e1,r) in
     let v2,r2 = red (e2,r1) in
     (E_const Unit, add_r x v r') *)
  | E_exec (e1,e2,e3,k) -> (* TODO e3 *)
     (* [Exec] *)
     if not (SMap.mem k r.mu) then
        (* [Exec-init] *)
        let r' = add_r k e1 r in
        red (E_exec (e1,e2,e3,k),r')
      else
        let e = SMap.find k r.mu in
        let e',r' = red (e,r) in
        if evaluated e' then
          (* [Exec-val] *)
          let e1',r' = red (e1,r') in
          E_tuple[e';E_const(Bool true)], (add_r k e1' r')
        else
          (* [Exec-exp] *)
          let v2,r'' = red (e2,r') in
          assert (evaluated v2);
          E_tuple[v2;E_const(Bool false)], (add_r k e' r'')
  | E_par(es) ->
      (* [Par] *)
      let es',r' = red_list es r in
      let e' = if List.for_all evaluated es'
               then E_tuple es'
               else E_par es' in
      e',r'
  | E_local_static_array(e1,_) ->
      let e1',r1 = red (e1,r) in
      assert (evaluated e1');
      let l = Ast.gensym () in
      let n = match un_annot e1' with E_const (Int (n,_)) -> n | _ -> assert false (*error *)
      in
      let any_constant = Unit in
      E_const (V_loc l), {r with statics = SMap.add l (Array.make n any_constant) r.statics}
  | E_array_get (x,e1) ->
      let e1',r1 = red (e1,r) in
      if evaluated e1' then E_const (buffer_get x e1' r),r else
      E_array_get (x,e1'),r1
  | E_array_length(x) ->
      E_const(buffer_length x r),r
  | E_array_set (x,e1,e2) ->
      let e1',r1 = red (e1,r) in
      let e2',r2 = red (e2,r1) in
      if evaluated e1' && evaluated e2' then E_const(Unit), set_buffer x e1' e2' r else
      E_array_set (x,e1',e2'),r2
  | E_generate _ 
  | E_for _ -> assert false (* todo *)

let reduce_until ~nb_iterations:nb (e_init,r) list_args =
   if list_args = [] then 
     Prelude.Errors.error (fun fmt ->
       Format.fprintf fmt "@[<v>Arguments needed to evaluate the program (see option -arg)@]");
  let rec aux i (e,r) args =
    if i > nb then (e,r) else
    match args with
    | [] -> assert false
    | [a] -> aux i (e,r) [a;a]
    | a::args' ->
        let ee = if evaluated e then E_app(e_init,a) else e in
        (* SMap.iter (fun x e -> fprintf fmt "~~~~~> (%s %a)\n" x Ast_pprint.pp_exp e) mu; *)
        let e',r' = red (ee,r) in
        let open Prelude.Errors in
        let open Format in
        (if evaluated e' then
          fprintf std_formatter "%a : %a --> %a@]\n" 
            (emph_pp blue (fun fmt () -> fprintf fmt "cycle %d" i)) () Ast_pprint.pp_exp a Ast_pprint.pp_exp e'
        else
          fprintf std_formatter "%a %a\n" Ast_pprint.pp_exp a (emph red) "(running)");
        aux (i+1) (e',r') args'
  in aux 0 (e_init,r) list_args



let interp ?(init_env=r_init) ~nb_iterations (e : e) (value_list : e list) : (e * r) =
  (* let e = Norm.normalize e in *)
  reduce_until ~nb_iterations (e,init_env) value_list

let prepare_statics (statics: (x * static) list) : c array SMap.t =
  smap_of_list statics |>
  SMap.map (function 
            | Static_array(c,n) -> Array.make n c
            | Static_const c -> Array.make 1 c)


let interp_pi ~nb_iterations (pi : pi) (value_list : e list) : (e * r) =
  let r = { r_init with statics = prepare_statics pi.statics } in
  let e = pi.main in
  interp ~init_env:r ~nb_iterations e value_list

(* *************** evaluate a close expression *************** *)

let eval e =
  let rec aux (e,r) =
    if evaluated e then e else aux (red (e,r))
  in aux (e,r_init)

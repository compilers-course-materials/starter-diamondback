open Expr
open Instruction
open Printf

type 'a envt = (string * 'a) list

let count = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

(*

This is a slightly modified implementation of ANF.  You should read and
understand it (because it's good for you, and because there may be written
questions that reference it).  Your interaction with it in this assignment
will be limited, though.

The key idea is that there are two kinds of holes in expressions:
immexpr-shaped and cexpr-shaped.  For example, a let statement has a
cexpr-shaped hole in the second position, while a prim1 has an immexpr-shaped
hole for its argument.

We can fill an immexpr-shaped hole with a cexpr-shaped expression by
introducing a new let binding, and using the temporary id as the immexpr.  And
we can fill a cexpr-shaped hole with an immexpr by using CImmExpr.

By using the right-shaped holes in the right places, we can avoid creating
intermediate let-bindings except where it's actually necessary.

For this assignment, you need to fill in the EApp case.

*)
type hole =
  | CHole of (cexpr -> aexpr)
  | ImmHole of (immexpr -> aexpr)

let fill_imm (h : hole) (v : immexpr) : aexpr =
  match h with
    | CHole(k) -> (k (CImmExpr(v)))
    | ImmHole(k) -> (k v)

let fill_c (h : hole) (c : cexpr) : aexpr =
  match h with
    | CHole(k) -> (k c)
    | ImmHole(k) ->
      let tmp = gen_temp "" in
      ALet(tmp, c, k (ImmId(tmp)))

let return_hole = CHole(fun ce -> ACExpr(ce))

let rec anf_list (es : expr list) (k : immexpr list -> aexpr) : aexpr =
  (* FILL: you probably want to implement this as a helper for the ECall case *)
  failwith "anf_list not yet implemented"

and anf (e : expr) (h : hole) : aexpr =
  match e with
    | ENumber(n) -> fill_imm h (ImmNumber(n)) 
    | EBool(b) -> fill_imm h (ImmBool(b)) 
    | EId(x) -> fill_imm h (ImmId(x))
    | EPrim1(op, e) ->
      anf e (ImmHole(fun imm -> (fill_c h (CPrim1(op, imm)))))
    | EPrim2(op, left, right) ->
      anf left (ImmHole(fun limm ->
        anf right (ImmHole(fun rimm ->
          (fill_c h (CPrim2(op, limm, rimm)))))))
    | EApp(f, args) ->
      failwith "anf of EApp not yet implemented"
      (* FILL: you need to implement this *)
    | EIf(cond, thn, els) ->
      anf cond (ImmHole(fun cimm ->
        (fill_c h (CIf(cimm, (anf thn return_hole), (anf els return_hole))))))
    | ELet([], body) -> anf body h
    | ELet((name, value)::rest, body) ->
      anf value (CHole(fun ce ->
        ALet(name, ce, anf (ELet(rest, body)) h)))

let anf_decl (d : decl) : adecl =
  match d with
    | DFun(name, args, body) ->
      ADFun(name, args, anf body return_hole)

let anf_program (p : program) : aprogram =
  match p with
    | Program(decls, main) ->
      AProgram(List.map anf_decl decls, anf main return_hole)

let rec find ls x =
  match ls with
    | [] -> None
    | (y,v)::rest ->
      if y = x then Some(v) else find rest x

let const_true = HexConst(0xffffffff)
let const_false = HexConst(0x7fffffff)

let acompile_imm_arg (i : immexpr) _ (env : int envt) : arg =
  match i with
    | ImmNumber(n) ->
      (* NOTE: the static overflow check should be done in well_formed, not here *)
      Const(n lsl 1)
    | ImmBool(b) ->
      if b then const_true else const_false
    | ImmId(name) ->
      begin match find env name with
        | Some(stackloc) -> RegOffset(-4 * stackloc, EBP)
        | None -> failwith ("Unbound identifier" ^ name)
      end

let acompile_imm (i : immexpr) (si : int) (env : int envt) : instruction list =
  [ IMov(Reg(EAX), acompile_imm_arg i si env) ]

let max n m = if n > m then n else m
let rec count_c_vars (ce : cexpr) : int =
  match ce with
    | CIf(_, thn, els) ->
      max (count_vars thn) (count_vars els)
    | _ -> 0

and count_vars (ae : aexpr) : int =
  match ae with
    | ALet(x, bind, body) -> 
      1 + (max (count_c_vars bind) (count_vars body))
    | ACExpr(ce) -> count_c_vars ce

let rec acompile_step (s : cexpr) (si : int) (env : int envt) : instruction list =
  failwith "Compile step not yet implemented"

and acompile_expr (e : aexpr) (si : int) (env : int envt) : instruction list =
  failwith "Compile aexpr not yet implemented"

let acompile_decl (ad : adecl) : instruction list =
  failwith "Compile decl not yet implemented"

(* You may find some of these helpers useful *)
let rec find_decl (ds : decl list) (name : string) : decl option =
  match ds with
    | [] -> None
    | (DFun(fname, _, _) as d)::ds_rest ->
      if name = fname then Some(d) else find_decl ds_rest name

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
    | [] -> false
    | x::xs -> (elt = x) || (find_one xs elt)

let rec find_dup (l : 'a list) : 'a option =
  match l with
    | [] -> None
    | [x] -> None
    | x::xs ->
      if find_one xs x then Some(x) else find_dup xs

let rec well_formed_e (e : expr) (ds : decl list) (env : bool envt) : string list =
  (* FILL: you need to implement this *)
  []

let well_formed_d (d : decl) (ds : decl list) : string list =
  (* FILL: you need to implement this *)
  []

let well_formed_p (p : program) : string list =
  match p with
    | Program(ds, maine) ->
      (* FILL: you may need to add more errors beyond those found from
         the declarations and the main expression *)
      (well_formed_e maine ds []) @
        (List.flatten (List.map (fun d -> well_formed_d d ds) ds))

let compile_to_string prog =
  match well_formed_p prog with
    | x::rest ->
      (* NOTE: This is where errors are reported, by concatenating them all together *)
      let errstr = (List.fold_left (fun x y -> x ^ "\n" ^ y) "" (x::rest)) in
      failwith errstr
    | [] ->
      let anfed = (anf_program prog) in
      (* FILL: You need to get from ANFed program to full assembly structure
      this time, possibly by starting from a previous lab's code *)
      failwith "Compile not yet implemented"


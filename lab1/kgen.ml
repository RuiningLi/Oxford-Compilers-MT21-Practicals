(* lab1/kgen.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

open Tree
open Keiko

let optflag = ref false

(* |gen_expr| -- generate code for an expression *)
let rec gen_expr =
  function
      Constant x ->
        CONST x
    | Variable x ->
        SEQ [LINE x.x_line; LDGW x.x_lab]
    | Monop (w, e1) ->
        SEQ [gen_expr e1; MONOP w]
    | Binop (w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2; BINOP w]

(* |gen_cond| -- generate code for short-circuit condition *)
let rec gen_cond e tlab flab =
  (* Jump to |tlab| if |e| is true and |flab| if it is false *)
  match e with
      Constant x ->
        if x <> 0 then JUMP tlab else JUMP flab
    | Binop ((Eq|Neq|Lt|Gt|Leq|Geq) as w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2; JUMPC (w, tlab); JUMP flab]
    | Monop (Not, e1) ->
        gen_cond e1 flab tlab
    | Binop (And, e1, e2) ->
        let lab1 = label () in
        SEQ [gen_cond e1 lab1 flab; LABEL lab1; gen_cond e2 tlab flab]
    | Binop (Or, e1, e2) ->
        let lab1 = label () in
        SEQ [gen_cond e1 tlab lab1; LABEL lab1; gen_cond e2 tlab flab]
    | _ ->
        SEQ [gen_expr e; CONST 0; JUMPC (Neq, tlab); JUMP flab]

(* |gen_stmt| -- generate code for a statement *)
(*
let rec gen_stmt s =
  match s with
      Skip -> NOP
    | Seq stmts -> SEQ (List.map gen_stmt stmts)
    | Assign (v, e) ->
        SEQ [LINE v.x_line; gen_expr e; STGW v.x_lab]
    | Print e ->
        SEQ [gen_expr e; CONST 0; GLOBAL "lib.print"; PCALL 1]
    | Newline ->
        SEQ [CONST 0; GLOBAL "lib.newline"; PCALL 0]
    | IfStmt (test, thenpt, elsept) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [gen_cond test lab1 lab2;
          LABEL lab1; gen_stmt thenpt; JUMP lab3;
          LABEL lab2; gen_stmt elsept; LABEL lab3]
    | WhileStmt (test, body) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [JUMP lab2; LABEL lab1; gen_stmt body;
          LABEL lab2; gen_cond test lab1 lab3; LABEL lab3]
    | RepeatStmt (body, test) ->
        let lab1 = label () in
        SEQ [LABEL lab1; gen_stmt body; gen_expr test; CONST 0; JUMPC (Eq, lab1)] *)

let rec gen_stmt exit_lab s =
  match s with
      Skip -> NOP
    | Seq stmts -> SEQ (List.map (gen_stmt exit_lab) stmts)
    | Assign (v, e) ->
        SEQ [LINE v.x_line; gen_expr e; STGW v.x_lab]
    | Print e ->
        SEQ [gen_expr e; CONST 0; GLOBAL "lib.print"; PCALL 1]
    | Newline ->
        SEQ [CONST 0; GLOBAL "lib.newline"; PCALL 0]
    | IfStmt (test, thenpt, elsept) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [gen_cond test lab1 lab2;
          LABEL lab1; gen_stmt exit_lab thenpt; JUMP lab3;
          LABEL lab2; gen_stmt exit_lab elsept; LABEL lab3]
    | WhileStmt (test, body) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [JUMP lab2; LABEL lab1; gen_stmt exit_lab body;
          LABEL lab2; gen_cond test lab1 lab3; LABEL lab3]
    | RepeatStmt (body, test) ->
        let lab1 = label () in
        SEQ [LABEL lab1; gen_stmt exit_lab body; gen_expr test; CONST 0; JUMPC (Eq, lab1)]
    | LoopStmt body ->
        let lab1 = label () and lab2 = label () in
        SEQ [LABEL lab1; gen_stmt lab2 body; JUMP lab1; LABEL lab2]
    | Exit -> SEQ [JUMP exit_lab]
    | CaseStmt (switch, cases, default) ->
        let def_lab = label () and case_exit_lab = label () in
        let
          rec dummy_list n x =
            match n with
                0 -> []
              | _ -> x :: dummy_list (n - 1) x
          and
          get_nums (nums, stmt) = nums
          and
          get_stmt (nums, stmt) = stmt
          and
          append_left (stmt, stmts) = SEQ [stmt; stmts; JUMP case_exit_lab]
          and
          make_pair (lab, nums) =
             match nums with
                 []      -> []
               | x :: xs -> (x, lab) :: make_pair (lab, xs)
          and
          make_casearm (f, s) = CASEARM (f, s)
          and
          make_lab x = LABEL x
        in let labs = List.map label (dummy_list (List.length cases) ()) in
        let table = List.concat (List.map make_pair (List.combine labs (List.map get_nums cases))) in
        let code_expr = gen_expr switch and code_stmts = List.map (gen_stmt exit_lab) (List.map get_stmt cases) in
        let n = List.length table in
        let code_jump = SEQ [CASEJUMP n; SEQ (List.map make_casearm table); JUMP def_lab] in
        let code_body = List.map append_left (List.combine (List.map make_lab labs) code_stmts) in
        let code_rest = SEQ [LABEL def_lab; gen_stmt exit_lab default; LABEL case_exit_lab] in
        SEQ [code_expr; code_jump; SEQ code_body; code_rest]


(* |translate| -- generate code for the whole program *)
let translate (Program ss) =
  let lab1 = label () in
  let code = SEQ [gen_stmt lab1 ss; LABEL lab1] in
    Keiko.output (if !optflag then Peepopt.optimise code else code)

(* lab2/dict.mli *)
(* Copyright (c) 2017 J. M. Spivey *)

type ident = string

type ptype = 
    Integer 
  | Boolean 
  | Array of int * ptype
  | Void

(* |def| -- definitions in environment *)
type def = 
  { d_tag: ident;               (* Name *)
    d_type: ptype;              (* Type *)
    d_lab: string }             (* Global label *)

type environment

(* |define| -- add a definition, raise Exit if already declared *)
val define : def -> environment -> environment

(* |lookup| -- search an environment or raise Not_found *)
val lookup : ident -> environment -> def

(* |init_env| -- initial empty environment *)
val init_env : environment

(* |type_size| -- compute the size occupied by a value of a given type *)
val type_size : ptype -> int

(* |is_array| -- return false for the types Integer and Boolean, and true for array types *)
val is_array : ptype -> bool

(* |base_type| -- return the base type of an array type *)
val base_type : ptype -> ptype

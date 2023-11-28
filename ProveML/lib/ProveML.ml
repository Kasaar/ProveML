include Ast

module Parser = Parser
module Lexer = Lexer

let parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
     ast


let rec string_of_pattern (p : pattern) : string =
  match p with
  | Constructor (name, []) -> name
  | Constructor (name, patterns) -> name ^ " (" ^ (String.concat ", " (List.map string_of_pattern patterns)) ^ ")"
  | Variable (name, type_name) -> name ^ " : " ^ type_name

let rec string_of_expression (e : expression) : string =
  match e with
  (* If you're confused about the structure of the AST,
     you can use this code to print more parentheses
     (besides using utop):
  | Application (Application (Identifier ",", e), arg) ->
    (string_of_expression_paren e) ^ ", " ^ (string_of_expression_paren arg)
  | Application (e, arg) ->
    (string_of_expression_paren e) ^ " " ^ string_of_expression_paren arg
     
     *)
  | Application (Application (Identifier ",", e), arg) ->
    (string_of_expression e) ^ ", " ^ (string_of_expression arg)
  | Application (e, arg) ->
    (string_of_expression e) ^ " " ^ string_of_expression_paren arg
  | Identifier name -> name
  | Match (e, cases) ->
    let case_strings = List.map (fun (pattern, body) ->
      let pattern_string = match pattern with
        | Constructor (name, []) -> name
        | Constructor (name, patterns) -> name ^ " (" ^ (String.concat ", " (List.map string_of_pattern patterns)) ^ ")"
        | Variable (name, type_name) -> name ^ " : " ^ type_name
      in
      (* the outer parentheses are redundant if the body does not end in a match, but better to be safe then sorry *)
      pattern_string ^ " -> " ^ (string_of_expression_paren body)
    ) cases in
    "match " ^ (string_of_expression e) ^ " with " ^ (String.concat " | " case_strings)

and string_of_expression_paren (e : expression) : string =
  match e with
  | Identifier name -> name
  | e -> "(" ^ string_of_expression e ^ ")"

let string_of_hint (h : hint option) : string =
  match h with
  | Some Axiom -> "\n(*hint: axiom *)"
  | Some (Induction name) -> "\n(*hint: induction " ^ name ^ " *)"
  | None -> ""
let string_of_equality (e : equality) : string =
  match e with
  | Equality (e1, e2) -> "(" ^ (string_of_expression e1) ^ " = " ^ (string_of_expression e2) ^ ")"
let string_of_typedvariable (TypedVariable (name, type_name) : typedVariable) : string =
  "(" ^ name ^ " : " ^ type_name ^ ")"
let string_of_declaration (d : declaration) : string =
  match d with
  | TypeDeclaration (name, variants) ->
    let variant_strings = List.map (function Variant (name, []) -> name
      | Variant (name, types) -> name ^ " of (" ^ (String.concat "*" types) ^ ")"
    ) variants in
    "type " ^ name ^ " = " ^ (String.concat " | " variant_strings)
  | FunctionDeclaration (TypedVariable (name, type_name), args, body) ->
    let arg_strings = List.map (function TypedVariable (name, type_name) -> "(" ^ name ^ " : " ^ type_name ^ ")") args in
    "let rec " ^ name ^ " " ^ (String.concat " " arg_strings) ^ " : " ^ type_name ^ " = " ^ (string_of_expression body)
  | ProofDeclaration (name, args, equality, hint) ->
    let arg_strings = List.map string_of_typedvariable args in
    "let (*prove*) " ^ name ^ " " ^ (String.concat " " arg_strings) ^ " = "
     ^ string_of_equality equality ^ string_of_hint hint

module Substitution:
sig
type 'a t
val empty : 'a t
val singleton : string -> 'a -> 'a t
val merge : 'a t -> 'a t -> 'a t option
val find : string -> 'a t -> 'a
end = struct
module MM = Map.Make(String)
type 'a t = 'a MM.t

let empty = MM.empty
let singleton = MM.singleton
let merge m1 m2 = Some (MM.merge (fun _k v1 v2 -> Some (match v1, v2 with
| Some v1', _ -> v1'
| _, Some v2' -> v2'
| None, None -> failwith "impossible"
)) m1 m2)
let find = MM.find
end

let (>>=) x f =
match x with
| Some v -> f v
| None -> None

(* let rec match_expressions (variables : string list) (pattern : expression) (goal : expression) : expression Substitution.t option =
match pattern, goal with
| Identifier id1, Identifier id2 ->
(* If the identifier is in the list of variables, create a substitution *)
if List.mem id2 variables then Some (Substitution.singleton id1 (Identifier id2)) else None
| Application (pat1, pat2), Application (expr1, expr2) ->
match_expressions variables pat1 expr1 >>= fun result1 ->
match_expressions variables pat2 expr2 >>= fun result2 ->
(Substitution.merge result1 result2)
| _ -> None *)

let rec match_expressions variables pattern expression =
match pattern with
| Identifier x -> if List.mem x variables then (* Todo: fix test for variables: the string "x" is not necessarily the only variable and it might not always be a variable either *)
(* if x is a variable: *)
Some (Substitution.singleton x expression)
else
(* if x is a constant: *)
(if pattern = expression then Some Substitution.empty else None)
| Application (p1, p2) -> (* x has to be an application too, otherwise return None *)
(match expression with
| Application (e1, e2) ->
(* We recursively match the sub-expressions.
This part is much easier to write if e2 is an expression (and it is for this particular ast),
because it's so symmetrical *)
(match match_expressions variables p1 e1, match_expressions variables p2 e2 with
| Some s1, Some s2 -> Substitution.merge s2 s1
| _ -> None)
| _ -> None)
| _ -> None

let rec attemptRewrite (variables : string list) (equality : equality) (expr : expression) : expression option =
let lhs = match equality with
| Equality (lhs, _) -> lhs
in
let rhs = match equality with
| Equality (_, rhs) -> rhs
in
match match_expressions variables lhs expr with
| Some map -> Some (rewrite variables map rhs)
| None -> (match expr with 
| Application(fn, arg) -> (match attemptRewrite variables equality arg with
| Some v -> Some (Application (fn, v))
| None -> None)
| _ -> None
)
and rewrite variables map rhs =
        match rhs with
        | Identifier nm -> (try (Substitution.find nm map) with Not_found -> (Identifier nm))
        | Application (e1, e2) -> Application (rewrite variables map e1, rewrite variables map e2)
        | _ -> failwith "not implemented"

let rec tryEqualities (expr : expression) (equations : (string * string list * equality) list) : (string * expression) option =
match equations with
| [] -> None
| h :: t -> (match h with
| nm, variables, equality -> (match attemptRewrite variables equality expr with
| Some e -> Some (nm, e)
| None -> tryEqualities expr t))

let rec performSteps (expr : expression) (equations : (string * string list * equality) list) : (string * expression) list =
match equations with
| [] -> []
| h :: t -> (match tryEqualities expr (h :: t) with
| None -> []
| Some (nm, e) -> (performSteps e t) @ [(nm, e)])


let rec compute_lhs lhs_steps acc =
match lhs_steps with
| [] -> acc
| h :: t -> (match h with
| (nm, e) ->  (compute_lhs t (["= {" ^ nm ^ "}"] @ ["\t" ^ (string_of_expression e)] @ acc)))

let rec compute_rhs rhs_steps acc =
match rhs_steps with
| [] -> acc
| h :: t -> (match h with
| (nm, e) ->  (compute_rhs t (acc @ ["= {" ^ nm ^ "}"] @ ["\t" ^ (string_of_expression e)])))

let produceProof (equality : equality) (equations : (string * string list * equality) list) : string list =
let lhs = match equality with
| Equality (lhs, _) -> lhs
in
let rhs = match equality with
| Equality (_, rhs) -> rhs
in
let lhs_steps = performSteps lhs equations
in
let rhs_steps = performSteps rhs equations
in
let lhs_proof = compute_lhs lhs_steps []
in
let rhs_proof = compute_rhs rhs_steps []
in lhs_proof @ rhs_proof

let rec variables_to_string vars =
        match vars with
        | [] -> []
        | h :: t -> (match h with
                | TypedVariable (nm, _) -> ([nm] @ (variables_to_string t)))

let rec prover rules declarations =
match declarations with
| ProofDeclaration (nm, vars, Equality (lhs,rhs), None) :: rest
-> (* no hint, so let's prove this *)
produceProof (Equality (lhs,rhs)) rules :: prover ((nm,(variables_to_string vars),(Equality (lhs,rhs)))::rules) rest
| ProofDeclaration (nm, vars, Equality (lhs,rhs), _) :: rest
-> (* we got a hint so we simply assume the statement *)
prover ((nm,(variables_to_string vars),(Equality (lhs,rhs)))::rules) rest
| _ :: rest -> prover rules rest
| [] -> []
let prover_main decls =
prover [] decls |>
List.map (String.concat "\n") |>
String.concat "\n\n" |>
print_endline

let test_main decls =
        let expr = (match decls with
                | ProofDeclaration (_, _, Equality (lhs,_), _) :: _ -> lhs
                | _ -> failwith "lol")
        in
        let rest = (match decls with
                | ProofDeclaration (_, _, _, _) :: rest -> rest
                | _ -> failwith "lol")
        in
        match rest with
        | ProofDeclaration (_, vars, Equality (lhs,rhs), _) :: _ ->
                (match (attemptRewrite (variables_to_string vars) (Equality (lhs,rhs)) expr) with
                | Some e -> string_of_expression e
                | None -> "nothing") |> print_endline
        | _ -> ()
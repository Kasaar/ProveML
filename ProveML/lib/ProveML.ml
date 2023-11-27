include Ast
module Parser = Parser
module Lexer = Lexer

let rec string_of_expression e =
        match e with
        | Identifier nm -> nm
        | Application (e1, e2) -> (match e2 with
                | Identifier "," -> (string_of_expression e1) ^ ","
                | _ -> (string_of_expression e1) ^ " " ^ (string_of_expression_with_parens e2))
and string_of_expression_with_parens e =
        match e with
        | Identifier nm -> nm
        | Application _ -> "(" ^ (string_of_expression e) ^ ")"

let string_of_equation e =
        match e with
        | Equality (e1, e2) -> "(" ^ (string_of_expression e1) ^ " = " ^ (string_of_expression e2) ^ ")"

let string_of_argument (Arg (name, type_name) : argument) : string =
        "(" ^ name ^ " : " ^ type_name ^ ")"

let string_of_hint h =
        match h with
        | Some Axiom -> "\n(*hint: axiom *)"
        | None -> ""

let string_of_declaration d =
        match d with
        | Let (nm, args, e, hint) -> 
                let arg_strings = List.map string_of_argument args in
                "let (*prove*) " ^ nm ^ " " ^ (String.concat " " arg_strings) ^ " = " ^ (string_of_equation e) ^ (string_of_hint hint)
        | _ -> "Not yet implemented, check ProveML.ml"
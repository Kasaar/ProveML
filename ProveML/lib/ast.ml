type expression =
        | Identifier of string
        | Application of expression * expression

type equation =
        | Equality of expression * expression

type argument =
        | Arg of string * string

type hint =
        | Axiom

type declaration =
        | Let of string * argument list * equation * hint option
        | Rec of expression * declaration
        | Type of expression
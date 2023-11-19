type expression =
        | Identifier of string
        | Application of expression * expression
        | Equals of expression * expression

type equation =
        | Operator of string
        | Type of expression * expression
        | Equation of expression * equation * expression

type declaration =
        | Let of expression * equation

type ast =
    Const of int
  | Add of ast * ast
  | Difference of ast * ast
  | Mul of ast * ast
  | Division of ast * ast
  | Unary_Minus of ast


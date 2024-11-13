{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex = ['0']['x' 'X'] (['a'-'f' 'A'-'F'] | num num* ['a'-'f' 'A'-'F']*)+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  | hex { CONST (Lexing.lexeme lexbuf) } 
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }

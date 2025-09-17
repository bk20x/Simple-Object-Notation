import std/[strutils, streams, re]




type
  TokenType* = enum
    tkInt,
    tkFloat,
    tkBool,
    tkString,
    tkObjStart,
    tkObjEnd,
    tkListStart,
    tkListEnd,
    tkColon,
    tkSemicol,
    tkNil,
    tkWhitespace

  Token* = object
    case kind*: TokenType
    of tkInt:
      intVal*: int
    of tkFloat:
      floatVal*: float
    of tkBool:
      boolVal*: bool
    of tkString:
      strVal*: string
    else:
      nil

  LexerState* = enum
    Ok, EOF, ExpectValue, InObject, InList
      
  Lexer* = object
    pos*: int
    buf*: string
    input*: Stream
    last*: Token
    expected*: TokenType
    state*: LexerState
    toks*: seq[Token]



    
let
  Patterns*: array[TokenType, Regex] = [
     re"^-?\d+$",
     re"[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?",
     re"true|false",
     re"[A-Za-z][A-Za-z0-9_-]*",
     re"\(",
     re"\)",
     re"\[",
     re"\]",
     re"\:",
     re"\;",
     re"\b\s*nil\s*\b",
     re"\s+"
   ]




func rest*(lx: var Lexer): string {.inline.} =
  lx.buf[lx.pos..^1]

  
proc flush(lx: var Lexer): seq[Token] {.inline.} =
  result = lx.toks
  lx.toks.setLen 0
  return result  

  
#proc captureTokVal() 


## State ::= Ok.
proc skipWhitespace*(lx: var Lexer) {.inline.} =
  var pos = lx.pos
  while lx.rest.match Patterns[tkWhitespace]:
    let matchLn = lx.rest.matchLen Patterns[tkWhitespace]
    inc(pos, matchLn)
  lx.pos = pos





var test* = """
      (x: 250;
       y: 250.5;
       z: worben)
"""

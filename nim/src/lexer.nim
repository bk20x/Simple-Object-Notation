import std/[strutils, streams, re]



var test = """
   (name: green_slime;
    health: 100;
    speed: 50;
    rarity: COMMON)
"""


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

  LexerState = enum
    Ok, EOF, ExpectValue, InObject, InList
      
  Lexer = object
    pos: int
    buf: string
    input: Stream
    last: Token
    expected: TokenType
    state: LexerState
    toks: seq[Token]


let
  Patterns: array[TokenType, Regex] = [
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


proc flush(lx: var Lexer): seq[Token] {.inline.} =
  result = lx.toks
  lx.toks.setLen 0
  return result
  
  

proc skipWhitespace(lx: var Lexer) {.inline.} =
  var pos = lx.pos
  while lx.buf[pos..pos].match Patterns[tkWhitespace]:
    let matchLn = lx.buf[pos..pos].matchLen Patterns[tkWhitespace]
    inc(pos, matchLn)
  lx.pos = pos
    
#proc parseValue(lx: var Lexer) {.inline.} =
  
#proc parseSon(lx: var Lexer): seq[Token] =
#  while not lx.state == EOF:
#    case lx.state
#    of Ok:
#     lx.skipWhitespace()
#    of ExpectValue:
#     lx.parseValue()
#    of InObject:
#     lx.parseObject()
#  return lx.flush()




echo "yoben!"





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
    tkNil

  Token* = object
    case kind*: TokenType
    of tkInt:
      intVal: int
    of tkFloat:
      floatVal: float
    of tkBool:
      boolVal: bool
    of tkString:
      stringVal: string
    else:
      nil

  SonLexer = object
    pos: int
    





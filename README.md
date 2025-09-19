# SON
### Simple serialization format for lisp with simple conversion to classes
```

(use-package :son)  ;;; or prefix exports with son:
=> T



;; Convert some son into a list of tokens
CL-USER> (defvar toks
           (lex 
            "(foo: 4.5; bar: Yelloooo; boben: (lst: [2; 4; (x: 5)] ) )"))
=> TOKS

CL-USER> toks
=> ((:OBJ-START) (:IDENT . "foo") (:COLON) (:FLOAT . 4.5) (:SEMICOL)
   (:IDENT . "bar") (:COLON) (:IDENT . "Yelloooo") (:SEMICOL) (:IDENT . "boben")
   (:COLON) (:OBJ-START) (:IDENT . "lst") (:COLON) (:LIST-START) (:INT . 2)
   (:SEMICOL) (:INT . 4) (:SEMICOL) (:OBJ-START) (:IDENT . "x") (:COLON)
   (:INT . 5) (:OBJ-END) (:LIST-END) (:OBJ-END) (:OBJ-END))



CL-USER> (defvar obj
           (parse-toks toks))
=> OBJ

CL-USER> obj
=> #<SON-OBJECT {10056579D3}>


;; Fields is a hashtable so you can think about it as pretty much the same as JSON
CL-USER> (fields obj)
=> #<HASH-TABLE :TEST EQUAL :COUNT 3 {1005559C63}> 


;; Get a field by name
CL-USER> (field "foo" obj)
=> 4.5


CL-USER> (field "lst"
          (field "boben" obj))
=> #<SON-LIST {1005657953}>   ;; A son list is enclosed in '[]'


;; The elems field of a son-list is a simple list
CL-USER> (elems 
          (field "lst"
           (field "boben" obj)))
=> (2 4  #<SON-OBJECT {10055D8313}>) 


;; `elem` gives you Direct access to the elements of a son-list
CL-USER> (elem 0        
          (field "lst"
           (field "boben" obj)))
=> 2

;; convert a son-object to a class
CL-USER> (defclass vec3 ()
           ((x)(y)(z)))
=> #<STANDARD-CLASS COMMON-LISP-USER::VEC3>

CL-USER> (defvar vecobj
           (parse-toks
            (lex "(x: 25; y: 50; z: 75)")))
=> VECOBJ

CL-USER> (defvar vec
           (to-class vecobj 'vec3))
=> VEC

CL-USER> (describe vec)
=> #<VEC3 {1003BFA3F3}>
   [standard-object]

 Slots with :INSTANCE allocation:
   X                              = 25
   Y                              = 50
   Z                              = 75


```

```
(
  mobs:[
   (name: green_slime; --- No quotes required for identifiers or String values, Strings are encoded for you.
    health: 50;
    damage: 5;
    speed: 55;
    rarity: COMMON --- Semicolons optional for the last element in a list/object. 
   );                  ^^^ they are required if you remove all whitespace, uglification for example.
   (name: blue_slime;
    health: 50;
    damage: 10;
    speed: 55;
    rarity: COMMON    
   )
 ];
  items: [
    (name: health_potion;
     item-type: POTION;
     rarity: UNCOMMON
    );
    (name: staff_of_frostbite;
     item-type: WEAPON;
     rarity: RARE
    )
  ]
  
)
```




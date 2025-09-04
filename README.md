# Simple Object Notation
## S-expression based serialization/deserialization format that's easy to parse and easy on the eyes.

### This project uses this beautiful package:
###  alexa: A Lexical Analyzer Generator -- https://github.com/quil-lang/alexa

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


# Simple Object Notation
## S-expression based format designed to be easy to parse and easy on the eyes. I plan to make it fully mappable and interchangeable with JSON.

```
(
  mobs:[
   (name: green_slime; --- No quotes required for identifiers or String values
    health: 50;
    damage: 5;
    speed: 55;
    rarity: COMMON --- Semicolons optional for the last element in a list/object
   );             
   (name: blue_slime;
    health: 50;
    damage: 10;
    speed: 55;
    rarity: COMMON    
   );
   (name: orange_slime;
    health: 75;
    damage: 20;
    speed: 55;
    rarity: COMMON
   );
   (name: skeleton;
    health: 80;
    damage: 25;
    speed: 60;
    rarity: COMMON
   );
   (name: will-o-wisp;
    health: 100;
    damage: 35;
    speed: 30;
    rarity: UNCOMMON
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
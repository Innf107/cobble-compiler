summon minecraft:armor_stand 0 0 0 {Marker:1, Invisible:1, Tags:["ANCHOR"]}
summon minecraft:armor_stand 0 0 0 {Marker:1, Invisible:1, Tags:["STACK"]}
scoreboard objectives add REGS dummy
scoreboard objectives setdisplay sidebar REGS
scoreboard players set R1 REGS 5
scoreboard players operation R2 REGS = R1 REGS

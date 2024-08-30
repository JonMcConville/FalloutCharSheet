library(tidyverse)

CharStats <- read.csv("C:/RLibraries/FalloutTTRPG/Grizzly.csv")
SkillDesc <- read.csv("C:/RLibraries/FalloutTTRPG/SkillDesc.csv")
PerkDesc <- read.csv("C:/RLibraries/FalloutTTRPG/Perks.csv")
Ammunition <- read.csv("C:/RLibraries/FalloutTTRPG/Ammunition.csv")
Weapons <- read.csv("C:/RLibraries/FalloutTTRPG/FalloutTTRPG_Weapons.csv")
Armour <- read.csv("C:/RLibraries/FalloutTTRPG/FalloutTTRPG_Armour.csv")


SPECIAL <- CharStats %>%
  filter(Attributetype == "SPECIAL")%>%
  select(Attribute, Value)%>%
  pivot_wider(names_from = Attribute, values_from = Value)

Skills <- CharStats %>%
  filter(Attributetype == "Skills")%>%
  select(Attribute,Value)

Inventory <- CharStats %>%
  filter(Attributetype == "Inventory")%>%
  select(Attribute, Value)

Ammunition <- CharStats %>%
  filter(Attributetype == "Ammunition")%>%
  select(Attribute, Value)

Caps <- CharStats %>%
  filter(Attributetype == "Caps")%>%
  select(Value)


?pivot_wider


Test <- merge(
  SkillDesc %>%
    select("Skill" = SKILL,"Perk" = ATTRIBUTE),
  CharStats %>%
    filter(Attributetype == "Skills")%>%
    select("Skill" = Attribute,Value),
  by = "Skill")



DerivedCharStats <- data.frame(
  Attribute = c("Carry Weight", "Defence", "Initiative", "Health Points", "Melee Damage"),
  Value = c(CharStats %>%
              filter(Attribute == "Strength")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1]%>%
              `*`(10)%>%
              `+`(150),
            if(
              CharStats %>%
              filter(Attribute == "Agility")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1]>8) {
              2}
            else {
              1},
            CharStats %>%
              filter(Attribute == "Perception")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1] + 
              CharStats %>%
              filter(Attribute == "Agility")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1],
            CharStats %>%
              filter(Attribute == "Endurance")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1] + 
              CharStats %>%
              filter(Attribute == "Luck")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1],
            if(
              CharStats %>%
              filter(Attribute == "Strength")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1]>10) {
              3}
            else {
              if(
                CharStats %>%
                filter(Attribute == "Strength")%>%
                mutate(Value = as.numeric(Value)) %>%
                .$Value %>%
                .[1]>8) {
                2}
              else {
                if(
                  CharStats %>%
                  filter(Attribute == "Strength")%>%
                  mutate(Value = as.numeric(Value)) %>%
                  .$Value %>%
                  .[1]>6) {
                  1}
                else {
                  0}
              }
            }
            
  )
)
  
  
  c("Attribute", "Value"),
c("CarryWeight", CharStats %>%
    filter(Attribute == "Strength")%>%
    .$Value %>%
    .[1]%>%
    `*`(10)%>%
    `+`(150))
    
    150+as.numeric((CharStats$Value) %>% filter(Attribute == Strength))*10)



CharStats %>%
  filter(Attribute == "Name") %>%
  .$Value

Inventory %>%
  filter(Attribute == "Fury") %>%
  .$Value[1]



Test <- merge(merge(merge(merge(CharStats%>%
        filter(Attributetype=="Inventory")%>%
        select(Attribute),
      Weapons,
      by.x = "Attribute",
      by.y = "WEAPON"
      ),
      CharStats%>%
        filter(Attributetype == "Skills")%>%
        select(Attribute, "SkillValue" = Value),
      by.x = "WEAPON.TYPE",
      by.y = "Attribute"),
      SkillDesc,
      by.x = "WEAPON.TYPE",
      by.y = "SKILL"),
      CharStats%>%
        filter(Attributetype == "SPECIAL")%>%
        mutate(Name_short = toupper(substr(Attribute, 1, 3)))%>%
        select(Name_short, Value),
      by.x ="ATTRIBUTE",
      by.y = "Name_short")%>%
  mutate(TN = as.numeric(SkillValue) + as.numeric(Value))%>%
  select("Name" = Attribute, "Skill" = WEAPON.TYPE, TN, "Damage" = DAMAGE.RATING, "Effects" = DAMAGE.EFFECTS, "Type" = DAMAGE.TYPE, "Rate" = FIRE.RATE, "Range" = RANGE, "Qualities" = QUALITIES, "Weight" = WEIGHT)


CharStats%>%
  filter(Attributetype == "SPECIAL")%>%
  mutate(Name_short = toupper(substr(Attribute, 1, 3)))%>%
  select(Name_short, Value)
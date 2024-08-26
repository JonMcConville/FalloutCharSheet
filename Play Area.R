library(tidyverse)

CharStats <- read.csv("C:/RLibraries/FalloutTTRPG/Grizzly.csv")
SkillDesc <- read.csv("C:/RLibraries/FalloutTTRPG/SkillDesc.csv")
PerkDesc <- read.csv("C:/RLibraries/FalloutTTRPG/Perks.csv")


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


Ammunition <- read.csv("C:/RLibraries/FalloutTTRPG/Ammunition.csv")


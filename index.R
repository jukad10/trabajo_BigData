#- código usado en index.qmd
#- es el código que uso en mi trabajo


#- pkgs
library(palmerpenguins)
library(tidyverse)

#- datos
df <- palmerpenguins::penguins


#- cojos datos de pingüinos y arreglo un poco 
df <- palmerpenguins::penguins %>% 
  dplyr::select(species, bill_depth_mm, body_mass_g) %>% 
  tidyr::drop_na()



p <- ggplot(data = df, 
            mapping = aes(x = bill_depth_mm, y = body_mass_g, color = species)) +
  geom_point()

p


#- segundo gráfico
p <- p + geom_smooth(method = "lm", se = FALSE) 

p


#- tercer gráfico
p + geom_smooth(method = "lm", se = FALSE, color = "black") 

p
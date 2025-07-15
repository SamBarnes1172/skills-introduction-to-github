library(tidyverse)
library(quarto)
library(janitor)
library(scales)
library(ggrepel)
library(patchwork)
library(dplyr)
library(here)
library(rstatix)
library(report)
library(ggstatsplot)
library(lsr)
data(penguins)
#create data with id
penguins_id <- penguins
penguins_id <- penguins_id |> mutate(id = row_number()) |> relocate(id)


penguins |> relocate(sex, year)

adel <- penguins |>
  filter(species == "Adelie")

penguins |>
  mutate(len_mass = flipper_len / body_mass) |>
  relocate(len_mass) |>
  group_by(island)

ggplot(data = penguins, mapping = aes(x = flipper_len, y = body_mass)) +
  geom_point(mapping = aes(colour = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions of Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)",
    colour = "Species",
    shape = "Species"
  )

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = class)) +
  geom_point()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = class)) +
  geom_smooth(aes(linetype = drv)) +
  geom_point()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_smooth(aes(linetype = drv)) +
  geom_point()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_smooth()

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl, scales = "free")

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

data("diamonds")

#pretty cool graph
ggplot(data = diamonds, aes(x = cut, fill = color)) +
  geom_bar(position = "dodge")

smaller <- diamonds |> filter(carat < 3)

ggplot(data = smaller, aes(x = carat)) + geom_histogram(binwidth = 0.01)

ggplot(data = diamonds, aes(x = price, y = after_stat(density))) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

diamonds |>
  count(color, cut) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))

mpgg_1 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  labs(
    x = "Engine Displacement",
    y = "Highway Mileage",
    title = "Highway Mileage Assosciated with Engine Displacement"
  )

mpgg_2 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  labs(x = "City Milage", y = "Highway Mileage")

mpgg_1 + mpgg_2

#start of long patchwork plot
p1 <- ggplot(mpg, aes(x = drv, y = cty, color = drv)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy, color = drv)) +
  geom_boxplot(show.legend = FALSE) +
  labs(titel = "Plot 2")
p3 <- ggplot(mpg, aes(x = cty, color = drv, fill = drv)) +
  geom_density(alpha = 0.5) +
  labs(title = "Plot 3")
p4 <- ggplot(mpg, aes(x = hwy, color = drv, fill = drv)) +
  geom_density(alpha = 0.5) +
  labs(title = "Plot 4")
p5 <- ggplot(mpg, aes(x = cty, y = hwy, color = drv)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~drv) +
  labs(title = "Plot 5")
(guide_area() / (p1 + p2) / (p3 + p4) / (p5)) +
  plot_annotation(
    title = "City and highway mileage for cars with different drive trains"
  ) +
  plot_layout(guides = "collect", heights = c(1, 2, 3, 4)) &
  theme(legend.position = "top")

#exercise patchwork
p6 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  labs(title = "Plot 1")
p7 <- ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot() +
  labs(title = "Plot 2")
p8 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  labs(title = "Plot 3")

(p6 / (p7 | p8))

#fun practice
penplot1 <- ggplot(penguins, aes(x = bill_len, y = bill_dep, color = species)) +
  geom_point() +
  labs(y = "Bill Depth (mm)", x = "Bill Length (mm)")
penplot2 <- ggplot(penguins, aes(x = island, y = species)) + geom_count()
(penplot1 / penplot2) +
  plot_annotation(title = "Penguin Stuff") +
  labs(x = "Species", y = "Islands")
plot_layout(heights = c(1, 5))

#this one dosen't look good
penguins |>
  count(island, species) |>
  ggplot(aes(x = species, y = island)) +
  geom_tile(aes(fill = n))

penguins |> filter(is.na(sex))

#vector practice
x <- (1:20)
result <- case_when(x %% 2 == 0 ~ "even", TRUE ~ "odd")

#count practice
penguins |> count(species, sort = TRUE) -> spe_num
penguins |> group_by(species) |> summarize(sum(is.na(sex))) -> no_sex
penguins |>
  group_by(species) |>
  count(body_mass > 3550, (!is.na(body_mass))) -> heavy
penguins |> count(is.na(body_mass))

#stats practice
penguins |> ggplot(aes(x = species, y = bill_len)) + geom_boxplot()
aov(bill_len ~ species, data = penguins) -> bill_spec
report(bill_spec)
tukey_hsd(bill_spec) -> tukey_spec
#good graph that shows pairwise comparisons
ggbetweenstats(
  data = penguins,
  x = species,
  y = bill_len,
  type = "parametric",
  var.equal = TRUE,
  plot.type = "box",
  pairwise_comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
#more stats practice
penguins |> ggplot(aes(x = island, y = bill_len)) + geom_boxplot()
aov(island ~ bill_len, data = penguins) -> is_bill

ggbetweenstats(
  data = penguins,
  x = island,
  y = bill_len,
  type = "parametric",
  var.equal = TRUE,
  plot.type = "box",
  pairwise_comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)

#even more stats practice
real_pen <- penguins |> group_by(sex) |> na.exclude(sex)
model <- aov(body_mass ~ sex, data = real_pen)
report(model)

real_pen |>
  ggplot(aes(x = sex, y = body_mass)) +
  geom_boxplot() +
  theme_classic() +
  labs(
    x = "Sex",
    y = "Body Mass (g)",
    title = "Body Mass Differences Between Sex in Penguins"
  )

penguins |> count(species)

ggbetweenstats(
  data = real_pen,
  x = sex,
  y = body_mass,
  type = "parametric",
  var.equal = TRUE,
  plot.type = "box",
  pairwise_comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
) +
  labs(x = "Sex", y = "Body Mass (g)", colour = "species")

summary(real_pen)

real_pen |>
  ggplot(aes(x = sex, y = body_mass)) +
  geom_boxplot() +
  geom_point(aes(color = species)) +
  geom_jitter(aes(color = species)) +
  theme_classic() +
  labs(
    x = "Sex",
    y = "Body Mass",
    title = "Relation Between Sex, Species, and Body Mass in 344 Penguins"
  )

#joining practice
penguins_id <- penguins
penguins_id <- penguins_id |> mutate(id = row_number()) |> relocate(id)
penguins <- penguins |> mutate(id = row_number()) |> relocate(id)
penguins <- penguins |> mutate(bill_vol = bill_len * bill_dep)
tryout <- penguins |> semi_join(penguins_id, join_by(id == id))
#so now my "penguins_id" has the new column bill_vol as it fit
#penguins and penguins_id together using id

#pivot practice
hm <- penguins |> pivot_wider(names_from = species, values_from = body_mass)
#this doesnt work yet

## ----include = FALSE----------------------------------
source("setup.R")


## -----------------------------------------------------
#| echo: false
#| eval: false
## # divergingx_hcl(palette="Zissou 1", n=10)
## # [1] "#3B99B1" "#40ABA5" "#6FB798" "#9FC095" "#C7C98A"
## # [6] "#EABE23" "#E8A419" "#E78802" "#EA6400" "#F5191C"
## # specplot(divergingx_hcl(palette="Zissou 1", n=10))


## -----------------------------------------------------
#| echo: false
plan <- tribble(~time, ~topic,
"30",	"Determining which plot is the most effective",
"15", "Representing uncertainty",
"15",	"Managing multivariate data",
"20",	"Mapping spatial data"
)
knitr::kable(plan)


## -----------------------------------------------------
#| code-fold: true
#| code-summary: "some possible patterns"
# scatterplot with points spread everywhere
# histogram with bell-shape
# side-by-side boxplots with same median/box
# no difference between colour groups


## -----------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 4
library(nullabor)
set.seed(853)
ggplot(lineup(null_permute('mpg'), n=12,
    mtcars), aes(x=disp, y=mpg)) +
  geom_hline(yintercept = 20.1, 
    colour="grey80", linewidth=2) +
  geom_point(colour = "#3B99B1") +
  facet_wrap(~ .sample, ncol=4) +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))


## -----------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 4
library(nullabor)
set.seed(853)
ggplot(lineup(null_permute('mpg'), n=12,
    mtcars), aes(x=disp, y=mpg)) +
  geom_smooth(method="lm", colour = "grey80",
    se=FALSE, linewidth=2) +
  geom_point(colour = "#3B99B1") +
  facet_wrap(~ .sample, ncol=4) +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 3
set.seed(925)
d1 <- tibble(s = rep(1:76, 14), 
             x = rnorm(14*76))
d2 <- tibble(s = rep(1:76, 14), 
             x = rexp(14*76))
m1 <- d1 |>
  group_by(s) |>
  summarise(x = mean(x)) |>
  ggplot(aes(x=x)) +
    geom_histogram(breaks = seq(-0.6, 0.6, 0.1),
      colour="white") +
    xlab("sample mean") + ylab("") +
    theme(axis.text = element_blank())
m2 <- d2 |>
  group_by(s) |>
  summarise(x = mean(x)) |>
  ggplot(aes(x=x)) +
    geom_histogram(breaks = seq(0.7, 2, 0.1),
      colour="white")+
    xlab("sample mean") + ylab("") +
    theme(axis.text = element_blank())
m1 + m2 + plot_layout(ncol=2)


## -----------------------------------------------------
#| echo: false
#| eval: false
## library(readabs)
## Sys.setenv(R_READABS_PATH = "./data/")
## lfs_1 <- read_abs("6202.0", tables = 1)
## lfs_2023 <- lfs_1 |>
##   mutate(year = year(date)) |>
##   filter(year == 2023)
## lfs_2023_sub <- lfs_2023 |>
##   mutate(month = lubridate::month(date, label = TRUE)) |>
##   filter(series == "Unemployed total ;  Persons ;",
##          series_type == "Original")
## ggplot(data = lfs_2023_sub, aes(x=month, y=value,
##                      fill = month)) +
##   geom_col() +
##   theme_void() +
##   theme(legend.position = "none")
## ggplot(data = lfs_2023_sub, aes(x="",
##                      y=value,
##                      fill = month)) +
##   geom_bar(stat="identity", width=1, color="white") +
##   coord_polar("y", start=0) +
##   theme_void() +
##   theme(legend.position = "none")
## save(lfs_2023_sub, file="data/lfs_2023_sub.rda")


## -----------------------------------------------------
#| echo: false
load("data/lfs_2023_sub.rda")
lfs_2023_sub <- lfs_2023_sub |>
  mutate(month = as.numeric(month)) |>
  select(month, value) |>
  mutate(value = round(value, 0))
# Make pattern a bit stronger
lfs_2023_sub <- lfs_2023_sub |>
  mutate(value = ifelse(month %in% c(5, 6),
               round(value*0.9, 0), value))
ntot <- sum(lfs_2023_sub$value)
set.seed(1208)
sim <- sample(1:12, 
  ntot*11, replace=TRUE)
lfs_2023_sub_l <- tibble(
  .sample = rep(1:11, rep(ntot, 11)), 
  month = sim)
lfs_2023_sub_l <- lfs_2023_sub_l |>
  group_by(.sample) |>
  count(month) |>
  rename(value = n)
lfs_2023_sub_l <- bind_rows(bind_cols(.sample = 12,
  lfs_2023_sub), lfs_2023_sub_l)
pos <- sample(1:12)
lfs_2023_sub_l <- lfs_2023_sub_l |>
  mutate(.sample = pos[lfs_2023_sub_l$.sample])


## -----------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 6
#| out-width: 80%
ggplot(lfs_2023_sub_l, aes(x=6, y=400)) +
  geom_text(aes(label = .sample), size=5) +
  theme_void() +
  facet_wrap(~.sample, ncol=4) +
  theme( 
        panel.background =
         element_rect(fill='transparent', 
         color = "black"), 
        panel.border =
         element_rect(fill='transparent', 
         color = "black"))


## -----------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 6
#| out-width: 80%
ggplot(lfs_2023_sub_l, aes(x=month, y=value)) +
  geom_col(breaks=seq(0, 12, 1)) +
  theme_void() +
  facet_wrap(~.sample, ncol=4) +
  theme(legend.position = "none")


## -----------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 6
#| out-width: 80%
ggplot(data = lfs_2023_sub_l, aes(x="", 
                     y=value, 
                     fill = month)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  facet_wrap(~.sample, ncol=4) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = "none")


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
set.seed(1234)
n = 5000

df = tibble(
  .draw = 1:n,
  intercept = rnorm(n, 3, 1),
  slope = rnorm(n, 1, 0.25),
  x = list(-4:5),
  y = Map(function(x, y) x + y * -4:5, intercept, slope)
) |>
  unnest(c(x, y))
df |>
  filter(.draw %in% 1:100) |>
  ggplot(aes(x = x, y = y, group = .draw)) +
  geom_line(alpha = 0.25)


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
set.seed(1234)
n = 4000
mpg = seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 100)

mtcars_boot = tibble(
  .draw = 1:n,
  m = lapply(.draw, function(d) loess(
    hp ~ mpg,
    span = 0.9,
    # this lets us predict outside the range of the data
    control = loess.control(surface = "direct"),
    data = slice_sample(mtcars, prop = 1, replace = TRUE)
  )),
  hp = lapply(m, predict, newdata = tibble(mpg)),
  mpg = list(mpg)
) |>
  select(-m) |>
  unnest(c(hp, mpg))

mtcars_boot |>
  group_by(mpg) |>
  curve_interval(hp, .width = c(.5, .7, .9), .interval = "bd-mbd") |>
  ggplot(aes(x = mpg, y = hp)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
  geom_point(data = mtcars) +
  scale_fill_brewer() +
  coord_cartesian(ylim = c(0, 400))


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 8
library(palmerpenguins)
f_std <- function(x) (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
p_std <- penguins |>
  select(bill_length_mm:body_mass_g, species) |>
  dplyr::rename(bl = bill_length_mm,
         bd = bill_depth_mm,
         fl = flipper_length_mm,
         bm = body_mass_g) |>
  na.omit() |>
  dplyr::mutate(bl = f_std(bl),
         bd = f_std(bd),
         fl = f_std(fl),
         bm = f_std(bm)) 

highlight_key(p_std) |>
  ggpairs(columns = 1:4) |>
  ggplotly(width=800, height=800) |>
  highlight("plotly_selected")


## -----------------------------------------------------
#| echo: false
world_map <- map_data("world")
world_map |> 
  filter(region %in% c("Australia", "New Zealand")) |> 
      DT::datatable(width=1150, height=100)


## -----------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 12
#| out-width: 30%
oz <- world_map |> 
  filter(region == "Australia") |>
  filter(lat > -50)
m1 <- ggplot(oz, aes(x = long, y = lat)) + 
  geom_point(size=0.2) + 
  coord_map() +
  ggtitle("Points")
m2 <- ggplot(oz, aes(x = long, y = lat, 
               group = group)) + #<<
  geom_path() + 
  coord_map() +
  ggtitle("Path")
m3 <- ggplot(oz, aes(x = long, y = lat, 
               group = group)) + #<<
  geom_polygon(fill = "#607848", colour = "#184848") +
  coord_map() +
  ggtitle("Filled polygon")
m1 + m2 + m3 + plot_layout(ncol=1)


## -----------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 12
#| out-width: 30%
oz <- world_map |> 
  filter(region == "Australia") |>
  filter(lat > -50)
m1 <- ggplot(oz, aes(x = long, y = lat)) + 
  geom_point(size=0.2) + 
  coord_map() +
  ggtitle("Points")
m2 <- ggplot(oz, aes(x = long, y = lat, 
               group = group)) + #<<
  geom_path() + 
  coord_map() +
  ggtitle("Path")
m3 <- ggplot(oz, aes(x = long, y = lat, 
               group = group)) + #<<
  geom_polygon(fill = "#607848", colour = "#184848") +
  coord_map() +
  ggtitle("Filled polygon")
m1 + m2 + m3 + plot_layout(ncol=1)


## -----------------------------------------------------
#| echo: false
# Read the data
# Replace null with 0, for three LGAs
# Convert to long form to join with polygons
# Make the date variables a proper date
# Set NAs to 0, this is a reasonable assumption
covid <- read_csv("data/melb_lga_covid.csv") |>
  mutate(Buloke = as.numeric(ifelse(Buloke == "null", "0", Buloke))) |>
   mutate(Hindmarsh = as.numeric(ifelse(Hindmarsh == "null", "0", Hindmarsh))) |>
   mutate(Towong = as.numeric(ifelse(Towong == "null", "0", Towong))) |>
  pivot_longer(cols = Alpine:Yarriambiack, names_to="NAME", values_to="cases") |>
  mutate(Date = ydm(paste0("2020/",Date))) |>
  mutate(cases=replace_na(cases, 0))


## -----------------------------------------------------
#| echo: false

# Case counts are cumulative, keep only latest
covid <- covid |>
  filter(Date == ymd("2020-10-20"))


## -----------------------------------------------------
#| eval: false
#| echo: false
#| message: false
## # Read the LGA data from strayr package.
## # This has LGAs for all of Australia.
## # Need to filter out Victoria LGAs, avoiding LGAs
## # from other states with same name, and make the names
## # match covid data names. The regex equation is
## # removing () state and LGA type text strings
## # Good reference: https://r-spatial.github.io/sf/articles/sf1.html
## # remotes::install_github("runapp-aus/strayr")
## library(strayr)
## library(ggthemes)
## library(sf)
## lga <- strayr::read_absmap("lga2018") |>
##   rename(lga = lga_name_2018) |>
##   filter(state_name_2016 == "Victoria")
## save(lga, file="data/lga.rda")


## -----------------------------------------------------
#| echo: false
#| eval: false
## ggplot(lga) + geom_sf() + theme_map()
## 
## lga_sm <- ms_simplify(lga)
## save(lga_sm, file="data/lga_sm.rda")
## ggplot(lga_sm) + geom_sf() + theme_map()


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
load("data/lga.rda")

covid_tot <- covid |>
  left_join(lga, by=c("NAME" = "lga")) |>
  st_as_sf()

# Make choropleth map, with appropriate colour palette
cm1 <- ggplot(covid_tot) + 
  geom_sf(aes(fill = cases, label = NAME),
    colour="grey80") + 
  scale_fill_distiller("Cases", 
    palette = "PuBuGn",
    direction=1) + 
  theme_map() +
  theme(legend.position="bottom")
cm1
# Make it interactive
# plotly::ggplotly() 


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
pop <- read_xlsx("data/VIF2019_Population_Service_Ages_LGA_2036.xlsx", sheet=3, skip=13, col_names = FALSE) |>
  select(`...4`, `...22`) |>
  rename(lga = `...4`, pop=`...22`) |>
  filter(lga != "Unincorporated Vic") |> 
  mutate(lga = str_replace(lga, " \\(.+\\)", "")) |>
  mutate(lga = ifelse(lga == "Colac-Otway", "Colac Otway", lga)) 

covid_tot <- covid_tot |>
  left_join(pop, by=c("NAME" = "lga")) 

covid_tot <- covid_tot |>
  mutate(cases_per10k = cases/pop*10000,
         lcases = log10(cases + 1)) 

library(cartogram)
covid_tot_carto <- covid_tot |> 
  st_transform(3395) |> 
  cartogram_cont("pop") |>
  st_transform("WGS84")   
  
covid_tot_carto <- st_cast(covid_tot_carto, "MULTIPOLYGON") 

cm2 <- ggplot(covid_tot_carto) + 
  geom_sf(aes(fill = cases, label=NAME),
    colour="grey80") + 
  scale_fill_distiller("Cases", palette = "PuBuGn",
                       direction=1) + 
  theme_map() +
  theme(legend.position="bottom")  
cm2 


## -----------------------------------------------------
#| echo: false
# Placement of hexmaps depends on position relative to
# Melbourne central
data(capital_cities)
covid_hexmap <- create_hexmap(
  shp = covid_tot,
  sf_id = "NAME",
  focal_points = capital_cities, verbose = TRUE)
# This shows the centroids of the hexagons
#ggplot(covid_hexmap, aes(x=hex_long, y=hex_lat)) +
#  geom_point()

# Hexagons are made with the `fortify_hexagon` function
covid_hexmap_poly <- covid_hexmap |>
  fortify_hexagon(sf_id = "NAME", hex_size = 0.1869) |>
  left_join(covid_tot, by="NAME") # hexmap code removed cases!
cm3 <- ggplot() +
  geom_sf(data=covid_tot, 
          fill = "white", colour = "grey80", size=0.1) +
  geom_polygon(data=covid_hexmap_poly, 
               aes(x=long, y=lat, group=hex_id, 
                   fill = cases, 
                   colour = cases,
                   label=NAME), size=0.2) +
  scale_fill_distiller("Cases", palette = "PuBuGn",
                       direction=1) +
  scale_colour_distiller("Cases", palette = "PuBuGn",
                       direction=1) +
  theme_map() +
  theme(legend.position="bottom")
cm3
# ggplotly()


## -----------------------------------------------------
#| echo: false
cm1 <- cm1 + theme(legend.position = "none")
ggplotly(cm1, width=800, height=600) |>
  config(displayModeBar = FALSE)


## -----------------------------------------------------
#| echo: false
cm2 <- cm2 + theme(legend.position = "none")
ggplotly(cm2, width=800, height=600) |>
  config(displayModeBar = FALSE)


## -----------------------------------------------------
#| echo: false
cm3 <- cm3 + theme(legend.position = "none")
ggplotly(cm3, width=800, height=600) |>
  config(displayModeBar = FALSE)


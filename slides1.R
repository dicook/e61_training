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
plan <- tribble(~timing, ~topic,
"15",	"Organising your data for efficient plot descriptions",
"15", "Grammatical descriptions for plots",
"30", "Cognitive perception principles",
"15", "Polishing your plots",
"30", "Adding interactivity"
)
knitr::kable(plan)


## -----------------------------------------------------
#| label: tb-data
#| echo: false
tb <- read_csv("data/TB_notifications_2023-08-21.csv") |>
  filter(country == "Australia", year > 1996, year < 2013) |>
  select(year, contains("new_sp")) 
glimpse(tb)


## -----------------------------------------------------
#| eval: false
#| code-fold: true
#| code-summary: "variables are:"
## # year, sex, age category


## -----------------------------------------------------
#| label: tb-data
#| eval: false
#| class-source: code_block_short
#| classes: code_block_short

## tb <- read_csv("data/TB_notifications_2023-08-21.csv") |>
##   filter(country == "Australia", year > 1996, year < 2013) |>
##   select(year, contains("new_sp"))
## glimpse(tb)


## -----------------------------------------------------
#| label: tb-datatable
#| class-source: code_block_short
#| classes: code_block_short
tb_dt <- fread("data/TB_notifications_2023-08-21.csv")
tb_dt <- tb_dt[country == "Australia" & year > 1996 & year < 2013, 
  c(6, 26:44)]


## * Import the CSV file
## import delimited "data/TB_notifications_2023-08-21.csv", clear
## 
## * Filter for Australia and years after 1996
## keep if country == "Australia" & year > 1996 & year < 2013
## 
## * Keep only the year and variables containing "new_sp"
## ds year *new_sp*, has(varl)
## keep `r(varlist)'
## 
## * Display the structure of the data
## describe
## 
## * Show the first few observations
## list in 1/10

## -----------------------------------------------------
#| label: tb-tidy
#| echo: false
tb_tidy <- tb |>
  select(-new_sp, -new_sp_m04, -new_sp_m514, 
                  -new_sp_f04, -new_sp_f514) |> 
  pivot_longer(starts_with("new_sp"), 
    names_to = "sexage", 
    values_to = "count") |>
  mutate(sexage = str_remove(sexage, "new_sp_")) |>
  separate_wider_position(
    sexage,
    widths = c(sex = 1, age = 4),
    too_few = "align_start"
  ) |>
  filter(age != "u") |>
  mutate(age = fct_recode(age, "0-14" = "014",
                          "15-24" = "1524",
                          "15-24" = "1524",
                          "25-34" = "2534",
                          "35-44" = "3544",
                          "45-54" = "4554",
                          "55-64" = "5564",
                          "> 65" = "65"))
tb_tidy |> slice_head(n=12)


## -----------------------------------------------------
#| label: tb-tidy
#| eval: false
#| class-source: code_block_short
#| classes: code_block_short

## tb_tidy <- tb |>
##   select(-new_sp, -new_sp_m04, -new_sp_m514,
##                   -new_sp_f04, -new_sp_f514) |>
##   pivot_longer(starts_with("new_sp"),
##     names_to = "sexage",
##     values_to = "count") |>
##   mutate(sexage = str_remove(sexage, "new_sp_")) |>
##   separate_wider_position(
##     sexage,
##     widths = c(sex = 1, age = 4),
##     too_few = "align_start"
##   ) |>
##   filter(age != "u") |>
##   mutate(age = fct_recode(age, "0-14" = "014",
##                           "15-24" = "1524",
##                           "15-24" = "1524",
##                           "25-34" = "2534",
##                           "35-44" = "3544",
##                           "45-54" = "4554",
##                           "55-64" = "5564",
##                           "> 65" = "65"))
## tb_tidy |> slice_head(n=12)


## -----------------------------------------------------
#| label: tb-tidy-dt
#| eval: false
#| class-source: code_block_short
#| classes: code_block_short
##  tb_dt_tidy <- tb_dt |>
##    melt(id.vars = "year") |>
## 
## 


## * Drop specified variables
## drop new_sp new_sp_m04 new_sp_m514 new_sp_f04 new_sp_f514
## 
## * Reshape data from wide to long format
## reshape long new_sp_, i(year) j(sexage) string
## 
## * Rename reshaped variable
## rename new_sp_ count
## 
## * Remove "new_sp_" prefix from sexage
## replace sexage = subinstr(sexage, "new_sp_", "", .)
## 
## * Separate sexage into sex and age
## gen sex = substr(sexage, 1, 1)
## gen age = substr(sexage, 2, .)
## 
## * Drop original sexage variable
## drop sexage
## 
## * Recode age variable
## replace age = "0-14" if age == "014"
## replace age = "15-24" if age == "1524"
## replace age = "25-34" if age == "2534"
## replace age = "35-44" if age == "3544"
## replace age = "45-54" if age == "4554"
## replace age = "55-64" if age == "5564"
## replace age = "> 65" if age == "65"
## replace age = "unknown" if age == "u"
## 
## * Convert age to a labeled factor variable
## encode age, gen(age_factor)
## 
## * List the first few observations to check the result
## list in 1/10

## -----------------------------------------------------
#| label: wdi
#| echo: false
wdi <- read_xlsx("data/P_Data_Extract_From_World_Development_Indicators.xlsx")
glimpse(wdi, width=55)


## -----------------------------------------------------
#| label: weather
#| echo: false
melb <- read.fwf(here::here("data/ASN00086282.dly"), 
   c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), fill=T)
glimpse(melb, width=55)


## -----------------------------------------------------
#| label: tb-plots
#| echo: true
#| eval: false
#| class-source: code_block_med
#| classes: code_block_med
#| code-line-numbers: "5,6,9,10,11"
## tb_yr <- tb_tidy |>
##   group_by(year) |>
##   summarise(count = sum(count, na.rm=TRUE))
## gg1 <- ggplot(tb_yr,
##   aes(x=year, y=count)) +
##   geom_col() +
##   ylim(c(0, 350))
## gg2 <- ggplot(tb_yr,
##   aes(x=year, y=count)) +
##   geom_point() +
##   geom_smooth(se=F) +
##   ylim(c(0, 350))
## gg1 + gg2 + plot_layout(ncol=1)


## * Collapse data to get yearly totals
## collapse (sum) count, by(year)
## 
## * Generate column plot
## graph bar (asis) count, over(year) ///
##     title("TB Cases by Year") ///
##     ytitle("Count") ///
##     name(g1, replace)
## 
## * Generate scatter plot with smoothed line
## twoway (scatter count year) ///
##        (lowess count year), ///
##     title("TB Cases by Year") ///
##     ytitle("Count") ///
##     name(g2, replace)
## 
## * Combine the two graphs vertically
## graph combine g1 g2, col(1) ysize(10)
## 

## -----------------------------------------------------
#| label: tb-plots
#| echo: false
#| fig-width: 5
#| fig-height: 9
#| out-width: 50%
tb_yr <- tb_tidy |>
  group_by(year) |>
  summarise(count = sum(count, na.rm=TRUE)) 
gg1 <- ggplot(tb_yr, 
  aes(x=year, y=count)) +
  geom_col() +
  ylim(c(0, 350))
gg2 <- ggplot(tb_yr, 
  aes(x=year, y=count)) +
  geom_point() +
  geom_smooth(se=F) +
  ylim(c(0, 350))
gg1 + gg2 + plot_layout(ncol=1)


## -----------------------------------------------------
#| echo: false
#| out-width: 70%
library(nullabor)
data(electoral)
ggplot(electoral$polls, 
       aes(x=Democrat, 
           y=Margin)) +
  geom_boxplot() +
  theme(aspect.ratio = 1.2, 
        panel.grid.major.x = element_blank())


## -----------------------------------------------------
#| eval: false
#| code-fold: true
#| code-summary: answer
## x = Democrat
## y = Margin
## geom = boxplot


## -----------------------------------------------------
#| fig-width: 5
#| fig-height: 3
#| echo: false
tb_tidy |>
  filter(age %in% c("45-54", "55-64"),
         sex == "f") |>
  ggplot(mapping=aes(x=year, 
                 y=count)) + 
  geom_smooth(aes(colour=age), se=F, method="lm") +
  scale_color_discrete_divergingx(palette="Geyser") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 4), 
    labels = c("98", "02", "06", "10")) +
  theme(aspect.ratio = 0.8, 
    axis.text = element_text(size="10"))


## -----------------------------------------------------
#| eval: false
#| code-fold: true
#| code-summary: answer
## x = year
## y = count
## colour: age
## geom = lm


## -----------------------------------------------------
#| echo: false
tb_bad <- tb_tidy |>
  pivot_wider(names_from = "sex", values_from = "count")
tb_bad |>
  slice_head(n=10)


## -----------------------------------------------------
#| eval: false
## tb_bad |>
##   ggplot() +
##     geom_point(aes(x=year, y=m), colour = "#A39000") +
##     geom_point(aes(x=year, y=f), colour = "#93B3FE")


## * Create the scatter plot
## twoway (scatter m year, mcolor("#A39000") msymbol(O)) ///
##        (scatter f year, mcolor("#93B3FE") msymbol(O)), ///
##        legend(order(1 "Male" 2 "Female")) ///
##        title("TB Cases by Year and Gender") ///
##        xtitle("Year") ytitle("Number of Cases")

## -----------------------------------------------------
#| echo: false
tb_tidy |>
  slice_head(n=10)


## -----------------------------------------------------
#| eval: false
## tb_tidy |>
##   ggplot() +
##     geom_point(aes(x=year, y=count, colour=sex))


## * Encode the sex variable if it's not already numeric
## encode sex, generate(sex_num)
## 
## * Create a custom color scheme
## colorpalette tableau, nograph
## local colors `r(p)'
## 
## * Create the scatter plot
## twoway (scatter count year if sex == "m", mcolor("`r(p1)'") msymbol(O)) ///
##        (scatter count year if sex == "f", mcolor("`r(p2)'") msymbol(O)), ///
##        legend(order(1 "Male" 2 "Female")) ///
##        title("TB Cases by Year and Sex") ///
##        xtitle("Year") ytitle("Number of Cases")

## -----------------------------------------------------
#| echo: false
vis_spacing <- 'style="padding-left:20px;"'
vis_spacing1 <- 'style="padding-left:10px;"'


## -----------------------------------------------------
#| label: proximity1
#| fig-width: 7
#| fig-height: 5
#| echo: false
tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x=year, 
           y=count, 
           colour=sex)) + 
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~age, ncol = 3) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 2), 
    labels = c("98", "00", "02", "04", "06", "08", "10", "12")) +
  theme(axis.text = element_text(size="10")) +
  ggtitle("Arrangement A")


## -----------------------------------------------------
#| label: proximity2
#| fig-width: 7
#| fig-height: 5
#| echo: false
tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x = year, 
             y = count, 
             colour = age)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~sex, ncol = 2) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 2), 
    labels = c("98", "00", "02", "04", "06", "08", "10", "12")) +
  theme(axis.text = element_text(size="10")) +
  ggtitle("Arrangement B")


## -----------------------------------------------------
#| label: cblind1
#| fig-width: 5
#| fig-height: 3.5
#| echo: false
tb_tidy |>
  filter(age %in% c("45-54", "55-64"),
         sex == "f") |>
  ggplot(mapping=aes(x=year, 
                 y=count)) + 
  geom_point() +
  geom_smooth(aes(colour=age), se=F, method="lm") +
  facet_wrap(~age, ncol = 2) +
  scale_color_discrete_divergingx(palette="Geyser") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 4), 
    labels = c("98", "02", "06", "10")) +
  theme(legend.position="none",
        axis.text = element_text(size="10"))
  


## -----------------------------------------------------
#| label: cblind2
#| fig-width: 3
#| fig-height: 3
#| out-width: 60%
#| echo: false
tb_tidy |>
  filter(age %in% c("45-54", "55-64"),
         sex == "f") |>
  ggplot(mapping=aes(x=year, 
                 y=count)) + 
  geom_smooth(aes(colour=age), se=F, method="lm") +
  scale_color_discrete_divergingx(palette="Geyser") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 4), 
    labels = c("98", "02", "06", "10")) +
  theme(legend.position="none",
        axis.text = element_text(size="10"))
  


## -----------------------------------------------------
#| label: olives-data
#| include: false
data(olives, package = "classifly")
df2 <- olives |>
  mutate(Region = factor(Region, labels = c("South", "Sardinia", "North")))


## -----------------------------------------------------
#| label: color-olives
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 80%
ggplot(olives, aes(palmitoleic, palmitic, color = Area)) +
  geom_point() +
  scale_color_discrete_divergingx(palette="Zissou 1") 


## -----------------------------------------------------
#| label: no-shadow
#| echo: false
#| fig-width: 7
#| fig-height: 7
#| out-width: 60%
ggplot(olives, aes(palmitoleic, palmitic, color = Area)) +
  geom_point() +
  facet_wrap(~Area) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  guides(color = FALSE) 


## -----------------------------------------------------
#| label: shadow
#| echo: false
#| fig-width: 7
#| fig-height: 7
#| out-width: 60%
ggplot(olives, aes(palmitoleic, palmitic)) +
  geom_point(data = dplyr::select(olives, -Area), color = "gray80") +
  geom_point(aes(color = Area), size=2) +
  facet_wrap(~Area) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  guides(color = FALSE)


## -----------------------------------------------------
#| label: shape
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
set.seed(209)
df <- data.frame(
  x=runif(100), 
  y=runif(100), 
  cl=sample(c(rep("A", 1), rep("B", 99))))
ggplot(data=df, aes(x, y, shape=cl)) + 
  geom_point(size=3, alpha=0.8) +
  theme(legend.position="None", aspect.ratio=1)


## ----is color preattentive, echo=FALSE----------------
#| label: colour
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
set.seed(454)
df <- data.frame(
  x=runif(100), 
  y=runif(100), 
  cl=sample(c(rep("A", 1), rep("B", 99))))
ggplot(data=df, aes(x, y, colour=cl)) + 
  geom_point(size=3, alpha=0.8) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.position="None", aspect.ratio=1)


## -----------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 1
V1 = tibble(x = 1:7, 
            native = factor(c("quoll", "emu", "roo", 
            "bilby", "quokka", "dingo", "numbat")))
ggplot(V1, aes(x=x, y=1, fill=native)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=native)) +
  ggtitle("qualitative") + 
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #axis.line = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))


## -----------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 1
V2 = tibble(x = 1:7, 
            fill = 1:7)
ggplot(V2, aes(x=x, y=1, fill=fill)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=fill)) +
  ggtitle("sequential: emphasise high") + 
  scale_fill_continuous_sequential(palette = "PinkYl") +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #axis.line = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))


## -----------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 1
V3 = tibble(x = 1:7, 
            fill = -3:3)
ggplot(V3, aes(x=x, y=1, fill=fill)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=fill)) +
  ggtitle("diverging: emphasise high and low") + 
  scale_fill_continuous_divergingx(palette = "ArmyRose") +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #axis.line = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
library(fable)
library(tsibble)
tourism_melb <- tourism |>
  filter(Region == "Melbourne")
fit <- tourism_melb |>
  model(
    ets = ETS(Trips ~ trend("A"))
  )
fc <- fit |>
  forecast(h = "5 years")
fc |>
  filter(Purpose == "Business") |>
  autoplot(tourism_melb) +
  ggtitle("Melbourne Business Trips") +
  theme(aspect.ratio = 0.5)


## -----------------------------------------------------
#| echo: false
#| fig-width: 5.5
#| fig-height: 5
#| out-width: 70%
library(vital)
library(viridis)
am <- aus_mortality |> 
  filter(State == "Victoria", 
         Sex != "total", 
         Year < 1980, 
         Age < 90) 

ggplot(am, aes(x=Age, y=Mortality, colour=Year, group=Year)) + 
    geom_line() +
    facet_wrap(~Sex, ncol=1) +
    scale_color_gradientn(colours = rainbow(10)) +
    scale_y_log10() + 
    theme(aspect.ratio = 0.5)


## -----------------------------------------------------
#| echo: false
#| fig-width: 5.5
#| fig-height: 5
#| out-width: 70%
ggplot(am, aes(x=Age, y=Mortality, colour=Year, group=Year)) + 
    geom_line() +
    facet_wrap(~Sex, ncol=1) +
    scale_colour_gradientn(colors = viridis_pal(option = "turbo")(10)) +
    scale_y_log10() + 
    theme(aspect.ratio = 0.5)


## -----------------------------------------------------
#| echo: false
#| fig-width: 5.5
#| fig-height: 5
#| out-width: 70%

ggplot(am, aes(x=Age, y=Mortality, colour=Year, group=Year)) + 
    geom_line() +
    facet_wrap(~Sex, ncol=1) +
    scale_color_gradientn(colours = deutan(rainbow(10))) +
    scale_y_log10() + 
    theme(aspect.ratio = 0.5)


## -----------------------------------------------------
#| echo: false
#| fig-width: 5.5
#| fig-height: 5
#| out-width: 70%
ggplot(am, aes(x=Age, y=Mortality, colour=Year, group=Year)) + 
    geom_line() +
    facet_wrap(~Sex, ncol=1) +
    scale_colour_gradientn(colors = deutan(viridis_pal(option = "turbo")(10))) +
    scale_y_log10() + 
    theme(aspect.ratio = 0.5)


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
#| out-width: 80%
ggplot(as_tibble(Titanic), aes(x=interaction(Sex, Age),
                               y=interaction(Class, Survived), 
                               fill=n)) +
  geom_tile() +
  xlab("Sex, Age") +
  ylab("Class, Survived") +
  scale_fill_continuous_sequential(palette = "Terrain")


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
#| out-width: 80%
ggplot(as_tibble(Titanic), aes(x=interaction(Sex, Age),
                               y=interaction(Class, Survived), 
                               fill=n)) +
  geom_tile() +
  xlab("Sex, Age") +
  ylab("Class, Survived") +
  scale_fill_continuous_sequential(palette = "Terrain", trans="sqrt")


## -----------------------------------------------------
#| echo: false
#| eval: false
## # remotes::install_github("kevinwang09/learningtower")
## library(learningtower)
## student_data_2018 <- load_student(2018)
## student_means <- student_data_2018 |>
##   group_by(country) |>
##   summarise(math = mean(math, na.rm=TRUE),
##             read = mean(read, na.rm=TRUE),
##             science = mean(science, na.rm=TRUE))
## save(student_data_2018,
##   file="data/student_data_2018.rda")
## save(student_means, file="data/student_means.rda")
## 
## # Compute differences and bootstrap
## student2018_stats <- student_data_2018 %>%
##   group_by(country) %>%
##   summarise(mathgap=mean(math[gender=="male"],
##                            na.rm=TRUE)-
##                     mean(math[gender=="female"],
##                            na.rm=TRUE),
##             wmathgap=weighted.mean(
##                     math[gender=="male"],
##                       w=stu_wgt[gender=="male"],
##                         na.rm=T)-
##                      weighted.mean(
##                     math[gender=="female"],
##                       w=stu_wgt[gender=="female"],
##                         na.rm=T),
##             readgap=mean(read[gender=="male"],
##                            na.rm=TRUE)-
##                     mean(read[gender=="female"],
##                            na.rm=TRUE),
##             wreadgap=weighted.mean(
##                     read[gender=="male"],
##                       w=stu_wgt[gender=="male"],
##                         na.rm=T)-
##                      weighted.mean(
##                     read[gender=="female"],
##                       w=stu_wgt[gender=="female"],
##                         na.rm=T))
## save(student2018_stats, file="data/student2018_stats.rda")
## 
## library(boot)
## cimathfn <- function(d, i) {
##   x <- d[i,]
##   if (nrow(x) == 0) {
##     ci <- 0
##   }
##   else {
##     ci <- weighted.mean(x$math[x$gender=="male"],
##        w=x$stu_wgt[x$gender=="male"], na.rm=T)-
##      weighted.mean(x$math[x$gender=="female"],
##         w=x$stu_wgt[x$gender=="female"], na.rm=T)
##   }
##   ci
## }
## cireadfn <- function(d, i) {
##   x <- d[i,]
##   if (nrow(x) == 0) {
##     ci <- 0
##   }
##   else {
##     ci <- weighted.mean(x$read[x$gender=="male"],
##        w=x$stu_wgt[x$gender=="male"], na.rm=T)-
##      weighted.mean(x$read[x$gender=="female"],
##         w=x$stu_wgt[x$gender=="female"], na.rm=T)
##   }
##   ci
## }
## bootmathfn <- function(d) {
##   if (nrow(d) == 0) {
##     ci <- c(0, 0)
##   }
##   else {
##     r <- boot(d, statistic=cimathfn, R=200)
##     l <- sort(r$t)[5]
##     u <- sort(r$t)[195]
##     ci <- c(l, u)
##   }
##   return(ci)
## }
## bootreadfn <- function(d) {
##   if (nrow(d) == 0) {
##     ci <- c(0, 0)
##   }
##   else {
##     r <- boot(d, statistic=cireadfn, R=200)
##     l <- sort(r$t)[5]
##     u <- sort(r$t)[195]
##     ci <- c(l, u)
##   }
##   return(ci)
## }
## math_results <- student_data_2018 %>%
##   split(.$country) %>%
##   purrr::map(bootmathfn)
## cnt <- names(math_results)
## math_results_tb <- tibble(country = rep(cnt, rep(2, length(cnt))),
##             ci = rep(c("l", "u"), length(cnt)),
##             value=unlist(math_results))
## math_results_tb <- math_results_tb |>
##   pivot_wider(names_from = ci, values_from = value) |>
##   filter(!(l == 0 & u == 0))
## read_results <- student_data_2018 %>%
##   split(.$country) %>%
##   purrr::map(bootreadfn)
## cnt <- names(read_results)
## read_results_tb <- tibble(country = rep(cnt, rep(2, length(cnt))),
##             ci = rep(c("l", "u"), length(cnt)),
##             value=unlist(read_results))
## read_results_tb <- read_results_tb |>
##   pivot_wider(names_from = ci, values_from = value) |>
##   filter(!(l == 0 & u == 0))
## save(math_results_tb,
##   file="data/math_results_tb.rda")
## save(read_results_tb,
##   file="data/read_results_tb.rda")


## -----------------------------------------------------
#| echo: false
#| fig-height: 6
#| fig-width: 4
#| out-width: 70%
load("data/student_means.rda")
student_means_sub <- student_means |>
  filter(country %in% c("SGP", "KOR", "POL", "DEU", "NOR", "IRL", "GBR", "IDN", "AUS", "NZL", "USA", "TUR", "PHL", "MAR", "URY", "CHL", "COL", "CAN"))
ggplot(student_means_sub, aes(x=country, y=math)) + 
  geom_point(colour="#8ACE00", size=4) + 
  coord_flip() +
  xlab("") +
  theme(aspect.ratio = 2)


## -----------------------------------------------------
#| echo: false
#| fig-height: 6
#| fig-width: 4
#| out-width: 70%
ggplot(student_means_sub, aes(x=country, y=math)) + 
  geom_point(colour="#8ACE00", size=4) + 
  coord_flip() +
  xlab("") +
  ylim(c(0, 1000)) +
  theme(aspect.ratio = 2)


## -----------------------------------------------------
#| echo: false
#| fig-height: 6
#| fig-width: 4
#| out-width: 70%
ggplot(student_means_sub, aes(x=fct_reorder(country, math), 
                          y=math)) + 
  geom_point(colour="#8ACE00", size=4) + 
  coord_flip() +
  xlab("") +
  ylim(c(0, 1000)) +
  theme(aspect.ratio = 2)


## -----------------------------------------------------
#| label: trade-data
#| include: false
#| echo: false
data(EastIndiesTrade, package = "GDAdata")
skimr::skim(EastIndiesTrade)


## -----------------------------------------------------
#| label: trade-plot1
#| echo: false
#| fig-height: 4
#| fig-width: 6
#| out-width: 80%
ggplot(EastIndiesTrade, aes(Year, Exports)) +
  geom_line(color = "#008A25", size = 2) +
  geom_line(aes(Year, Imports), color = "#e6005c", size = 2) +
  geom_ribbon(aes(ymin = Exports, ymax = Imports), fill = "gray") +
  labs(y = "<span style='color:#008A25'>Export</span>/<span style='color:#e6005c'>Import</span>") +
  theme(aspect.ratio=0.7, axis.title.y = ggtext::element_markdown())


## -----------------------------------------------------
#| label: trade-plot2
#| echo: false
#| fig-height: 4
#| fig-width: 6
#| out-width: 80%
ggplot(EastIndiesTrade, aes(Year, Imports - Exports)) +
  geom_line(size = 2) +
  theme(aspect.ratio=0.7)


## -----------------------------------------------------
#| fig-width: 10
#| fig-height: 4
#| echo: false
data(anorexia, package="MASS")
ggplot(data=anorexia, 
 aes(x=Prewt, y=Postwt, 
	colour=Treat)) + 
 coord_equal() +
 xlim(c(70, 110)) + ylim(c(70, 110)) +
 xlab("Pre-treatment weight (lbs)") +  
 ylab("Post-treatment weight (lbs)") +
 geom_abline(intercept=0, slope=1,  
   colour="grey80", linewidth=1.25) + 
 geom_density2d() + 
 geom_point(size=3) +
 facet_grid(.~Treat) +
 theme(legend.position = "none")


## -----------------------------------------------------
#| fig-width: 10
#| fig-height: 4
#| echo: false
ggplot(data=anorexia, 
  aes(x=Prewt, colour=Treat,
    y=(Postwt-Prewt)/Prewt*100)) + 
  xlab("Pre-treatment weight (lbs)") +  
  ylab("Percent increase in weight") +
  geom_hline(yintercept=0, linewidth=1.25, 
    colour="grey80") + 
  geom_point(size=3) +   
  facet_grid(.~Treat) +
 theme(legend.position = "none")


## -----------------------------------------------------
#| echo: false
#| fig-width: 10
#| fig-height: 4
#| out-width: 80%
tb_aus_idn <- read_csv("data/TB_notifications_2023-08-21.csv") |>
  filter(iso3 %in% c("AUS", "IDN", "KOR")) |> 
  select(year, iso3, c_newinc) |>
  pivot_wider(names_from = iso3, values_from = c_newinc) |>
  mutate_at(vars(AUS:KOR), function(x) x/max(x, na.rm=TRUE)) |>
  pivot_longer(AUS:KOR, names_to = "iso3", 
    values_to = "rel_count")

tb_aus_idn |>
    ggplot(aes(x=year, y=rel_count)) +
      geom_hline(yintercept = 0, colour = "grey70") +
      geom_point() +
      geom_smooth(se=F, colour = "#E87700") +
      xlab("") + ylab("Relative count") +
      ggtitle("A") +
      facet_wrap(~iso3) +
      theme(aspect.ratio = 0.9)


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 4
#| out-width: 80%
tb_aus_idn |>
    ggplot(aes(x=year, y=rel_count, fill=iso3)) +
      geom_col(position = "dodge") +
  scale_fill_discrete_divergingx(palette = "Zissou 1") +
  xlab("") + ylab("Relative count") +
  ggtitle("B") +
  theme(aspect.ratio = 0.5, 
        legend.title = element_blank())


## -----------------------------------------------------
#| label: electoral
#| echo: false
#| out-width: 70%
library(nullabor)
data(electoral)
ggplot(electoral$polls, 
       aes(x=Democrat, 
           y=Margin)) +
  geom_boxplot() +
  theme(aspect.ratio = 1.2, 
        panel.grid.major.x = element_blank())


## -----------------------------------------------------
#| label: electoral-answers
#| echo: false
#| eval: false
## ggplot(electoral$polls,
##        aes(x=1,
##            y=Margin,
##            colour=Democrat)) +
##   geom_point()
## 
## ggplot(electoral$polls,
##        aes(x=Margin,
##            fill=Democrat)) +
##   geom_histogram()


## -----------------------------------------------------
#| code-fold: true
#| code-summary: "Some answers"
# 1: soften grid, resolve axis overlap
# 2: put line over points
# 3: remove repeats '000
# 4: aspect ratio=1 to read association
# 5: re-map variables


## -----------------------------------------------------
#| label: styling1
#| fig-width: 7
#| fig-height: 5
#| out-width: 80%
#| echo: false
tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x=year, 
           y=count, 
           colour=sex)) + 
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~age, ncol = 3) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 1)) +
  theme(axis.text = element_text(size="10"),
        panel.grid.major = element_line(color="black")) 


## -----------------------------------------------------
#| label: styling2
#| fig-width: 7
#| fig-height: 5
#| out-width: 80%
#| echo: false
tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x=year, 
           y=count, 
           colour=sex)) + 
  geom_smooth(se=F) +
  geom_point() +
  facet_wrap(~age, ncol = 3) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 2), 
    labels = c("98", "00", "02", "04", "06", "08", "10", "12")) +
  theme(axis.text = element_text(size="10",
                    color="grey80"),
        axis.title = element_text(size="10",
                    color="grey80"),
        panel.grid.major =   
           element_line(color="white"),
        panel.background =
           element_rect(fill="grey90", 
                        colour = "grey80")) 


## -----------------------------------------------------
#| label: styling3
#| fig-width: 7
#| fig-height: 5
#| out-width: 70%
#| echo: false
gapminder |> 
  filter (year == 2007) |>
  ggplot(aes(x=lifeExp, 
             y=gdpPercap,
             label=country,
             colour=continent)) +
  geom_point() +
  scale_colour_discrete_divergingx(palette = "Zissou 1")


## -----------------------------------------------------
#| label: styling4
#| fig-width: 7
#| fig-height: 5
#| out-width: 80%
#| echo: false
gapminder |> 
  filter (year == 2007) |>
  ggplot(aes(x=lifeExp, 
             y=gdpPercap,
             label=country)) +
  geom_point(colour = "#3B99B1") +
  scale_y_log10("gdpPercap ('000)",
                breaks = seq(0, 50000, 10000), 
                labels = seq(0, 50, 10)) +
  theme(aspect.ratio = 0.5)


## -----------------------------------------------------
#| label: styling5
#| fig-width: 7
#| fig-height: 5
#| out-width: 80%
#| echo: false
gapminder |> 
  filter (year == 2007) |>
  ggplot(aes(x=as.numeric(country),
             y=gdpPercap,
             fill=lifeExp)) +
  geom_col() + xlab("country") +
  scale_fill_distiller(palette = "RdPu", trans="log10") +    
  scale_y_log10("gdpPercap ('000)",
                breaks = seq(0, 50000, 10000), 
                labels = seq(0, 50, 10)) +
  theme(aspect.ratio = 0.5)


## -----------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 1
clrs <- divergingx_hcl(palette="Zissou 1", n=7)
ggplot(V1, aes(x=x, y=1, fill=native)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=native)) +
  scale_fill_manual(values = clrs) +
  ggtitle("original") + 
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))


## -----------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 1
clrs <- divergingx_hcl(palette="Zissou 1", n=7)
ggplot(V1, aes(x=x, y=1, fill=native)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=native)) +
  scale_fill_manual(values = deutan(clrs)) +
  ggtitle("deuteranope (green cone cells defective)") + 
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
library(gapminder)
gp <- gapminder |> 
  filter (year == 2007) |>
  ggplot(aes(x=lifeExp, 
             y=gdpPercap,
             label=country,
             colour=continent)) +
  geom_point() +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  scale_y_log10("gdpPercap ('000)",
                breaks = seq(0, 50000, 10000), 
                labels = seq(0, 50, 10)) +
  theme(axis.title = element_text(family="Helvetica"),
        axis.text = element_text(family="Helvetica"),
        legend.title = element_text(family="Helvetica"),
        legend.text = element_text(family="Helvetica"))
gp + geom_text()



## -----------------------------------------------------
#| echo: false
ggplotly(gp, width=700, height=550) |>
  config(displayModeBar = FALSE)


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
set.seed(802)
gapminder_ts <- gapminder |>
  as_tsibble(index=year, key=c(country, continent)) |>
  sample_n_keys(50)
gphk <- highlight_key(gapminder_ts, ~country)

gpl <- ggplot(gphk, aes(x=year, 
                             y=lifeExp, 
                             group=country)) +
        geom_line() +
        ylab("Life Expectancy") +
        ggtitle("Click on a line to highlight a country") +
        theme(axis.title = element_text(family="Helvetica"),
          axis.text = element_text(family="Helvetica"),
          legend.title = element_text(family="Helvetica"),
          legend.text = element_text(family="Helvetica"),
          title = element_text(family="Helvetica"))

ggpl <- ggplotly(gpl, height = 800, width = 1600) |>
  config(displayModeBar = FALSE)
        
highlight(ggpl)
        


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
gggs <- ggplot(gphk, aes(x=continent, y=country)) +
  geom_point() +
  xlab("") + ylab("") +
  theme(axis.title = element_text(family="Helvetica"),
      axis.text = element_text(family="Helvetica"),
      legend.title = element_text(family="Helvetica"),
      legend.text = element_text(family="Helvetica"),
      title = element_text(family="Helvetica"))

gggspl <- ggplotly(gggs, width=500, height=500) |>
  config(displayModeBar = FALSE) |>
  highlight(on = "plotly_selected", 
            off = "plotly_doubleclick") 

ggpl2 <- ggplotly(gpl, height = 500, width = 1000) |>
  config(displayModeBar = FALSE) |>
  highlight(on = "plotly_selected", 
            off = "plotly_doubleclick") 
  
bscols(gggspl, ggpl2, widths = c(5, 7))


## -----------------------------------------------------
#| echo: false
#| fig-width: 10
#| fig-height: 8
gpl <- ggplot(gphk, aes(x=year, 
                             y=lifeExp, 
                             group=country)) +
        geom_line() +
        ylab("Life Expectancy") + xlab("") +
        theme(axis.title = element_text(family="Helvetica"),
          axis.text = element_text(family="Helvetica"),
          legend.title = element_text(family="Helvetica"),
          legend.text = element_text(family="Helvetica"),
          title = element_text(family="Helvetica"))
ggpl2 <- ggplotly(gpl, height = 500, width = 1000) |>
  config(displayModeBar = FALSE)
  
bscols(widths = c(4, 7),
    filter_select("country", "country", gphk, ~country, multiple = TRUE),
  ggpl2)
  


## -----------------------------------------------------
#| echo: false
#| fig-width: 12
#| fig-height: 4
library(gapminder)
library(gganimate)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = country_colors, guide=FALSE) +
  scale_size("Population size", range = c(2, 12), breaks=c(1*10^8, 2*10^8, 5*10^8, 10^9, 2*20^9)) +
  scale_x_log10() +
  facet_wrap(~continent, ncol=5) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  gganimate::transition_time(year) +
  gganimate::ease_aes('linear')


## ----include = FALSE------------------------------------------
source("setup.R")


## -------------------------------------------------------------
#| echo: false
#| eval: false
## # divergingx_hcl(palette="Zissou 1", n=10)
## # [1] "#3B99B1" "#40ABA5" "#6FB798" "#9FC095" "#C7C98A"
## # [6] "#EABE23" "#E8A419" "#E78802" "#EA6400" "#F5191C"
## # specplot(divergingx_hcl(palette="Zissou 1", n=10))


## -------------------------------------------------------------
#| echo: false
plan <- tribble(~time, ~topic,
"15",	"Initial data analysis",
"30", "Exploring data",
"15",	"Constructing null samples",
"20", "Wrap-up: questions, discussion, other topics"
)
knitr::kable(plan)


## -------------------------------------------------------------
#| echo: false
wdi <- read_xlsx(here::here("data/P_Data_Extract_From_World_Development_Indicators.xlsx"), n_max = 4788)
wdi_tidy <- wdi |>
  select(`Country Code`, `Series Code`, `2004 [YR2004]`:`2022 [YR2022]`) |>
    rename_all(janitor::make_clean_names) |>
  pivot_longer(x2004_yr2004:x2022_yr2022,
               names_to = "year", 
               values_to = "value") |>
  mutate(year = as.numeric(str_sub(year, 2, 5)),
         value = as.numeric(value)) 

options(width=75)
wdi_tidy |> glimpse()



## -------------------------------------------------------------
#| echo: false
#| fig-width: 7
#| fig-height: 8
#| out-width: 50%
wdi_tidy |>
  pivot_wider(names_from = series_code,
              values_from = value) |>
  vis_miss(sort_miss = TRUE)


## -------------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 50%
wdi_tidy |>
  pivot_wider(names_from = series_code,
              values_from = value) |>
  gg_miss_var(show_pct=TRUE) + xlab("")


## -------------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 7
#| out-width: 40%
set.seed(239)
country_sub <- wdi_tidy |> 
  select(country_code) |>
  distinct() |>
  pull(country_code)
country_sub <- sample(country_sub, 50)  
wdi_tidy |>
  filter(country_code %in% country_sub) |>
  pivot_wider(names_from = country_code,
              values_from = value) |>
  gg_miss_var(show_pct=TRUE) + xlab("") 


## -------------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 60%
wdi_tidy |>
  pivot_wider(names_from = year,
              values_from = value) |>
  gg_miss_var(show_pct=TRUE) + xlab("")


## -------------------------------------------------------------
#| echo: false
#| fig-width: 7
#| fig-height: 8
#| out-width: 70%
wdi |>
  select(`Country Code`, `Series Code`, `2004 [YR2004]`:`2022 [YR2022]`) |>
    rename_all(janitor::make_clean_names) |>
  vis_miss(sort_miss = TRUE)


## -------------------------------------------------------------
#| echo: false
load("data/student_data_2018.rda")
options(width=50)
glimpse(student_data_2018)


## -------------------------------------------------------------
#| code-fold: true
#| code-summary: "some things"
# Math gap
# More books higher score


## -------------------------------------------------------------
#| echo: false
#| fig-height: 16
#| fig-width: 8
#| out-width: 80%
load("data/math_results_tb.rda")
load("data/student2018_stats.rda")
student2018_stats <- student2018_stats |>
  filter(!is.na(wreadgap))
ggplot() +
  geom_hline(yintercept = 0, 
    colour="grey80", linewidth=2) +
  geom_point(data=student2018_stats, 
    aes(x=fct_reorder(country, wmathgap), 
        y=wmathgap)) + coord_flip() +
  xlab("") + ylab("Math gap") +
  theme(aspect.ratio = 2)


## -------------------------------------------------------------
#| echo: false
#| fig-height: 16
#| fig-width: 8
#| out-width: 80%
load("data/read_results_tb.rda")
ggplot() +
  geom_hline(yintercept = 0, 
    colour="grey80", linewidth=2) +
  geom_point(data=student2018_stats, 
    aes(x=fct_reorder(country, wreadgap), 
        y=wreadgap)) + coord_flip() +
  xlab("") + ylab("Reading gap") +
  theme(aspect.ratio = 2)


## -------------------------------------------------------------
#| echo: false
#| fig-height: 8
#| fig-width: 16
#| out-width: 80%
student_data_2018 |>
  filter(!is.na(television)) |>
  group_by(country, television) |>
  summarise(math = mean(math, na.rm=TRUE)) |>
  ungroup() |>
  filter(!is.na(math)) |>
  ggplot(aes(x=television, y=math, group=country)) +
    geom_line() +
    facet_wrap(~country, ncol=16)


## -------------------------------------------------------------
#| label: wages-trend1
#| echo: false
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
wages |>
  ggplot() +
    geom_line(aes(x = xp, y = ln_wages, group = id), alpha=0.1) +
    geom_smooth(aes(x = xp, y = ln_wages), se=F) +
    xlab("years of experience") +
    ylab("wages (log)") +
  theme(aspect.ratio = 0.6)


## -------------------------------------------------------------
#| label: wages-trend2
#| echo: false
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
wages |>
  ggplot() +
    geom_line(aes(x = xp, y = ln_wages, group = id), alpha=0.1) +
    geom_smooth(aes(x = xp, y = ln_wages, 
      group = high_grade, colour = high_grade), se=F) +
    xlab("years of experience") +
    ylab("wages (log)") +
  scale_colour_viridis_c("education") +
  theme(aspect.ratio = 0.6)


## -------------------------------------------------------------
#| label: sample-n1
#| echo: false
#| fig-width: 6
#| fig-height: 5
set.seed(753)
wages |>
  sample_n_keys(size = 10) |> 
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = as.factor(id))) + 
  geom_line() +
  xlim(c(0,13)) + ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## -------------------------------------------------------------
#| label: sample-n2
#| echo: false
#| fig-width: 6
#| fig-height: 5
set.seed(749)
wages |>
  sample_n_keys(size = 10) |> 
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = as.factor(id))) + 
  geom_line() +
  xlim(c(0,13)) + ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## -------------------------------------------------------------
#| label: sample-n3
#| echo: false
#| fig-width: 6
#| fig-height: 5
set.seed(757)
wages |>
  sample_n_keys(size = 10) |> 
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = as.factor(id))) + 
  geom_line() +
  xlim(c(0,13)) + ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## -------------------------------------------------------------
#| label: increasing
#| echo: false
#| fig-width: 6
#| fig-height: 5
wages_slope <- wages |>   
  add_n_obs() |>
  filter(n_obs > 4) |>
  add_key_slope(ln_wages ~ xp) |> 
  as_tsibble(key = id, index = xp) 
wages_spread <- wages |>
  features(ln_wages, feat_spread) |>
  right_join(wages_slope, by="id")

wages_slope |> 
  filter(.slope_xp > 0.3) |> 
  ggplot(aes(x = xp, 
             y = ln_wages, 
             group = id,
             colour = factor(id))) + 
  geom_line() +
  xlim(c(0, 4.5)) +
  ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## -------------------------------------------------------------
#| label: decreasing
#| echo: false
#| fig-width: 6
#| fig-height: 5
wages_slope |> 
  filter(.slope_xp < (-0.4)) |> 
  ggplot(aes(x = xp, 
             y = ln_wages, 
             group = id,
             colour = factor(id))) + 
  geom_line() +
  xlim(c(0, 4.5)) +
  ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## -------------------------------------------------------------
#| label: small-sigma
#| echo: false
#| fig-width: 6
#| fig-height: 5
wages_spread |> 
  filter(sd < 0.1) |> 
  ggplot(aes(x = xp, 
             y = ln_wages, 
             group = id,
             colour = factor(id))) + 
  geom_line() +
  xlim(c(0, 12)) +
  ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## -------------------------------------------------------------
#| label: large-sigma
#| echo: false
#| fig-width: 6
#| fig-height: 5
wages_spread |> 
  filter(sd > 0.8) |> 
  ggplot(aes(x = xp, 
             y = ln_wages, 
             group = id,
             colour = factor(id))) + 
  geom_line() +
  xlim(c(0, 12)) +
  ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## -------------------------------------------------------------
#| label: tsibbletalk1
#| echo: false
#| fig-height: 5
#| fig-width: 10
#| out-width: 100%
tourism_shared <- tourism |>
  as_shared_tsibble(spec = (State / Region) * Purpose)

tourism_feat <- tourism_shared |>
  features(Trips, feat_stl)

p1 <- tourism_shared |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = Region), alpha = 0.5) +
  facet_wrap(~ Purpose, scales = "free_y") +
  theme(axis.title = element_text(family="Helvetica"),
        axis.text = element_text(family="Helvetica"),
        legend.title = element_text(family="Helvetica"),
        legend.text = element_text(family="Helvetica"))
p2 <- tourism_feat |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(group = Region)) +
  xlab("trend") + ylab("season") +
  theme(axis.title = element_text(family="Helvetica"),
        axis.text = element_text(family="Helvetica"),
        legend.title = element_text(family="Helvetica"),
        legend.text = element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica"))
  
subplot(
    ggplotly(p1, tooltip = "Region", width = 1400, height = 700) |>
  config(displayModeBar = FALSE),
    ggplotly(p2, tooltip = "Region", width = 1200, height = 600) |>
  config(displayModeBar = FALSE),
    nrows = 1, widths=c(0.5, 0.5), heights=1) |>
  highlight(dynamic = FALSE)
  


## -------------------------------------------------------------
#| code-fold: true
#| code-summary: "null sample method"
# Simulated data from a polynomial shape 
# which tries to model three-point success


## -------------------------------------------------------------
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
set.seed(538)
threept <- subset(lal, type == "3pt" & !is.na(x) & !is.na(y))
threept <- threept[c(".id", "period", "time", "team", "etype", "player", "points", "result", "x", "y")]
threept <- transform(threept, 
  x = x + runif(length(x), -0.5, 0.5),
  y = y + runif(length(y), -0.5, 0.5))
threept <- transform(threept, 
  r = sqrt((x - 25) ^ 2 + y ^ 2),
  angle = atan2(y, x - 25))

# Focus in on shots in the typical range
threept_sub <- threept %>% 
  filter(between(r, 20, 39)) %>%
  mutate(angle = angle * 180 / pi) %>%
  select(angle, r)

ggplot(lineup(null_lm(r ~ poly(angle, 2)), 
              true=threept_sub, n = 20, pos = 2), 
       aes(x=angle, y=r)) + 
  geom_point(alpha=0.3) + 
  scale_x_continuous("Angle (degrees)", 
  breaks = c(0, 45, 90, 135, 180), limits = c(0, 180)) +
  facet_wrap(~ .sample, ncol = 5) +
  theme_bw() +
  theme(axis.text=element_blank(),
        axis.title=element_blank())



## -------------------------------------------------------------
#| code-fold: true
#| code-summary: "null sample method"
# Permute the class variable 
# which breaks association


## -------------------------------------------------------------
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
set.seed(536)
ggplot(data=lineup(method=null_permute("mpg"), 
                   n = 12, true=mtcars)) +
  geom_boxplot(aes(x=factor(cyl), 
                   y=mpg, 
                   fill=factor(cyl))) +
  scale_fill_discrete_divergingx(palette = "Zissou 1") +
  facet_wrap(~.sample, ncol=4) +
  theme(legend.position="none",
        axis.text = element_blank(),
        axis.title = element_blank())


## -------------------------------------------------------------
#| code-fold: true
#| code-summary: "null sample method"
# Permute the group variable 
# which breaks association


## -------------------------------------------------------------
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
set.seed(711)
wasps <- wasps |> dplyr::select(-ID)
library(MASS)
wasps_lda <- as_tibble(predict(lda(Group~., data=wasps), dimen=2)$x)
wasps_lda <- bind_cols(wasps, wasps_lda)

wasps_lineup <- NULL
for (i in 1:11) {
  x <- wasps
  x$Group <- sample(x$Group)
  x_lda <- as_tibble(predict(lda(Group~., data=x), dimen=2)$x)
  x_lda <- bind_cols(x |> dplyr::select(Group), x_lda)
  x_lda$.sample <- i
  wasps_lineup <- bind_rows(wasps_lineup, x_lda)
}
wasps_lda <- wasps_lda |> dplyr::select(Group, LD1, LD2)
wasps_lda$.sample <- 12
wasps_lineup <- bind_rows(wasps_lineup, wasps_lda)
ggplot(wasps_lineup, aes(x=LD1, y=LD2, colour=Group)) +
  geom_point() +
  facet_wrap(~.sample, ncol=4) + 
  scale_colour_discrete_divergingx(palette = "Zissou 1") +   
  theme(legend.position="none",
        axis.text = element_blank(),
        axis.title = element_blank()
  )


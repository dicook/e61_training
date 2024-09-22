## ----include = FALSE----------------------------------
source("setup.R")


## -----------------------------------------------------
#| label: tb-data
#| echo: false
tb <- read_csv("data/TB_notifications_2023-08-21.csv") |>
  filter(country == "Australia", year > 1996, year < 2013) |>
  select(year, contains("new_sp")) 
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
  ) 


## -----------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 3
#| out-width: 60%
tb_tidy |>
  group_by(year) |>
  summarise(count = sum(count, na.rm=T)) |>
  ggplot(aes(x=year, y=count)) +
    geom_col() +
    theme(aspect.ratio=0.7)


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 100%
tb_tidy |>
  filter(age %in% c("1524", "2534", "3544")) |>
  group_by(year, age, sex) |>
  summarise(count = sum(count, na.rm=T)) |>
  ggplot(aes(x=year, y=count)) +
    geom_col() +
    facet_grid(sex~age) +
    theme(aspect.ratio=0.7)


## -----------------------------------------------------
#| echo: false
#| fig-width: 7
#| fig-height: 6.5
#| out-width: 70%
tb_tidy |>
  group_by(year, age) |>
  summarise(count = sum(count, na.rm=T)) |>
  ggplot(aes(x=year, y=count)) +
    geom_col() +
    facet_wrap(~age, ncol=3) +
    theme(aspect.ratio=0.7)


## -----------------------------------------------------
#| echo: false
tb_tidy <- tb_tidy |>
  filter(!(age %in% c("u", "014"))) |>
  mutate(age = fct_recode(age, 
                          "15-24" = "1524",
                          "15-24" = "1524",
                          "25-34" = "2534",
                          "35-44" = "3544",
                          "45-54" = "4554",
                          "55-64" = "5564",
                          ">65" = "65"))
tb_tidy_smry <- tb_tidy |>
  group_by(year, age, sex) |>
  summarise(count = sum(count, na.rm=T)) 



## -----------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 3
#| out-width: 100%
ggplot(tb_tidy_smry, aes(x=year, y=count)) +
    geom_col() +
    facet_grid(sex~age) +
    theme(aspect.ratio=0.7)


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 100%
base_plot <- ggplot(tb_tidy_smry, aes(x=year, y=count, colour=sex)) +
    geom_point(alpha=0.5) +
    geom_smooth(method="lm", se=F) +
    facet_wrap(~age, ncol=3) +
    scale_color_manual("", values = hcl.colors(7, palette="Zissou 1")[c(1,6)]) 
base_plot +
  theme(aspect.ratio=0.7) 


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 100%
base_plot + scale_y_continuous("count", breaks=seq(0, 50, 20)) +
    theme(aspect.ratio=0.7, 
          axis.text = element_text(colour = "grey80"),
          axis.title = element_text(colour = "grey80"),
          panel.grid.major = element_line(colour = "grey80",
                                          linewidth=0.1),
          panel.background  =
            element_rect(fill = 'transparent', colour = "grey80"),
          strip.background =
            element_rect(fill = "grey90", colour = "grey90")
          ) 


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 100%
base_plot + scale_y_continuous("count", breaks=seq(0, 50, 20)) +
    labs(title = "Incidence of tuberculosis", 
         subtitle = "Growing in young adults") +
    theme(aspect.ratio=0.7, 
          axis.text = element_text(colour = "grey80"),
          axis.title = element_text(colour = "grey80"),
          panel.grid.major = element_line(colour = "grey80",
                                          linewidth=0.1),
          panel.background  =
            element_rect(fill = 'transparent', colour = "grey80"),
          strip.background =
            element_rect(fill = "grey90", colour = "grey90"),
          plot.title = element_text(size = 24, colour = "black", 
                                    face = "bold"),
          plot.subtitle = element_text(size = 18, colour = "black")
          ) 


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 100%
ggplot(tb_tidy_smry, aes(x=year, y=count, colour=sex)) +
    geom_point(alpha=0.3) +
    geom_smooth(method="lm", se=F, linewidth=1.5) +
    facet_wrap(~age, ncol=3) +
    scale_color_manual("", values = deutan(hcl.colors(7, palette="Zissou 1")[c(1,6)])) +
    scale_y_continuous("count", breaks=seq(0, 50, 20)) +
    labs(title = "Incidence of tuberculosis", 
         subtitle = "Growing in young adults") +
    theme(aspect.ratio=0.7, 
          axis.text = element_text(colour = "grey80"),
          axis.title = element_text(colour = "grey80"),
          panel.grid.major = element_line(colour = "grey80",
                                          linewidth=0.1),
          panel.background  =
            element_rect(fill = 'transparent', colour = "grey80"),
          strip.background =
            element_rect(fill = "grey90", colour = "grey90"),
          plot.title = element_text(size = 24, colour = "black", 
                                    face = "bold"),
          plot.subtitle = element_text(size = 18, colour = "black")
          )


## -----------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 100%
ggplot(tb_tidy_smry, aes(x=year, y=count, colour=sex)) +
    geom_point(alpha=0.3) +
    geom_smooth(method="lm", se=F, linewidth=1.5) +
    facet_wrap(~age, ncol=3) +
    scale_color_manual("", values = hcl.colors(7, palette="Zissou 1")[c(6, 1)]) +
    scale_y_continuous("count", breaks=seq(0, 50, 20)) +
    labs(title = "Incidence of tuberculosis", 
         subtitle = "Growing in young adults") +
    geom_curve(data=filter(tb_tidy_smry, age == "15-24"), aes(x = 2007, y = 43, xend = 2010, yend = 39), 
                             colour = "#3B99B1", 
                             size=0.5, 
                             curvature = -0.2,
                             arrow = arrow(length = unit(0.03, "npc"))) +
    geom_label(data=filter(tb_tidy_smry, age == "15-24"), 
               aes(x = 2006, y = 42, label = "men"), 
                   hjust = 0.8, vjust = 0.5, colour = "#3B99B1", 
             fill = 'transparent', 
             label.size = NA, 
             family="Helvetica") +
    geom_segment(data=filter(tb_tidy_smry, age == "15-24"), 
                 aes(x = 2010, y = 18, xend = 2010, yend = 25), 
                             colour = "#E87700", 
                             size=0.5, 
                             curvature = 0.2,
                             arrow = arrow(length = unit(0.03, "npc"))) +
    geom_label(data=filter(tb_tidy_smry, age == "15-24"), 
               aes(x = 2010, y = 16, label = "women"), 
                   hjust = 0.5, vjust = 0.8, colour = "#E87700", 
             fill = 'transparent', 
             label.size = NA, 
             family="Helvetica") +
     theme(aspect.ratio=0.7, 
          axis.text = element_text(colour = "grey80"),
          axis.title = element_text(colour = "grey80"),
          panel.grid.major = element_line(colour = "grey80",
                                          linewidth=0.1),
          panel.background  =
            element_rect(fill = 'transparent', colour = "grey80"),
          strip.background =
            element_rect(fill = "grey90", colour = "grey90"),
          plot.title = element_text(size = 24, colour = "black", 
                                    face = "bold"),
          plot.subtitle = element_text(size = 18, colour = "black"),
          legend.position = "none"
          ) 


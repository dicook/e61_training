library(tidyverse)
library(colorspace)
library(patchwork)
library(broom)
library(ggbeeswarm)
library(data.table)
library(readxl)
library(naniar)
library(forcats)
library(nullabor)
library(plotly)
library(brolgar)
library(DT)
library(crosstalk)
library(gganimate)
library(gapminder)
library(tsibble)
library(tsibbletalk)
library(feasts)
library(ggdist)
library(GGally)
library(cubble)
library(sf)
library(ggthemes)
library(sugarbag)

options(width = 200)
knitr::opts_chunk$set(
  fig.width = 3,
  fig.height = 3,
  fig.align = "center",
  dev.args = list(bg = 'transparent'),
  out.width = "100%",
  fig.retina = 4,
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  cache = FALSE
)
theme_set(ggthemes::theme_gdocs(base_size = 12) +
            theme(aspect.ratio = 1,
                  plot.background =
                    element_rect(fill = 'transparent', colour = NA),
                  plot.title.position = "plot",
                  plot.title = element_text(size = 18),
                  panel.background  =
                    element_rect(fill = 'transparent',
                                 colour = "black", linewidth = 0.5),
                  axis.line.x  =
                    element_line(colour = "black", linewidth = 0.25),
                  legend.background =
                    element_rect(fill = 'transparent', colour = NA),
                  legend.key        =
                    element_rect(fill = 'transparent', colour = NA)
            )
)

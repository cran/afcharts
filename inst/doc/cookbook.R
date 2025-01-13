## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----load-packages------------------------------------------------------------
library(afcharts)
library(ggplot2)
library(dplyr)
library(ggtext)
library(scales)

# Use gapminder data for cookbook charts
library(gapminder)

## ----line-charts-1------------------------------------------------------------
gapminder |>
  filter(country == "United Kingdom") |>
  ggplot(aes(x = year, y = lifeExp)) +
  geom_line(linewidth = 1, colour = af_colour_values["dark-blue"]) +
  theme_af() +
  scale_y_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, 20),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(
    x = "Year",
    y = NULL,
    title = "Living Longer",
    subtitle = "Life Expectancy in the United Kingdom 1952-2007",
    caption = "Source: Gapminder"
  )

## ----line-charts-2, fig.height = 5--------------------------------------------
gapminder |>
  filter(country %in% c("United Kingdom", "China")) |>
  ggplot(aes(x = year, y = lifeExp, colour = country)) +
  geom_line(linewidth = 1) +
  theme_af(legend = "bottom") +
  scale_colour_discrete_af() +
  scale_y_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, 20),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(
    x = "Year",
    y = NULL,
    title = "Living Longer",
    subtitle = "Life Expectancy in the United Kingdom and China 1952-2007",
    caption = "Source: Gapminder",
    colour = NULL
  )

## ----bar-data-----------------------------------------------------------------
pop_bar_data <- gapminder |>
  filter(year == 2007 & continent == "Americas") |>
  slice_max(order_by = pop, n = 5)

## ----bar-chart-1--------------------------------------------------------------
ggplot(pop_bar_data, aes(x = reorder(country, -pop), y = pop)) +
  geom_col(fill = af_colour_values["dark-blue"]) +
  theme_af() +
  scale_y_continuous(
    limits = c(0, 350E6),
    labels = scales::label_number(scale = 1E-6),
    expand = expansion(mult = c(0, 0.1)),
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = stringr::str_wrap(
      "The U.S.A. is the most populous country in the Americas",
      40
    ),
    subtitle = "Population of countries in the Americas (millions), 2007",
    caption = "Source: Gapminder"
  )

## ----bar-chart-2--------------------------------------------------------------
ggplot(pop_bar_data, aes(x = pop, y = reorder(country, pop))) +
  geom_col(fill = af_colour_values["dark-blue"]) +
  theme_af(grid = "x", axis = "y") +
  scale_x_continuous(
    limits = c(0, 350E6),
    labels = scales::label_number(scale = 1E-6),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = stringr::str_wrap(
      "The U.S.A. is the most populous country in the Americas",
      40
    ),
    subtitle = "Population of countries in the Americas (millions), 2007",
    caption = "Source: Gapminder"
  )

## ----grouped-bar-chart, fig.height = 5.5--------------------------------------
grouped_bar_data <-
  gapminder |>
  filter(
    year %in% c(1967, 2007) &
      country %in% c("United Kingdom", "Ireland", "France", "Belgium")
  )

ggplot(
  grouped_bar_data,
  aes(x = country, y = lifeExp, fill = as.factor(year))
) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(seq(0, 100, 20)),
    labels = c(seq(0, 100, 20)),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_af(legend = "bottom") +
  scale_fill_discrete_af() +
  labs(
    x = "Country",
    y = NULL,
    fill = NULL,
    title = "Living longer",
    subtitle = "Difference in life expectancy, 1967-2007",
    caption = "Source: Gapminder"
  )

## ----stacked-bar-chart, fig.height = 5.5--------------------------------------
stacked_bar_data <-
  gapminder |>
  filter(year == 2007) |>
  mutate(
    lifeExpGrouped = cut(
      lifeExp,
      breaks = c(0, 75, Inf),
      labels = c("Under 75", "75+")
    )
  ) |>
  group_by(continent, lifeExpGrouped) |>
  summarise(n_countries = n(), .groups = "drop")

ggplot(
  stacked_bar_data,
  aes(x = continent, y = n_countries, fill = lifeExpGrouped)
) +
  geom_bar(stat = "identity", position = "fill") +
  theme_af(legend = "bottom") +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.1))
  ) +
  coord_cartesian(clip = "off") +
  scale_fill_discrete_af() +
  labs(
    x = NULL,
    y = NULL,
    fill = "Life Expectancy",
    title = "How life expectancy varies across continents",
    subtitle = "Percentage of countries by life expectancy band, 2007",
    caption = "Source: Gapminder"
  )

## ----histogram----------------------------------------------------------------
gapminder |>
  filter(year == 2007) |>
  ggplot(aes(x = lifeExp)) +
  geom_histogram(
    binwidth = 5,
    colour = "white",
    fill = af_colour_values["dark-blue"]
  ) +
  theme_af() +
  scale_y_continuous(
    limits = c(0, 35),
    breaks = c(seq(0, 35, 5)),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = NULL,
    y = "Number of \ncountries",
    title = "How life expectancy varies",
    subtitle = "Distribution of life expectancy, 2007",
    caption = "Source: Gapminder"
  )

## ----scatterplot, fig.height = 5----------------------------------------------
gapminder |>
  filter(year == 2007) |>
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(colour = af_colour_values["dark-blue"]) +
  theme_af(axis = "none", grid = "xy") +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(
    x = "GDP (US$, inflation-adjusted)",
    y = "Life\nExpectancy",
    title = stringr::str_wrap(
      "The relationship between GDP and Life Expectancy is complex",
      40
    ),
    subtitle = "GDP and Life Expectancy for all countires, 2007",
    caption = "Source: Gapminder"
  )

## ----small-multiples, fig.height = 5.5----------------------------------------
gapminder |>
  filter(continent != "Oceania") |>
  group_by(continent, year) |>
  summarise(pop = sum(as.numeric(pop)), .groups = "drop") |>
  ggplot(aes(x = year, y = pop, fill = continent)) +
  geom_area() +
  theme_af(axis = "none", ticks = "none", legend = "none") +
  scale_fill_discrete_af() +
  facet_wrap(~ continent, ncol = 2) +
  scale_y_continuous(
    breaks = c(0, 2e9, 4e9),
    labels = c(0, "2bn", "4bn")
  ) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank()) +
  labs(
    x = NULL,
    y = NULL,
    title = "Asia's rapid growth",
    subtitle = "Population growth by continent, 1952-2007",
    caption = "Source: Gapminder"
  )

## ----pie-chart----------------------------------------------------------------
stacked_bar_data |>
  filter(continent == "Europe") |>
  ggplot(aes(x = "", y = n_countries, fill = lifeExpGrouped)) +
  geom_col(colour = "white", position = "fill") +
  coord_polar(theta = "y") +
  theme_af(grid = "none", axis = "none", ticks = "none") +
  theme(axis.text = element_blank()) +
  scale_fill_discrete_af() +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "How life expectancy varies in Europe",
    subtitle = "Percentage of countries by life expectancy band, 2007",
    caption = "Source: Gapminder"
  )


## ----focus-chart--------------------------------------------------------------
pop_bar_data |>
  ggplot(aes(x = reorder(country, -pop), y = pop, fill = country == "Brazil")) +
  geom_col() +
  theme_af(legend = "none") +
  scale_y_continuous(
    limits = c(0, 350E6),
    labels = scales::label_number(scale = 1E-6),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_discrete_af("focus", reverse = TRUE) +
  labs(
    x = NULL,
    y = NULL,
    title = stringr::str_wrap(
      "Brazil has the second highest population in the Americas",
      40
    ),
    subtitle = "Population of countries in the Americas (millions), 2007",
    caption = "Source: Gapminder"
  )

## ----interactive-charts-------------------------------------------------------
p <-
  pop_bar_data |>
  # Format text for tooltips
  mutate(
    tooltip = paste0(
      "Country: ", country, "\n",
      "Population (millions): ", round(pop / 10 ^ 6, 1)
    )
  ) |>
  ggplot(aes(x = reorder(country, -pop), y = pop, text = tooltip)) +
  geom_col(fill = af_colour_values["dark-blue"]) +
  theme_af(ticks = "x") +
  theme(text = element_text(family = "")) +
  scale_y_continuous(
    limits = c(0, 350E6),
    labels = scales::label_number(scale = 1E-6),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = NULL,
    y = "Population (millions)"
  )

plotly::ggplotly(p, tooltip = "text") |>
  plotly::config(
    modeBarButtons = list(list("resetViews")),
    displaylogo = FALSE
  )

## ----annotations-data---------------------------------------------------------
ann_data <- gapminder |>
  filter(country %in% c("United Kingdom", "China"))

## ----annotations-1------------------------------------------------------------
ann_data |>
  ggplot(aes(x = year, y = lifeExp)) +
  geom_line(
    aes(colour = country),
    linewidth = 1
  ) +
  theme_af(legend = "none") +
  scale_colour_discrete_af() +
  scale_y_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, 20),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(1952, 2017),
    breaks = seq(1952, 2017, 5)
  ) +
  annotate(
    geom = "label",
    x = 2008, y = 73,
    label = "China",
    label.size = NA,
    hjust = 0,
    vjust = 0.5
  ) +
  annotate(
    geom = "label",
    x = 2008,
    y = 79.4,
    label = "United Kingdom",
    label.size = NA,
    hjust = 0,
    vjust = 0.5
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "Living Longer",
    subtitle = "Life Expectancy in the United Kingdom and China 1952-2007",
    caption = "Source: Gapminder"
  )

## ----annotations-2------------------------------------------------------------
ann_labs <- ann_data |>
  group_by(country) |>
  mutate(min_year = min(year)) |>
  filter(year == max(year)) |>
  ungroup()

ann_data |>
  ggplot(aes(x = year, y = lifeExp)) +
  geom_line(
    aes(colour = country),
    linewidth = 1
  ) +
  theme_af(legend = "none") +
  scale_colour_discrete_af() +
  scale_y_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, 20),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(1952, 2017),
    breaks = seq(1952, 2017, 5)
  ) +
  geom_label(
    data = ann_labs,
    aes(x = year, y = lifeExp, label = country),
    hjust = 0,
    vjust = 0.5,
    nudge_x = 0.5,
    label.size = NA
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "Living Longer",
    subtitle = "Life Expectancy in the United Kingdom and China 1952-2007",
    caption = "Source: Gapminder"
  )

## ----annotations-3------------------------------------------------------------
ggplot(pop_bar_data, aes(x = reorder(country, -pop), y = pop)) +
  geom_col(fill = af_colour_values["dark-blue"]) +
  geom_text(
    aes(label = round(pop / 1E6, 1)),
    vjust = 1.2,
    colour = "white"
  ) +
  theme_af() +
  scale_y_continuous(
    limits = c(0, 350E6),
    labels = scales::label_number(scale = 1E-6),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = stringr::str_wrap(
      "The U.S.A. is the most populous country in the Americas",
      40
    ),
    subtitle = "Population of countries in the Americas (millions), 2007",
    caption = "Source: Gapminder"
  )

## ----sorting------------------------------------------------------------------
population_chart <- pop_bar_data |>
  ggplot(aes(x = pop, y = reorder(country, pop))) +
  geom_col(fill = af_colour_values["dark-blue"]) +
  theme_af(axis = "y", grid = "x")

## ----chart-titles-------------------------------------------------------------
population_chart +
  labs(
    x = NULL,
    y = NULL,
    title = stringr::str_wrap(
      paste("The U.S.A. has the highest population in the Americas"),
      width = 40
    ),
    subtitle = "Population of countries of the Americas (millions), 2007",
    caption = "Source: Gapminder"
  )

## ----expand-------------------------------------------------------------------
population_chart +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))

## ----axis-limits-breaks-labels-custom-----------------------------------------
population_chart +
  scale_x_continuous(
    breaks = seq(0, 400E6, 100E6),
    labels = seq(0, 400, 100),
    limits = c(0, 420E6),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "Population (millions)"
  )

## ----axis-limits-breaks-labels-fct--------------------------------------------
limits_pretty <- function(x) range(pretty(x))

population_chart +
  scale_x_continuous(
    breaks = pretty,
    labels = label_number(scale = 1E-6),
    limits = limits_pretty,
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    x = "Population (millions)"
  )


## ----using-scales, fig.height = 5.5-------------------------------------------
stacked_bar_data |>
  ggplot(aes(x = continent, y = n_countries, fill = lifeExpGrouped)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_af(legend = "bottom") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = scales::percent
  ) +
  scale_fill_discrete_af() +
  labs(
    x = NULL,
    y = NULL,
    fill = "Life Expectancy",
    title = "How life expectancy varies across continents",
    subtitle = "Percentage of countries by life expectancy band, 2007",
    caption = "Source: Gapminder"
  )

## ----clip, fig.height = 5.5---------------------------------------------------
last_plot() + coord_cartesian(clip = "off")

## ----add-a-line---------------------------------------------------------------
gapminder |>
  filter(country == "United Kingdom") |>
  ggplot(aes(x = year, y = lifeExp)) +
  geom_line(linewidth = 1, colour = af_colour_values[1]) +
  geom_hline(
    yintercept = 75,
    colour = af_colour_values[2],
    linewidth = 1,
    linetype = "dashed"
  ) +
  annotate(geom = "text", x = 2007, y = 70, label = "Age 70") +
  theme_af() +
  scale_y_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, 20),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(
    x = "Year",
    y = NULL,
    title = "Living Longer",
    subtitle = "Life Expectancy in the United Kingdom 1952-2007",
    caption = "Source: Gapminder"
  )

## ----text-wrap-1--------------------------------------------------------------
plot <-
  ggplot(pop_bar_data, aes(x = reorder(country, -pop), y = pop)) +
  geom_col(fill = af_colour_values["dark-blue"]) +
  theme_af() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = NULL,
    subtitle = "Population of countries in the Americas (millions), 2007",
    caption = "Source: Gapminder"
  )

plot +
  labs(
    y = "Population of country",
    title = paste("The U.S.A. is the most populous country in ",
                  "the Americas")
  )

## ----text-wrap-2--------------------------------------------------------------
plot +
  labs(
    y = "Population\n of country",
    title = stringr::str_wrap(
      paste("The U.S.A. is the most populous country in ",
            "the Americas"),
      width = 40
    )
  )

## ----adjust-theme-------------------------------------------------------------
ggplot(pop_bar_data, aes(x = reorder(country, -pop), y = pop)) +
  geom_col(fill = af_colour_values["dark-blue"]) +
  theme_af(axis = "xy") +
  theme(
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, 350E6),
    labels = scales::label_number(scale = 1E-6)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = stringr::str_wrap(
      "The U.S.A. is the most populous country in the Americas",
      40
    ),
    subtitle = "Population of countries in the Americas (millions), 2007",
    caption = "Source: Gapminder"
  )

## ----html-formatting----------------------------------------------------------
ann_data <- gapminder |>
  filter(country %in% c("United Kingdom", "China"))

ann_labs <- ann_data |>
  group_by(country) |>
  mutate(min_year = min(year)) |>
  filter(year == max(year)) |>
  ungroup()

ann_data |>
  ggplot(aes(x = year, y = lifeExp, colour = country)) +
  geom_line(linewidth = 1) +
  theme_af(legend = "none") +
  scale_colour_discrete_af() +
  scale_y_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, 20),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(1952, 2017),
    breaks = seq(1952, 2017, 5)
  ) +
  geom_label(
    data = ann_labs,
    aes(x = year, y = lifeExp, label = country),
    hjust = 0,
    vjust = 0.5,
    nudge_x = 0.5,
    label.size = NA
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "Living Longer",
    subtitle = "Life Expectancy in the
    <span style='color:darkorange;'>United Kingdom</span> and
    <span style='color:navy;'>China</span> 1952-2007",
    caption = "Source: Gapminder"
  ) +
  theme(plot.subtitle = element_markdown())

## ----af-palette, fig.height = 5-----------------------------------------------
gapminder |>
  filter(country %in% c("United Kingdom", "China")) |>
  ggplot(aes(x = year, y = lifeExp, colour = country)) +
  geom_line(linewidth = 1) +
  theme_af(legend = "bottom") +
  scale_colour_discrete_af("main2") +
  scale_y_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, 20),
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(
    x = "Year",
    y = NULL,
    title = "Living Longer",
    subtitle = "Life Expectancy in the United Kingdom and China 1952-2007",
    caption = "Source: Gapminder",
    colour = NULL
  )

## ----different-colour-palette-1-----------------------------------------------
my_palette <- c("#0F820D", "#000000")

gapminder |>
  filter(country == "United Kingdom") |>
  ggplot(aes(x = year, y = lifeExp)) +
  geom_line(linewidth = 1, colour = my_palette[1]) +
  theme_af() +
  scale_y_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, 20),
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(
    x = "Year",
    y = NULL,
    title = "Living Longer",
    subtitle = "Life Expectancy in the United Kingdom 1952-2007",
    caption = "Source: Gapminder"
  )

## ----different-colour-palette-2, fig.height = 5.5-----------------------------
gapminder |>
  filter(country %in% c("United Kingdom", "China")) |>
  ggplot(aes(x = year, y = lifeExp, colour = country)) +
  geom_line(linewidth = 1) +
  theme_af(legend = "bottom") +
  scale_colour_manual(values = my_palette) +
  scale_y_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, 20),
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(
    x = "Year",
    y = NULL,
    title = "Living Longer",
    subtitle = "Life Expectancy in the United Kingdom and China 1952-2007",
    caption = "Source: Gapminder",
    colour = NULL
  )


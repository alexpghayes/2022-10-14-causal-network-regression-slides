library(netmediate)
library(tidygraph)
library(here)
library(ggraph)

set.seed(27)

glasgow1 <- glasgow[[1]] |>
  activate(nodes) |>
  filter(selection129) |>
  mutate(
    smokes_dimaria = as.numeric(tobacco_int > 1),
    drinks_dimaria = as.numeric(alcohol_int > 2),
    weed_dimaria = as.numeric(cannabis_int > 2),
    in_degree = centrality_degree(mode = "in"),
    friend_group = group_walktrap()
  ) |>
  activate(edges) |>
  filter(friendship != "Structurally missing") |>
  activate(nodes)

layout <- glasgow1 |>
  create_layout(layout = 'stress')

degrees <- c(0, 2, 4, 6, 8, 10)

layout |>
  ggraph() +
  geom_edge_fan(
    arrow = arrow(length = unit(1, 'mm')),
    end_cap = circle(2.5, 'mm')
  ) +
  geom_node_point(aes(size = in_degree)) +
  scale_size(breaks = degrees, labels = degrees) +
  labs(
    title = "Many behaviors are social and depend on network structure",
    subtitle = "Each node represents one adolescent",
    caption = "Source: Teenage Friends and Lifestyle Study, s129 dataset, 1995",
    size = "Num friends\n(in-degree)"
  ) +
  theme_void(base_size = 18, base_family = "Fira Sans")

ggsave(
  here::here("figures/glasgow-degree.png"),
  height = 6,
  width = 6 * 13/9,
  dpi = 500
)

layout |>
  ggraph() +
  geom_edge_fan(
    arrow = arrow(length = unit(1, 'mm')),
    end_cap = circle(2.5, 'mm')
  ) +
  geom_node_point(aes(color = alcohol_fct, size = in_degree)) +
  scale_size(guide = waiver()) +
  scale_color_brewer(direction = 1, palette = "RdYlGn") +
  labs(
    title = "How to account for the social nature of drinking?",
    subtitle = "Each node represents one adolescent",
    caption = "Source: Teenage Friends and Lifestyle Study, s129 dataset, 1995",
    color = "Frequency of\nalcohol use"
  ) +
  theme_void(base_size = 18, base_family = "Fira Sans")

ggsave(
  here::here("figures/glasgow-alcohol.png"),
  height = 6,
  width = 6 * 13/9,
  dpi = 500
)

layout |>
  ggraph() +
  geom_edge_fan(
    arrow = arrow(length = unit(1, 'mm')),
    end_cap = circle(2.5, 'mm')
  ) +
  geom_node_point(
    aes(color = as.factor(friend_group), size = in_degree),
    show.legend = FALSE
  ) +
  labs(
    title = "Key idea: suppose node behavior varies with friend group",
    subtitle = "i.e. nodal features (treatments, controls, outcomes) are homophilous",
    caption = "Nodes colored by estimated friend group"
  ) +
  theme_void(base_size = 18, base_family = "Fira Sans")

ggsave(
  here::here("figures/glasgow-friendgroups.png"),
  height = 6,
  width = 6 * 13/9,
  dpi = 500
)

layout |>
  ggraph() +
  geom_edge_fan(
    arrow = arrow(length = unit(1, 'mm')),
    end_cap = circle(2.5, 'mm')
  ) +
  geom_node_point(aes(color = tobacco_fct, size = in_degree)) +
  labs(
    title = "Friendships in a secondary school in Glasgow",
    subtitle = "s129 dataset from the Teenage Friends and Lifestyle Study, 1995",
    caption = "Each node represents one student",
    size = "Popularity (in-degree)",
    color = "Tobacco usage"
  ) +
  theme_void(base_size = 18, base_family = "Fira Sans")

ggsave(
  here::here("figures/glasgow-smokes.pdf")
)

library(gdim)

cv_eigs <- eigcv(A1, k_max = 50)
cv_eigs
```

```{r}
plot(cv_eigs)  # suggests k ~= 4 or 5, maybe up to 9
```

```{r}
library(vsp)

fa1 <- vsp(A1, rank = 9, degree_normalize = FALSE)
fa1

plot_mixing_matrix(fa1)
screeplot(fa1)
```


```{r}
plot_ipr_pairs(fa1)
```


```{r}
library(tidyverse)
library(vsp)

fa1 %>%
  get_varimax_y(1:9) %>%
  dplyr::select(-id) %>%
  GGally::ggpairs(ggplot2::aes(alpha = 0.001), progress = FALSE) +
  ggplot2::theme_minimal()
```


```{r}
fa1 %>%
  get_varimax_z(1:9) %>%
  dplyr::select(-id) %>%
  dplyr::mutate(leverage = purrr::pmap_dbl(., sum)) %>%
  dplyr::select(-leverage) %>%
  GGally::ggpairs(ggplot2::aes(alpha = 0.001), progress = FALSE) +
  ggplot2::theme_minimal()
```
looks like Y factors are better than Z factors

```{r}
V1 <- VS(A1, 9)

m_fit <- nodelm(V1 ~ sex_fct, graph = glasgow1)
anova(m_fit)

o_fit <- nodelm(smokes_dimaria ~ sex_fct + V1, data = glasgow1)
summary(o_fit)

ols <- nodelm(smokes_dimaria ~ sex_fct, data = glasgow1)
summary(ols)
```




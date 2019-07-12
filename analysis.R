# anlysis

library(tidyverse)
library(countrycode)
library(geosphere)
library(widyr)


# df_app <- readRDS("data/regpat_epo_app.rds")
df_inv <- readRDS("data/regpat_epo_inv.rds")

df_inv <- df_inv %>% mutate(eu28 = countrycode(ctry, origin = "iso2c", destination= "eu28"))
df_inv <- df_inv %>% group_by(appln_id) %>% filter(any(eu28 %in% "EU")) %>% ungroup()

df_inv <- df_inv %>% left_join(cent, by = "reg") %>% 
  left_join(world, by = "ctry")

df <- df %>% mutate(
  lat = case_when(
    !is.na(lat.x) ~ lat.x,
    TRUE ~ lat.y
  ),
  lon = case_when(
    !is.na(lon.x) ~ lon.x,
    TRUE ~ lon.y
  )
) %>% select(-lat.x, -lat.y, -lon.x, -lon.y, everything())

saveRDS(df, file = "C:/Users/KalcikR/OneDrive/Arbeit/AIT/1803_Radical Inno/radical/data/temp.rds")
# df_app %>% count(appln_id) %>%  count(n)
# df_app %>% group_by(sector) %>% summarise(n = n(), radical = mean(radicalness, na.rm = TRUE))
df <- readRDS("data/temp.rds")
df_small <- df[1:10000, ]
saveRDS(df_small, file = "data/temp_small.rds")
# df_app %>% count(appln_id) %>%  count(n)
# df_app %>% group_by(sector) %>% summarise(n = n(), radical = mean(radicalness, na.rm = TRUE))
df <- readRDS("data/temp_small.rds")
lol <- df[1:20, ]

person_dist <- function(df, pb = NULL){
  # progress bar magic!
  if (!is.null(pb)) pb$tick()$print()
  
  x <- df[, c("lon", "lat")]
  item_vals <- df$person_id
  #item_u <- unique(item_vals)
  match(item_vals, item_u)
  x <- distm(x)
  # dimnames(x) <- list(df$person_id, df$person_id)
  x <- x %>% reshape2::melt(varnames = c("person_id_1", "person_id_2")) %>% as.tibble()
  x <- x %>% filter(person_id_1 < person_id_2)
  x$person_id_1 <- item_vals[x$person_id_1]
  x$person_id_2 <- item_vals[x$person_id_2]
  return(x)
}

lolo <- df %>% group_by(appln_id, radicalness) %>% nest(person_id, lat, lon) 

pb <- dplyr::progress_estimated(length(lolo$appln_id))
df_dist <- lolo %>% 
  mutate(data = map(data, person_dist, pb = pb)) 
saveRDS(df_dist, file = "C:/Users/KalcikR/OneDrive/Arbeit/AIT/1803_Radical Inno/radical/data/inventor_distances.rds")
df_dist <- readRDS("data/inventor_distances.rds")
df_dist <- df_dist %>% unnest()

df_dist <- df_dist %>% mutate(
  d = cut(value, 
          breaks = c(-Inf, 0, 10000, 100000, 1000000, Inf),
          labels = c("Same Region", "0 - 10km", "10 - 100km", "100 - 1000km", ">1000km")
  )
)

df_avg_dist <- df_dist %>% group_by(appln_id) %>% summarise(avg_dist = mean(value, na.rm = TRUE))
df_avg_dist <- df_avg_dist %>% mutate(
  d = cut(avg_dist, 
          breaks = c(-Inf, 0, 100000, 1000000, Inf),
          labels = c("Same Region", "10 - 100km", "100 - 1000km", ">1000km")
  )
)
saveRDS(df_avg_dist, file = "data/inventor_avg_distances.rds")

df_dist %>% filter(!is.na(d)) %>% 
  ggplot(aes(x = d, y = radicalness)) +
  geom_boxplot() +
  facet_wrap(~tech_field)

df_dist %>% filter(radicalness != 0, !is.na(d)) %>% 
  ggplot(aes(x = radicalness)) +
  geom_histogram() + 
  facet_wrap(~d, scale = "free_y")

model <- lm("radicalness ~ value", df_dist) # %>% filter(radicalness != 0, value != 0)) %>% summary()

df_dist %>% group_by(appln_id) %>% 
  summarise(dist = mean(value), radicalness = first(radicalness)) %>% 
  filter(dist != 0, radicalness != 0) %>% 
  ggplot(aes(x = dist, y = radicalness)) +
  geom_point() +
  geom_smooth()


aov(radicalness ~ d, data = df_dist) %>% summary()

df_dist <- df_dist %>% select(-radicalness) %>% 
  left_join(radical, by = "appln_id")

df_dist %>% group_by(d) %>% 
  summarise(rad = mean(radicalness, na.rm = TRUE), n())

df_app %>% filter(radicalness > 0) %>% 
  ggplot(aes(x = radicalness)) +
    geom_histogram() +
    facet_wrap(~sector, scales = "free_y")



# something ---------------------------------------------------------------




df_inv %>% group_by(appln_id) %>% summarise(radicalness = mean(radicalness), regs = count(regs))




# region network ----------------------------------------------------------
df <- readRDS("data/temp_small.rds")
df <- readRDS("data/temp.rds")
df <- df %>% select(appln_id, reg) %>% distinct()
nodes <-  df %>% count(reg) %>% arrange(desc(n))

edges <- full_join(df, df, by = "appln_id")
edges <- edges %>% count(reg.x, reg.y) %>% 
  filter(reg.x != reg.y)%>% arrange(desc(n))

library(tidygraph)

g <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
g <- g %>% activate(edges) %>% distinct()

g <- g %>% 
  activate(nodes) %>% 
  mutate(central = centrality_alpha(), between = centrality_betweenness())

saveRDS(g, "data/graph.rds")

# merge -------------------------------------------------------------------
df_avg_dist <- readRDS(file = "data/inventor_avg_distances.rds")
# df <- readRDS("data/temp_small.rds")
df <- readRDS("data/temp.rds")

df_patent <- df %>% group_by(appln_id) %>% 
  summarise(
    year = first(year_app), 
    tech_field = first(tech_field), 
    ctry = first(ctry),
    radicalness = first(radicalness),
    avg_central = mean(central))

df_patent <- df_patent %>% left_join(df_avg_dist, by = c("appln_id"))
df_patent <- df_patent %>% left_join(as_tibble(g), by = "reg")
df_patent <- df_patent %>% replace_na(list(avg_dist = 0, d = "Same Region"))
df_patent <- df_patent %>% left_join(avg_similarity, by = "appln_id")

# anlysis -----------------------------------------------------------------
df_patent <- df_patent %>% 
  mutate(
    avg_dist_sqr = avg_dist ^2,
    similarity_unity = case_when(is.na(similarity) ~ 1, TRUE ~ similarity)
  )

saveRDS(df_patent, file = "data/regression_in.rds")

m1 <- lm("radicalness ~ avg_dist + avg_dist_sqr + avg_central + year + factor(tech_field) ", df_patent)
summary(m1)

m2 <- lm("radicalness ~ factor(d) + avg_central + year + factor(tech_field)", df_patent)
summary(m2)

m3 <- lm("radicalness ~ avg_dist + avg_dist_sqr + avg_central + similarity + year + factor(tech_field) ", df_patent)
summary(m3)

m4 <- lm("radicalness ~ factor(d) + avg_central + similarity + year + factor(tech_field)", df_patent)
summary(m4)

m7 <- lm("radicalness ~ avg_dist + avg_dist_sqr + avg_central + similarity_unity + year + factor(tech_field) ", df_patent)
summary(m7)

m8 <- lm("radicalness ~ factor(d) + avg_central + similarity_unity + year + factor(tech_field)", df_patent)
summary(m8)

stargazer::stargazer(m1, m2, m3, m4, m7, m8, type = "html", 
          dep.var.labels = c(), 
          covariate.labels = c(), 
          out="models.htm")


# only radical > 0 --------------------------------------------------------

df_patent_filtered <- df_patent %>% filter(radicalness > 0 )

m1 <- lm("radicalness ~ avg_dist + avg_dist_sqr + avg_central + year + factor(tech_field) ", df_patent_filtered)
summary(m1)

m2 <- lm("radicalness ~ factor(d) + avg_central + year + factor(tech_field)", df_patent_filtered)
summary(m2)

m3 <- lm("radicalness ~ avg_dist + avg_dist_sqr + avg_central + similarity + year + factor(tech_field) ", df_patent_filtered)
summary(m3)

m4 <- lm("radicalness ~ factor(d) + avg_central + similarity + year + factor(tech_field)", df_patent_filtered)
summary(m4)

m7 <- lm("radicalness ~ avg_dist + avg_dist_sqr + avg_central + similarity_unity + year + factor(tech_field) ", df_patent_filtered)
summary(m7)

m8 <- lm("radicalness ~ factor(d) + avg_central + similarity_unity + year + factor(tech_field)", df_patent_filtered)
summary(m8)

stargazer::stargazer(m1, m2, m3, m4, m7, m8, type = "html", 
                     dep.var.labels = c(), 
                     covariate.labels = c(), 
                     out = "models_intensive.htm")

# merge with full data ----------------------------------------------------
person_reg <- df %>% select(reg, person_id, year)
reg_edges <- df %>% select(appln_id, reg) %>% distinct()
reg_edges <- full_join(reg_edges, reg_edges, by = "appln_id")


# node similarity ---------------------------------------------------------
# similarity <- df %>% select(appln_id, reg) %>% 
#   count(appln_id, reg) %>% 
#   widyr::pairwise_delta(appln_id, reg, n, method = "burrows")

s <- reg_edges %>% count(reg.x, reg.y) %>% 
  filter(reg.x != reg.y) %>% 
  group_by(reg.x) %>% mutate(total.x = sum(n, na.rm = TRUE)) %>% ungroup() %>% 
  group_by(reg.y) %>% mutate(total.y = sum(n, na.rm = TRUE)) %>% ungroup() %>% 
  mutate(similarity = n /(total.x + total.y))

avg_similarity <- reg_edges %>% left_join(s, by = c("reg.x", "reg.y"))
avg_similarity <- avg_similarity %>% group_by(appln_id) %>% summarise(similarity = mean(similarity, na.rm = TRUE))


# g %>% activate(node) %>% 
#   mutate(similarity = node_similarity_with())

reg_edges %>% count(reg.x, reg.y)
  widyr::pairwise_delta()

# viz ---------------------------------------------------------------------
g_aut <- g %>% activate(nodes) %>% 
  filter(str_sub(reg, 1, 2) == "AT") %>% 
  left_join(nuts, by = "reg")

g_aut %>%
  # ggraph(layout = "graphopt") + 
  # ggraph(layout = 'linear', circular = TRUE) +
  ggraph() +
  geom_edge_link(aes(width = n), alpha = 0.2) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_point(aes(size = n, colour = NUTS.2)) +
  geom_node_text(aes(label = reg_name), repel = TRUE) +
  labs(edge_width = "# Co-Patents", node_size = "# Total Patents") +
  theme_graph()

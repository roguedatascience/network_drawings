library(tidyverse)
library(igraph)
library(viridis)

# https://stackoverflow.com/questions/29586219/r-dplyr-rename-column-name-by-position-instead-of-name

set.seed(13)

all_options <-
    expand.grid(1:40, 1:40) %>%
    setNames(., c('x', 'y')) %>%
    sample_frac(.7)

df <-
    dist(all_options) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename('from' = !!names(.[1])) %>%
    gather('to', 'dist', -from) %>%
    dplyr::as_data_frame() %>%
    filter(dist > 0 & dist < 2) %>%
    sample_frac(.15) %>%
    left_join(
        all_options %>%
            rownames_to_column() %>%
            rename(from_x = x,
                   from_y = y),
        by = c('from' = 'rowname')
    ) %>%
    left_join(
        all_options %>%
            rownames_to_column() %>%
            rename(to_x = x,
                   to_y = y),
        by = c('to' = 'rowname')
    )

df %>%
    ggplot() +
    geom_segment(aes(x = from_x,
                     y = from_y,
                     xend = to_x,
                     yend = to_y))

g <-
    graph_from_data_frame(df[, 1:2],
                          directed = FALSE)

components_ls <-
    components(g)

components_df <-
    components_ls$membership %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    setNames(., c('node', 'component'))

components_df %>%
    group_by(component) %>%
    count() %>%
    arrange(desc(n)) %>%
    ggplot(aes(x = n)) +
    geom_histogram()

df <-
    df %>%
    left_join(components_df, by = c('from' = 'node'))

df_unique <-
    bind_rows(
        df %>%
            rename(node = from, x = from_x, y = from_y) %>%
            select(node, x, y),
        df %>%
            rename(node = to, x = to_x, y = to_y) %>%
            select(node, x, y)
    ) %>%
    distinct()


colors_vec <-
    viridis(n = 20, option = 'D')

set.seed(13)
df$edge_col <-
    sample(colors_vec, nrow(df), replace = TRUE)

set.seed(10)
df_unique$node_col <-
    sample(colors_vec, nrow(df_unique), replace = TRUE)

png('network_drawing.png', width = 800, height = 800)

df %>%
    ggplot() +
    geom_segment(aes(x = from_x,
                     y = from_y,
                     xend = to_x,
                     yend = to_y,
                     col = edge_col)) +
    # geom_point(data = df_unique,
    #            aes(x = x, y = y, col = node),
    #            size = 1) +
    geom_point(data = df_unique,
               aes(x = x, y = y),
               col = 'black', pch = 21, size = 5) +
    geom_point(data = df_unique,
               aes(x = x, y = y, fill = node_col),
               pch = 21, size = 3) +
    theme_void() +
    theme(legend.position = 'none',
          plot.background =
              element_rect(
                  fill = 'black',
                  color = 'black'))

dev.off()
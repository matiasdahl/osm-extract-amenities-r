require('dplyr')
require('knitr')
require('ggplot2')
require('scales')
require('lubridate')

Sys.setlocale(category = "LC_ALL", locale = 'UTF-8')

#
#  Takes data frame with `id` and `type` columns and rewrites the
#  `id` column with url links to openstreetmap.org with links to
#  the map elements.
#
add_osm_links <- function(df) {
    tmp <- df %>%
        mutate(id = paste('<a href="https://openstreetmap.org/',
                          type , '/', id, '">', id, '</a>',
                          sep = ''))
    return(tmp)
}

#
#  Compress revision versions and return a flattened data frame with
#  one line per map element.
#
flatten_elements <- function(df) {
    tmp <- df %>%
        group_by(id, type) %>%
        summarize(last_version = last(version),
                  last_is_visible = last(visible),
                  last_amenity_type = last(amenity_type),
                  sec1970A = first(sec1970),
                  sec1970B = last(sec1970),
                  last_pos1 = last(pos1),
                  last_pos2 = last(pos2),
                  last_name = last(name)) %>%
        ungroup() %>%
        # reorder columns analogous to the order in the output
        # of `load_amenities`.
        select(id, last_version, last_is_visible, sec1970A,
               sec1970B, type, last_pos1, last_pos2,
               last_amenity_type, last_name)
    return(as.data.frame(tmp))
}

#
#  Create a summary table
#
amenity_summary <- function(am_df) {
    nodes <- filter(am_df, type == 'node')
    ways <- filter(am_df, type == 'way')
    relations <- filter(am_df, type == 'relation')

    eval_on_all <- function(f) {
        return(c(f(nodes), f(ways), f(relations), f(am_df)))
    }

    flat_nodes <- flatten_elements(nodes)
    flat_ways <- flatten_elements(ways)
    flat_relations <- flatten_elements(relations)
    flat_amenities <- flatten_elements(am_df)

    eval_on_flats <- function(f) {
        return(c(f(flat_nodes), f(flat_ways),
                 f(flat_relations), f(flat_amenities)))
    }

    summary <- data.frame(
        number_of_extracted_versions = eval_on_all(nrow),
        unique_map_elements = eval_on_flats(nrow),
        currently_visible = eval_on_flats(function(df) {
            return(nrow(filter(df, last_is_visible == TRUE)))
        }),
        currently_deleted = eval_on_flats(function(df) {
            return(nrow(filter(df, last_is_visible == FALSE)))
        }),
        unique_amenity_types = eval_on_all(function(df) {
            return(uniques(df$amenity_type))
        })
    )

    row.names(summary) <- c('Nodes', 'Ways', 'Relations', 'Total')
    return(t.data.frame(summary))
}

#
#  Functions for plotting growth graph
#
amenity_growth <- function(flat_amenities) {
    growth_df <- flat_amenities %>%
        filter(last_is_visible == TRUE,
               last_amenity_type != "") %>%
        select(sec1970A) %>%
        arrange(sec1970A) %>%
        mutate(tag_date = from_epoch(sec1970A))
    growth_df$entry_count <- 1:nrow(growth_df)
    return(growth_df %>% select(tag_date, entry_count))
}

plot_growth <- function(flat_amenities) {
    growth_df <- amenity_growth(flat_amenities)

    # We are plotting an increasing graph. Speed up the plotting by
    # taking only 20000 samples.
    sample_points <- seq(1, nrow(growth_df), length.out = 20000)

    return(ggplot(growth_df[sample_points, ],
                  aes(x = tag_date, y = entry_count)) +
               geom_line(size = 1.0) +
               labs(x = "", y = "Element count"))
}

#
#  Functions for plotting age profile
#
plot_age_profile <- function(flat_amenities) {
    monthly_profile <- amenity_growth(flat_amenities) %>%
        mutate(month = month(tag_date),
               year = year(tag_date)) %>%
        group_by(year, month) %>%
        summarize(monthly_growth = length(entry_count)) %>%
        ungroup()

    monthly_profile$date = ISOdate(monthly_profile$year, monthly_profile$month, 1)

    return(ggplot(monthly_profile)
           + geom_bar(stat = "identity", aes(x = date,
                                             y = monthly_growth)) +
               labs(x = "", y = "Monthly growth") +
               scale_y_log10())
}

#
#  Create a bar plot showing the `top_n` most popular tags (and their counts)
#  in the current version.
#
plot_top_amenities <- function(flat_amenities, top_n) {
    live_amenities <- flat_amenities %>%
        filter(last_is_visible == TRUE, last_amenity_type != "")
    freq_table <- as.data.frame(table(live_amenities$last_amenity_type)) %>%
        arrange(Freq)

    # relevel factor variable to plot in sorted order and not alphabetically.
    freq_table$Var1 <- factor(freq_table$Var1,
                              levels = as.character(freq_table$Var1))

    return(ggplot(tail(freq_table, top_n), aes(x = Var1, y = Freq)) +
               theme(text = element_text(size = 16)) +
               labs(y = "\n Current map elements with tag", x = "") +
               scale_y_log10() +
               coord_flip() +
               geom_bar(stat = "identity", width = 0.7))
}


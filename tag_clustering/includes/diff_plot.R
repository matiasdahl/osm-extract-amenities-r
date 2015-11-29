plot_transition <- function(amenities_df, t1, t2) {
    ids_has_been_t1 <- unique((amenities_df %>%
                                   filter(amenity_type == t1))$unique_id)

    # filter out everything that never has been tagged with t1.
    amenities_1 <- amenities_df %>%
        filter(unique_id %in% ids_has_been_t1) %>%
        select(unique_id, version, sec1970, amenity_type, visible)

    is_now_t1 <- function(s) {
        entry_data_now <- amenities_1 %>% filter(sec1970 <= s)
        live1 <- live_amenities(entry_data_now) %>%
            filter(amenity_type == t1)
        length(unique(live1$unique_id))
    }

    is_now_t2_was_t1 <- function(s) {
        entry_data_now <- amenities_1 %>% filter(sec1970 <= s)
        ids_now_t2 <- (live_amenities(entry_data_now) %>%
                           filter(amenity_type == t2))$unique_id

        ids_have_been_t1 <- (entry_data_now %>%
                                 filter(amenity_type == t1))$unique_id

        ids_now_t2_was_t1 <- intersect(ids_now_t2, ids_have_been_t1)
        length(unique(ids_now_t2_was_t1))
    }

    dt0 = as.Date("2006-01-01", "%Y-%m-%d")
    dt1_end_data = as.Date("2015-08-15", "%Y-%m-%d")
    dt1_end_scale = as.Date("2016-01-01", "%Y-%m-%d")

    date_to_sec <- function(dt) as.integer(format(dt, format = "%s"))

    date_samples <- data.frame(sec1970 =
                                   seq(from = date_to_sec(dt0),
                                       to = date_to_sec(dt1_end_data),
                                       by = 3*24*60*60))

    d1 <- date_samples
    d1$count <- sapply(d1$sec1970, is_now_t1)
    d1$tag <- rep(t1, nrow(d1))

    d2 <- date_samples
    d2$count <- sapply(d2$sec1970, is_now_t2_was_t1)
    d2$tag <- rep(t2, nrow(d2))

    df <- rbind(d1, d2)
    df$tag <- as.factor(df$tag)
    df$tag <- factor(df$tag, levels = c(t1, t2))
    df$date <- from_epoch(df$sec1970)

    color_table <- c('#F8766D', # red-orange
                     '#00BFC4')  # cyan
    attributes(color_table)$names <- c(t1, t2)

    ggplot(df,
           aes(x = date,
               y = count,
               col = tag)) +
        geom_point(size = 1.2) +
        # adjust size of circles in legend
        guides(colour = guide_legend(override.aes = list(size = 3.5))) +
        scale_color_manual("", labels = c(
            paste0("tagged '", t1, "'   "),
            paste0("was tagged '", t1, "', is now tagged '", t2, "'")
        ),
        values = color_table) +
        theme(legend.position = "bottom",
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text = element_text(size = 14),
              legend.title = element_blank(),
              legend.key = element_rect(fill = "white"),
              legend.text = element_text(size = 14)) +
        labs(x = "", y = "Nr. of elements\n") +
        scale_x_date(breaks = seq(from = dt0,
                                  to = dt1_end_scale,
                                  by = "12 months"),
                     labels = date_format("%Y"))
}

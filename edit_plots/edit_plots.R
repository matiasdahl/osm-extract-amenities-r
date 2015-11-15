#
#  Select all amenities that at some point have been tagged as a particular type,
#  e.g., 'school'.
#
amenities_of_type <- function(amenities, am_type) {
    #
    #  Find all `unique_id`:s that have any version tagged with `am_type`. We
    #  need this list since deleted entries are not tagged. Also, the same
    #  element can be tagged with multiple tags in different versions.
    #
    unique_ids <- unique((amenities %>% filter(amenity_type == am_type))$unique_id)

    return(amenities %>%
        filter(unique_id %in% unique_ids) %>%
        arrange(sec1970) %>%
        select(-version, -visible, -pos1, -pos2))
}

#
#  Find table of (amenity_type, number of unique elements).
#
amenity_frequency_table <- function(all_amenities) {
    tmp <- all_amenities %>%
        # To count number of unique map elements, we do not need to know
        # whether an element has been deleted or not. Also, there is no
        # tag info when `visible==FALSE`.
        filter(visible == TRUE) %>%
        select(unique_id, amenity_type) %>%
        group_by(amenity_type) %>%
        summarize(count = n_distinct(unique_id)) %>%
        ungroup() %>%
        arrange(desc(count)) %>%
        filter(amenity_type != "") %>%
        select(amenity_type, count)

    return(tmp)
}

date_labels <- function() {
    return(seq(from = as.Date("2006-01-01", "%Y-%m-%d"),
               to = as.Date("2016-01-01", "%Y-%m-%d"),
               by = "12 months"))
}

#
#  Plot total number of edits per month
#
plot_by_month <- function(am_selected) {
    xy_by_month <- am_selected %>%
                     group_by(year, month) %>%
                     summarize(intensity = n(),
                                   date0 = first(date))

    return(ggplot(xy_by_month, aes(x = date0,
                                   y = intensity)) +
        geom_point(size = 1.5) +
        scale_y_log10() +
        scale_x_date(breaks = date_labels(),
                     labels = date_format("%Y")) +
        labs(x = "", y = "Edits/month (log scale)\n"))
}

pt_size <- function(x_nrows) {
    # 3 630 000  edits <->  point size = 0.5
    #    20 000  edits <-> point size = 0.90
    x1 <- 3629209.0
    y1 <- 0.50
    x0 <- 20000.0
    y0 <- 0.90
    return((y1-y0) * (x_nrows-x0)/(x1 - x0) + y0)
}

plot_all_edits <- function(am_selected) {

    # Order `unique_id` ids by first-tag-date. Add `entry_count` to
    # this list.
    e_counts <- am_selected %>%
        group_by(unique_id) %>%
        summarize(sec0 = min(sec1970)) %>%
        ungroup() %>%
        arrange(sec0) %>%
        select(unique_id)
    e_counts$entry_count <- 1:nrow(e_counts)

    # Spread a map element's `entry_count` value (=height in graph) to
    # all versions of that map element.
    counts_w_versions <- inner_join(x = am_selected,
                            y = e_counts,
                            by = c('unique_id'))

    # Discretize edits by week. Here we could also do date0 = min(date),
    # but taking min:s of ints (as below) is faster.
    tmp <- counts_w_versions %>%
        group_by(year, week, entry_count) %>%
        summarize(edit_intensity = n(),
                  sec0 = min(sec1970)) %>%
        ungroup() %>%
        mutate(date0 = from_epoch(sec0)) %>%
        select(date0, entry_count, edit_intensity)

    # normalize edit_intensity to 0.8 ... 1.0. This will be our alpha.
    tmp$edit_intensity <- tmp$edit_intensity - min(tmp$edit_intensity)
    assert(max(tmp$edit_intensity) > 0)
    tmp$edit_intensity <- (tmp$edit_intensity / max(tmp$edit_intensity)) * 0.2
    tmp$edit_intensity <- tmp$edit_intensity + 0.8
    assert(min(tmp$edit_intensity) >= 0.799)
    assert(max(tmp$edit_intensity) <= 1.001)

    return(ggplot(tmp, aes(x = date0,
                           y = entry_count,
                           alpha = edit_intensity)) +
        geom_point(size = pt_size(nrow(am_selected))) +
        theme(legend.position = "none") +
        scale_x_date(breaks = date_labels(),
                     labels = date_format("%Y")) +
        labs(x = "", y = "Map element number\n"))
}

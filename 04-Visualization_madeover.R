shhh <- function(expr) suppressPackageStartupMessages(suppressWarnings(suppressMessages(expr)))
shhh({
    library(magrittr)
    library(lubridate)
    library(scales)
    library(RColorBrewer)
    library(dplyr)
    library(tidyr)
    library(polloi)
    library(ggplot2)
    library(ggrepel)
})

file <- "metrics/metrics.tsv"
pageview <- read.csv(file, sep = '\t')
pageview$month <- as.Date(pageview$month, format = "%Y-%m-%d")

head(pageview)

tail(pageview)

pageview_fy <- pageview %>%
    select(month,total_pageview,previews_seen) %>%
    mutate(previews_seen = replace_na(previews_seen, 0)) %>%
    mutate(interactions = total_pageview+previews_seen) %>%
    filter(month >= '2018-07-01') %>%
    mutate(fiscal_year = ifelse(month >= '2019-07-01' & month < '2020-07-01', 'FY 2019/20', ifelse(month <'2019-07-01','FY 2018/19',ifelse(month <'2021-07-01','FY 2020/21','FY 2021/22'))),
           MonthN =as.factor(format(as.Date(month),"%m")),
           Month = months(as.Date(month), abbreviate=TRUE))

pageview_fy$MonthN = factor(pageview_fy$MonthN, levels=c("07","08", "09", "10", "11", "12", "01", "02", "03", "04", "05", "06"))

pageview_fy %<>%
    mutate(
        dataloss_affected = case_when(
            `month` >= "2021-06-01" & `month` < "2021-11-01" ~ TRUE,
            `month` >= "2021-11-01" & `month` < "2022-02-01" ~ TRUE,
            TRUE ~ FALSE
        ),
        fiscal_year_expanded = case_when(
            dataloss_affected ~ paste(fiscal_year, "(undercounted)"),
            TRUE ~ fiscal_year
        )
    )

# For continuous line:
pageview_fy <- bind_rows(
    pageview_fy,
    pageview_fy %>%
        filter(`month` >= "2022-01-01" & `month` <= "2022-02-01") %>%
        mutate(fiscal_year_expanded = fiscal_year_expanded[1]) %>%
        filter(`month` == "2022-02-01")
)

tail(pageview_fy)

source("supplementary_files/pageview_dataloss_helper_functions.R")

pageviews_dataloss <- readr::read_tsv("supplementary_files/pageview_dataloss.tsv.gz", show_col_types = FALSE)

loss_bounds <- pageviews_dataloss %>%
    loss_boundaries() %>%
    bind_rows(.id = "which_loss")

loss_bounds

dataloss_estimates_fy <- pageview_fy %>%
    mutate(
        which_loss = case_when(
            `month` >= "2021-06-01" & `month` < "2021-11-01" ~ "loss1",
            `month` >= "2021-11-01" & `month` < "2022-02-01" ~ "loss2",
            TRUE ~ "none"
        )
    ) %>%
    # Include Feb 2022 for continuity:
    filter(which_loss != "none" | `month` == "2022-02-01") %>%
    left_join(loss_bounds, by = "which_loss") %>%
    mutate(
        estimated_pageview_lower = total_pageview / (1 - lower),
        estimated_pageview_middle = total_pageview / (1 - middle),
        estimated_pageview_upper = total_pageview / (1 - upper),
        estimated_interactions_lower = previews_seen + estimated_pageview_lower,
        estimated_interactions_middle = previews_seen + estimated_pageview_middle,
        estimated_interactions_upper = previews_seen + estimated_pageview_upper
    )

options(repr.plot.width = 16, repr.plot.height = 8)

year_colors <- list(
    "y-3" = "#BDD7E7",
    "y-2" = "#6BAED6",
    "y-1" = "#3182BD",
    "y-0" = "#08519C",
    "undercount" = "gray60"
)

axis_x_text_current <- as.numeric(levels(pageview_fy$MonthN)) == month(max(pageview_fy$month))

gg <- ggplot(pageview_fy, aes(x = MonthN)) +
    geom_line(
        aes(
            y = interactions,
            group = fiscal_year_expanded,
            color = fiscal_year_expanded,
            linetype = fiscal_year_expanded
        ),
        size = 1.5
    ) +
    geom_point(
        aes(
            y = interactions,
            group = fiscal_year_expanded,
            color = fiscal_year_expanded,
            size = dataloss_affected
        ),
        data = pageview_fy %>%
            filter(
                fiscal_year == "FY 2021/22",
                !(fiscal_year_expanded == "FY 2021/22 (undercounted)" & `month` == "2022-02-01")
            )
    ) +
    geom_ribbon(
        data = dataloss_estimates_fy %>%
            filter(fiscal_year == "FY 2021/22"),
        aes(
            ymin = estimated_interactions_lower,
            ymax = estimated_interactions_upper,
            group = fiscal_year, fill = fiscal_year
        ),
        alpha = 0.8
    )

gg <- gg +
    scale_linetype_manual(values = c(
        "FY 2018/19" = "longdash",
        "FY 2019/20" = "twodash",
        "FY 2020/21" = "dashed",
        "FY 2021/22" = "solid",
        "FY 2021/22 (undercounted)" = "solid"
    ), guide = "none") +
    scale_size_manual(
        values = c("TRUE" = 3, "FALSE" = 5),
        guide = "none"
    ) +
    scale_fill_manual(values = c(
        "FY 2021/22" = year_colors[["y-0"]]
    ), guide = "none") +
    # last 4 colors from brewer.pal(5, "Blues")
    # refer to display.brewer.pal(5, "Blues")
    scale_color_manual(values = c(
        "FY 2018/19" = year_colors[["y-3"]],
        "FY 2019/20" = year_colors[["y-2"]],
        "FY 2020/21" = year_colors[["y-1"]],
        "FY 2021/22" = year_colors[["y-0"]],
        "FY 2021/22 (undercounted)" = year_colors[["undercounted"]]
    ), guide = "none") +
    scale_x_discrete(
        "Month",
        breaks = pageview_fy$MonthN,
        labels = pageview_fy$Month,
        expand = expansion(mult = 0.1)
    ) +
    scale_y_continuous(
        "Content Interactions",
        labels = polloi::compress,
        limits = c(13E9, 23E9)
    ) +
    ggthemes::theme_clean(base_family = "Montserrat") +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(
            angle = 0, hjust = 0, face = "bold",
            margin = margin(r = -9, unit = "cm")
        ),
        axis.title = element_text(size = 17, hjust = 0.5, vjust = -12),
        axis.text.x = element_text(
            hjust = 0, size = 17,
            color = ifelse(axis_x_text_current, "dodgerblue4", "gray40"),
            face = ifelse(axis_x_text_current, "bold", "plain")
        ),
        plot.margin = margin(l = 3, t = 0.5, b = 0.5, r = 0.1, unit = "cm"),
        axis.text.y = element_text(hjust = 1, size = 21),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 17, hjust = 0.0)
    )

gg_labeled <- gg +
    geom_text_repel(
        aes(
            x = MonthN, y = interactions,
            label = "FY 2021/22\n(undercounted)",
            color = fiscal_year_expanded
        ),
        data = pageview_fy %>%
            filter(fiscal_year %in% c("FY 2021/22")) %>%
            filter(Month == "Jul"),
        hjust = 0, nudge_x = 0.1,
        vjust = 0, nudge_y = -3e9,
        force = 1,
        size = 5,
        family = "Montserrat",
        fontface = 1.5,
        segment.curvature = -0.5,
        segment.square = FALSE,
        segment.shape = 1,
        segment.inflect = TRUE,
        segment.angle = -45,
        point.padding = 1,
        box.padding = 2,
        segment.ncp = 1
    ) +
    geom_text_repel(
        aes(
            x = MonthN, y = estimated_interactions_middle,
            label = "FY 2021/22\n(estimated)",
            color = fiscal_year
        ),
        alpha = 0.8,
        data = dataloss_estimates_fy %>%
            filter(fiscal_year %in% c("FY 2021/22")) %>%
            filter(Month == "Jul"),
        hjust = 0, nudge_x = 0,
        vjust = 0, nudge_y = 1e9,
        force = 0.5,
        size = 5,
        family = "Montserrat",
        fontface = 1.5,
        segment.curvature = -0.5,
        segment.square = FALSE,
        segment.shape = 1,
        segment.inflect = TRUE,
        segment.angle = -45,
        segment.alpha = 0.6,
        point.padding = 1,
        box.padding = 2,
        segment.ncp = 1
    ) +
    geom_text_repel(
        aes(
            x = MonthN, y = interactions,
            label = fiscal_year_expanded,
            color = fiscal_year_expanded
        ),
        data = pageview_fy %>%
            filter(fiscal_year %in% c("FY 2020/21", "FY 2021/22", "FY 2018/19", "FY 2019/20")) %>%
            filter(fiscal_year_expanded != "FY 2020/21 (undercounted)") %>%
            group_by(fiscal_year) %>%
            top_n(1, MonthN) %>%
            ungroup(),
        hjust = 0, nudge_x = 0.5, size = 5,
        family = "Montserrat",
        fontface = 1.5,
        point.size = NA,
        point.padding = 1,
        box.padding = 1,
        arrow = arrow(length = unit(0.015, "npc"), type = "closed")
    )

ggsave(
    plot = gg_labeled,
    filename = "content_interactions_makeover.png",
    path = "~/Desktop",
    width = 20,
    height = 8,
    units = "in",
    dpi = 150
)


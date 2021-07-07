# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Figures script
# April 2019
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./data/analysis/sample_processed'
'./data/models/fit_twopl_irt_summary'
'./data/models/sample_irt'
'./data/analysis/cvap_prop'
'./data/analysis/vep_prop'
'./data/analysis/rvp_prop'
'./data/analysis/sample_analysis'
'./data/analysis/analysis_categorized'
'./data/analysis/raw_subgroups'
'./data/models/pred_*'
'./data/models/poststrat_*'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./figures/map_sample.pdf'
'./figures/irt.pdf'
'./figures/sample.pdf'
'./figures/euler.pdf'
'./figures/timeseries.pdf'
'./figures/predictive_val_1.pdf'
'./figures/density.pdf'
'./figures/qqplot.pdf'
'./figures/smvp_density.pdf'
'./figures/subgroups.png'
'./figures/subgroups_voter_types.png'
'./figures/subgroups_poststrat.png'
'./figures/subgroups_interactions_*.png'
'./figures/subgroups_interactions_*.pdf'
'./figures/model_rc_*.pdf'
'./figures/subgroups_altmeasures.png'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 37 - PREPARATIONS
Line 86 - MAP OF SAMPLE (APPENDIX FIGURE A2)
Line 255 - IRT PLOT (APPENDIX FIGURE D2)
Line 445 - SAMPLE COMPARISON PLOT (PAPER FIGURE 1)
Line 545 - EULER DIAGRAMM (PAPER FIGURE 2)
Line 777 - TIME SERIES PLOT (APPENDIX FIGURE G1)
Line 889 - PREDICTIVE VALIDATION PLOT (APPENDIX FIGURE E2)
Line 960 - DENSITY PLOT (APPENDIX FIGURE G2)
Line 1016 - QUANTILE-QUANTILE PLOT (APPENDIX FIGURE G3)
Line 1061 - SMVP DENSITY PLOT (PAPER FIGURE 3)
Line 1110 - SUBGROUP PLOT (PAPER FIGURE 4)
Line 1201 - SUBGROUP VOTER TYPES PLOT (PAPER FIGURE 5)
Line 1443 - SUBGROUP PLOT POSTSTRATIFIED (APPENDIX FIGURE G4)
Line 1529 - SUBGROUP INTERACTIONS PLOTS (APPENDIX FIGURES G6-G8)
Line 2005 - SUBGROUP WITH VOTING PROPENSITY PLOTS (APPENDIX FIGURES G9-G15)
Line 4668- SUBGROUP PLOTS WITH ALTERNATIVE THRESHOLDS (APPENDIX FIGURE G5)
Line 4916 - SUBGROUP INTERACTION PLOTS FOR VOTER TYPES (APPENDIX FIGURES G16-31)
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")

# import fonts --------------------------------------------------------------------------
# to get the fonts as in the paper, run (on Windows OS):
# font_import()
# loadfonts()
# loadfonts(device = "postscript")
# loadfonts(device = "win")


#### MAP OF SAMPLE (APPENDIX FIGURE A2) =================================================

# retrieve shape files and process ------------------------------------------------------
florida_counties <- us_counties(resolution = c("high"), states = "Florida") %>%
  as("Spatial") %>%
  fortify(region = "name")
florida_districts <- us_congressional(resolution = c("high"), states = "Florida") %>%
  as("Spatial") %>%
  fortify(region = "cd115fp")
florida_state <- us_states(resolution = c("high"), states = "Florida") %>%
  as("Spatial") %>%
  fortify(region = "statefp")

# retrieve city coordinates and population figures --------------------------------------
florida_cities <- us_cities(states = "Florida")
florida_cities$long <- unlist(lapply(florida_cities$geometry, `[`, 1))
florida_cities$lat <- unlist(lapply(florida_cities$geometry, `[`, 2))
florida_cities$population <- as.numeric(florida_cities$population)

# draw state border ---------------------------------------------------------------------
map_border <- ggplot(data = florida_state, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "gray40", size = 0.2, fill = NA) +
  coord_map("albers", lat0=30, lat1=40)

# import data
sample_processed <- readRDS("./data/analysis/sample_processed")

# compute district deviation from equal sample distribution
district_pop <- sample_processed %>% 
  group_by(congressional_district) %>% 
  summarize(pop = n())
district_pop$popoff <- c(770361, 731845, 739130, 800494, 729175, 764500, 772969,
                         756993, 868945, 830014, 770114, 779151, 730650, 776940, 
                         804652, 814209, 778091, 772184, 814074, 776092, 788240,
                         757352, 753950, 770480, 780826, 797160, 755809)
district_pop$pop <- district_pop$pop/(nrow(sample_processed)/27)-1
district_pop$popoff <- district_pop$popoff/(sum(district_pop$popoff)/27)-1
district_pop$congressional_district <- as.character(district_pop$congressional_district)
district_pop$congressional_district[1:9] <- c("01","02","03","04","05","06","07",
                                              "08","09")
key_district_pop <- district_pop$pop
key_district_popoff <- district_pop$popoff
names(key_district_pop) <- district_pop$congressional_district
names(key_district_popoff) <- district_pop$congressional_district
florida_districts$pop <- recode(florida_districts$id, !!!key_district_pop)
florida_districts$popoff <- recode(florida_districts$id, !!!key_district_popoff)
labels_district <- data.frame(label = c("R", "R", "R", "R", "D", "R", "D", "R", "D", 
                                        "D", "R", "R", "D", "D", "R", "R", "R", "R", 
                                        "R", "D", "D", "D", "D", "D", "R", "R/D", "R/D"))
centroids_districts <- us_congressional(resolution = c("high"), states = "Florida") %>%
  as("Spatial")
labels_district <- labels_district[as.numeric(centroids_districts@data$cd115fp),]
labels_district <- cbind(labels_district, 
                         as.data.frame(gCentroid(centroids_districts, byid = TRUE)))

# draw plot
map_left <- map_border +
  geom_polygon(data = florida_districts, color = "gray40", size = 0.2, fill = NA) +
  geom_point(data = sample_processed, aes(x = lon, y = lat, group = NA),
             color = "black", size = 0.01, alpha = 0.1, stroke = 0) +
  geom_point(data = florida_cities, aes(x = long, y = lat, group = NA, size = population),
             color = "gray70", alpha = 0.5, stroke = 0) +
  scale_size_continuous("population", range = c(0.5, 5)) +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        legend.position = "none",
        # remove legend
        text = element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.x = element_text(margin=margin(1,0,0,0,"mm")),
        axis.text.y = element_text(margin=margin(0,1,0,0,"mm")),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 15, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 15, color = "black")) +
  # specify axis title size and margin
  labs(x = "", y = "Latitude") +
  scale_y_continuous(limits = c(24.2, 31.1), expand = c(0, 0), breaks = c(25.5, 27, 28.5, 30)) +
  scale_x_continuous(limits = c(-87.8, -80), expand = c(0, 0), breaks = c(-87.0, -85.0, -83.0, -81.0))

# deviation from equal population distibution
map_middle <- map_border +
  geom_polygon(data = florida_districts, aes(fill = pop), color = "gray40", size = 0.2) +
  scale_fill_gradient(low='white', high='grey20', limits = c(-1,1)) +
  geom_polygon(data = florida_districts, color = "gray40", size = 0.2, fill = NA) +
  geom_text(data = labels_district, aes(x = x, y = y, label = labels_district), inherit.aes = FALSE,
            family="CMU Serif") +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        #legend.position = "none",
        # remove legend
        text = element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 12, color = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        # specify axis text size
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.15, 0.25),
        legend.text = element_text(margin = margin(l = 5)),
        legend.title = element_blank()) +
  # specify axis title size and margin
  labs(x = "Longitude", y = "") +
  scale_y_continuous(limits = c(24.2, 31.1), expand = c(0, 0), breaks = c(25.5, 27, 28.5, 30)) +
  scale_x_continuous(limits = c(-87.8, -80), expand = c(0, 0), breaks = c(-87.0, -85.0, -83.0, -81.0))

map_right <- map_border +
  geom_polygon(data = florida_districts, aes(fill = popoff), color = "gray40", size = 0.2) +
  scale_fill_gradient(low='white', high='grey20', limits = c(-1,1)) +
  geom_polygon(data = florida_districts, color = "gray40", size = 0.2, fill = NA) +
  geom_text(data = labels_district, aes(x = x, y = y, label = labels_district), inherit.aes = FALSE,
            family="CMU Serif") +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        # remove legend
        text = element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks.y = element_blank(),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.x = element_text(margin=margin(1,0,0,0,"mm")),
        axis.text.y = element_blank(),
        # specify axis text size
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.15, 0.25),
        legend.text = element_text(margin = margin(l = 5)),
        legend.title = element_blank()) +
  # specify axis title size and margin
  labs(x = "", y = "") +
  scale_y_continuous(limits = c(24.2, 31.1), expand = c(0, 0), breaks = c(25.5, 27, 28.5, 30)) +
  scale_x_continuous(limits = c(-87.8, -80), expand = c(0, 0), breaks = c(-87.0, -85.0, -83.0, -81.0))

a <- map_left + theme(plot.margin = unit(c(0, -6, -0.44, 0), "cm"))
b <- map_middle + theme(plot.margin = unit(c(0, 0, -0.22, -4), "cm"))
d <- map_right + theme(plot.margin = unit(c(0, 0, 0.5, -4), "cm"))

map_sample <- grid.arrange(ggplotGrob(a), rbind(ggplotGrob(b), ggplotGrob(d), size = "first"), ncol = 2,
                           widths=c(6,4),
                           bottom = textGrob("Longitude", gp=gpar(fontsize=14,fontfamily="CMU Serif")))

ggsave("./figures/map_sample.pdf", map_sample, width = 16, height = 8, dpi = 1200, device = cairo_pdf)


#### IRT PLOT (APPENDIX FIGURE D2) ======================================================
fit_twopl_irt_summary <- readRDS("./data/models/fit_twopl_irt_summary")
sample_irt <- readRDS("./data/models/sample_irt")
# select discrimination and difficulty parameters
item_parameters <- subset(fit_twopl_irt_summary, 
                          subset = str_detect(row.names(fit_twopl_irt_summary), "alpha\\[|beta\\["))
# data
icc_data <- data.frame(theta = rep(seq(-2.5,2.5,0.05), 14),
                       elections = rep(unique(sample_irt$election), each = 101),
                       alpha = rep(item_parameters[1:14,]$mean, each = 101),
                       alpha_lower = rep(item_parameters[1:14,]$`2.5%`, each = 101),
                       alpha_upper = rep(item_parameters[1:14,]$`97.5%`, each = 101),
                       beta = rep(item_parameters[15:28,]$mean, each = 101),
                       beta_lower = rep(item_parameters[15:28,]$`2.5%`, each = 101),
                       beta_upper = rep(item_parameters[15:28,]$`97.5%`, each = 101),
                       type = rep(c("midterm", "presidential", "midterm", 
                                    "presidential", "midterm", "presidential",
                                    "midterm", "mprimary", "gprimary", "mprimary", 
                                    "gprimary", "mprimary", "gprimary", "mprimary"), each = 101))
# compute turnout probabilities
icc_data$pr <- arm::invlogit(icc_data$alpha*(icc_data$theta - icc_data$beta))
icc_data$pr_upper <- arm::invlogit(icc_data$alpha_lower*(icc_data$theta - icc_data$beta_lower))
icc_data$pr_lower <- arm::invlogit(icc_data$alpha_upper*(icc_data$theta - icc_data$beta_upper))
levels(icc_data$type)
icc_data$type <- factor(icc_data$type, levels = c("presidential", "midterm", "gprimary", "mprimary"))


g <- ggplot() + 
  geom_line(data = icc_data, aes(x = theta, y = pr, group = elections, color = type), size = 1) +
  scale_colour_manual(labels = c("  presidential", "  midterms", "  pres. primaries", "  mid. primaries"), values = c("black", "gray40", "gray70", "gray90")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      # remove grids
      text=element_text(family="CMU Serif"),
      # specify font family
      panel.border = element_rect(fill=NA, color = "black", size=.5),
      # create panel borders
      axis.ticks = element_line(color = "black", size =.25),
      axis.ticks.length = unit(0.25, "cm"),
      # specify appearance of ticks
      axis.text = element_text(size = 10, color = "black"),
      # specify axis text size
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
      legend.position = c(0.9, 0.2),
      legend.title=element_blank(),
      legend.text=element_text(size=9),
      panel.spacing = unit(0, "lines")) +
  labs(y = "Turnout probability") +
  # specify axis labels
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = c(seq(0.1,0.9, 0.2))) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.5,2.5)) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "gen_2006" & round(icc_data$pr, 2) == 0.49,]$theta-0.05, 
                    y = 0.5, label = "2006", family="CMU Serif", 
                    size = 4, color = "black", angle = 50) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "gen_2008" & round(icc_data$pr, 2) == 0.49,]$theta-0.05, 
                    y = 0.5, label = "2008", family="CMU Serif", 
                    size = 4, color = "black", angle = 43) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "gen_2010" & round(icc_data$pr, 2) == 0.51,]$theta-0.08, 
                    y = 0.5, label = "2010", family="CMU Serif", 
                    size = 4, color = "black", angle = 57) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "gen_2012" & round(icc_data$pr, 2) == 0.49,]$theta-0.05, 
                    y = 0.5, label = "2012", family="CMU Serif", 
                    size = 4, color = "black", angle = 48) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "gen_2014" & round(icc_data$pr, 2) == 0.49,]$theta-0.05, 
                    y = 0.5, label = "2014", family="CMU Serif", 
                    size = 4, color = "black", angle = 54) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "gen_2016" & round(icc_data$pr, 2) == 0.51,]$theta-0.09, 
                    y = 0.5, label = "2016", family="CMU Serif", 
                    size = 4, color = "black", angle = 48) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "gen_2018" & round(icc_data$pr, 2) == 0.49,]$theta-0.05, 
                    y = 0.5, label = "2018", family="CMU Serif", 
                    size = 4, color = "black", angle = 41) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "pri_2006" & round(icc_data$pr, 2) == 0.49,]$theta-0.05, 
                    y = 0.5, label = "2006", family="CMU Serif", 
                    size = 4, color = "black", angle = 45) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "pri_2008" & round(icc_data$pr, 2) == 0.49,]$theta, 
                    y = 0.5, label = "2008", family="CMU Serif", 
                    size = 4, color = "black", angle = 54) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "pri_2010" & round(icc_data$pr, 2) == 0.49,]$theta-0.05, 
                    y = 0.5, label = "2010", family="CMU Serif", 
                    size = 4, color = "black", angle = 57) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "pri_2012" & round(icc_data$pr, 2) == 0.49,]$theta, 
                    y = 0.5, label = "2012", family="CMU Serif", 
                    size = 4, color = "black", angle = 57) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "pri_2014" & round(icc_data$pr, 2) == 0.52,]$theta-0.02, 
                    y = 0.5, label = "2014", family="CMU Serif", 
                    size = 4, color = "black", angle = 57) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "pri_2016" & round(icc_data$pr, 2) == 0.49,]$theta-0.05, 
                    y = 0.5, label = "2016", family="CMU Serif", 
                    size = 4, color = "black", angle = 54) +
  ggplot2::annotate("text", 
                    x = icc_data[icc_data$elections == "pri_2018" & round(icc_data$pr, 2) == 0.50,]$theta-0.07, 
                    y = 0.5, label = "2018", family="CMU Serif", 
                    size = 4, color = "black", angle = 55)
  
thetas <- subset(fit_twopl_irt_summary, 
                 subset = !str_detect(row.names(fit_twopl_irt_summary), "alpha|beta|lp"))

g2 <- ggplot() + 
  stat_density(data = thetas, aes(x = `50%`), geom = "area", fill = "gray70", color = "black", size = 0.5, alpha = 0.5) +
  #  facet_grid(panel~., scale="free_y", space = "free_y") +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        legend.position = c(0.9, 0.6),
        legend.title=element_blank(),
        panel.spacing = unit(0, "lines")) +
  labs(y = "Density", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.63), breaks = c(seq(0.1,0.5, 0.2))) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.5,2.5), breaks = c(-2,-1,0,1,2))

fit_twopl_irt_summary <- readRDS("./data/models/fit_twopl_irt_summary")
thetas <- subset(fit_twopl_irt_summary, 
                 subset = !str_detect(row.names(fit_twopl_irt_summary), "alpha|beta|lp"))
thetas <- arrange(thetas, mean)
thetas$index <- 1:length(thetas$mean)
rm(fit_twopl_irt_summary)

g3 <- ggplot(data = thetas) +
  geom_errorbar(aes(x = index, ymax = `90%`, ymin = `10%`, 
                    width = 0), color = "gray70", size = 0.1, position=position_dodge(width = .3)) +
  geom_point(aes(y = `50%`, x = index), shape = 20, color = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 4.1, 0, 0), "mm"), size = 12, color = "black"),
        legend.position = c(0.9, 0.6),
        legend.title=element_blank(),
        panel.spacing = unit(0, "lines")) +
  labs(y = "Propensity to vote", x = "Voters") +
  scale_x_continuous(expand = c(0, 0), limits = c(1,102241), breaks = c(seq(25000,75000, 25000)),
                     labels = c("25k", "50k", "75k")) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-2,-1,0,1,2)) +
  coord_flip(ylim = c(-2.5,2.5))

pl = list(g + theme(plot.margin = unit(c(0.2,0.2,-0.26,0.2), "cm")),
          g2 + theme(plot.margin = unit(c(0,0.2,-1.5,0.2), "cm")),
          g3 + theme(plot.margin = unit(c(0,0.2,0.2,0.2), "cm")))
IRT <- grid.arrange(grobs = pl, heights=c(4, 1, 1.7), ncol = 1)

ggsave("./figures/irt.pdf", IRT, width = 9, height = 6, dpi = 1200, device = cairo_pdf)


#### SAMPLE COMPARISON PLOT (PAPER FIGURE 1) ============================================

# cvap estimated with acs and pums data
cvap <- readRDS("./data/analysis/cvap_prop")
# vep corrects further for correctional population
vep <- readRDS("./data/analysis/vep_prop")
# rvp
rvp <- readRDS("./data/analysis/rvp_prop")
# sp
sp <- readRDS("./data/analysis/sp_prop")
sp2 <- readRDS("./data/analysis/sp_prop2")
sp3 <- readRDS("./data/analysis/sp_prop3")
# format data for plotting
# common frame with variable, estimate, moe, group
cvap$variable <- as.character(cvap$variable)
cvap <- cvap[-1,]
cvap$group <- "cvap"
vep$variable <- as.character(vep$variable)
vep <- vep[-1,]
vep$group <- "vep"
rvp$variable <- as.character(rvp$variable)
rvp <- rvp[-c(7,17:24),]
rvp[14,1] <- "otherp" 
rvp$group <- "rvp" 
sp$variable <- as.character(sp$variable)
sp2$variable <- as.character(sp2$variable)
sp3$variable <- as.character(sp3$variable)
sp$estimate <- sp$estimate*100
sp2$estimate <- sp2$estimate*100
sp3$estimate <- sp3$estimate*100
sp <- sp[-c(16:21),]
sp2 <- sp2[-c(16:21),]
sp3 <- sp3[-c(16:21),]
sp[14,1] <- "otherp" 
sp2[14,1] <- "otherp" 
sp3[14,1] <- "otherp" 
sp$moe <- 0
sp2$moe <- 0
sp3$moe <- 0
sp$group <- "sp"
sp2$group <- "sp2"
sp3$group <- "sp3"
off <- data.frame(variable = c("to_pri_2018", "to_gen_2018"), 
                  estimate = c(27, 63), 
                  moe = c(0, 0), 
                  group = c("off", "off"))
#bind all together
pop_data <- rbind(cvap, vep, rvp, sp2, off)
pop_data <- filter(pop_data, variable != "male")
pop_data$group <- factor(pop_data$group, levels = c("off", "rvp", "cvap", "vep", "sp2"), 
                         labels = c("Official", "Registered-voter pop.", "Citizen-voting-age pop.",
                                    "Voting-eligible pop.", "Sample"))
# plot
sample <- ggplot(pop_data, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = estimate - moe,
                                ymax = estimate + moe), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = estimate, shape = group), size = 2.3, fill = "white", 
              position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 15.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(2.5, 6.5, 10.5, 15.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.792, 0.913),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(limits = c("to_gen_2018", "to_pri_2018", "otherp", "npa", "republican", "democrat", "other", "white", "hispanic", "black", "age_60plus", "age_50_59", 
                              "age_35_49", "age_25_34", "age_18_24", "female"),
                   labels = c("age_60plus" = "60 +", "age_50_59" = "50-59", "age_35_49" = "35-49", 
                              "age_25_34" = "25-34", "age_18_24" = "18-24", "female" = "Female",
                              "black" = "Black", "hispanic" = "Hispanic", "white" = "White", "other" = "Other",
                              "democrat" = "Democrat", "republican" = "Republican", "npa" = "None",
                              "otherp" = "Other", "to_pri_2018" = "Turnout \n 2018 (Pri)", 
                              "to_gen_2018" = "Turnout \n 2018 (Gen)"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,2), limits = c(0,100),
                     breaks = seq(10, 90, by = 20)) +
  labs(y = "", x = "Turnout           Party                    Race                      Age               Sex")

ggsave("./figures/sample.pdf", sample, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)


#### EULER DIAGRAMM (PAPER FIGURE 2) ====================================================

# import data and subset
sample_analysis <- readRDS("./data/analysis/sample_analysis") %>%
  dplyr::select(eligible_gen_2016, eligible_gen_2018, 
                eligible_pri_2016, eligible_pri_2018, voted_gen_2016, 
                voted_gen_2018, voted_pri_2016, voted_pri_2018,
                theta_median, polact_full_a1:polact_elwp5_a4, terminated)

# prepare euler data for 2016 primary and general elections
euler_pri_2016 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2016 == TRUE) %>%
  dplyr::select(voted_pri_2016, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE)
euler_gen_2016 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2016 == TRUE) %>%
  dplyr::select(voted_gen_2016, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE)
euler_pri_2018 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
                                 pop = TRUE)
euler_gen_2018 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE)
# generate and store area-proportional Euler diagrams
eulerdiag_pri_2016 <- euler(euler_pri_2016)   
saveRDS(eulerdiag_pri_2016, "./figures/eulerdiag_pri_2016")
eulerdiag_gen_2016 <- euler(euler_gen_2016)   
saveRDS(eulerdiag_gen_2016, "./figures/eulerdiag_gen_2016")
eulerdiag_pri_2018 <- euler(euler_pri_2018)   
saveRDS(eulerdiag_pri_2018, "./figures/eulerdiag_pri_2018")
eulerdiag_gen_2018 <- euler(euler_gen_2018)   
saveRDS(eulerdiag_gen_2018, "./figures/eulerdiag_gen_2018")

# prepare euler plots
eulerplot_pri_2016 <- plot(readRDS("./figures/eulerdiag_pri_2016"),
                           quantities = FALSE, edges = TRUE, labels = FALSE,
                           fills = c("transparent","transparent","transparent",
                                      "transparent","transparent", "transparent",
                                      "gray80"),
                           lty = c(1,2,0), lwd = 2)
eulerplot_gen_2016 <- plot(readRDS("./figures/eulerdiag_gen_2016"),
                           quantities = FALSE, edges = TRUE, labels = FALSE,
                           fills = c("transparent","transparent","transparent",
                                     "transparent","transparent", "transparent",
                                     "gray80"),
                           lty = c(1,2,0), lwd = 2)
eulerplot_pri_2018 <- plot(readRDS("./figures/eulerdiag_pri_2018"),
                           quantities = FALSE, edges = TRUE, labels = FALSE,
                           fills = c("transparent","transparent","transparent",
                                     "transparent","transparent", "transparent",
                                     "gray80"),
                           lty = c(1,2,0), lwd = 2)
eulerplot_gen_2018 <- plot(readRDS("./figures/eulerdiag_gen_2018"),
                           quantities = FALSE, edges = TRUE, labels = FALSE,
                           fills = c("transparent","transparent","transparent",
                                     "transparent","transparent", "transparent",
                                     "gray80"),
                           lty = c(1,2,0), lwd = 2)
# plot
eulerplot_pri_2016 <- ggplot(data = data.frame()) +
  geom_blank() +
  geom_hline(yintercept = 0.93) +
  theme_bw() +
  coord_fixed() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_blank(),
        # specify appearance of ticks
        axis.text = element_blank(),
        # specify axis text size
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  xlim(0, 1) + ylim(0, 1) +
  annotation_custom(grob = eulerplot_pri_2016, xmin = 0.055, xmax = 1.055, ymin = -0.045, ymax = 0.955) +
  ggplot2::annotate("text", x = 0.5, y = 1, label = "2016 primary election (pres.)", family="CMU Serif", 
            size = 5, color = "black") +
  ggplot2::annotate("text", x = 0.7, y = 0.82, label = "34.5%", family="CMU Serif", 
                  size = 5, color = "black") +
  ggplot2::annotate("text", x = 0.82, y = 0.07, label = "65.5%", family="CMU Serif", 
                    size = 5, color = "black") +
  ggplot2::annotate("segment", x = 0.58, xend = 0.7, y = 0.5, yend = 0.78,
           size = 1, colour = "black") +
  ggplot2::annotate("segment", x = 0.72, xend = 0.8, y = 0.35, yend = 0.1,
                    size = 1, colour = "black")
eulerplot_gen_2016 <- ggplot(data = data.frame()) +
  geom_blank() +
  geom_hline(yintercept = 0.93) +
  theme_bw() +
  coord_fixed() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_blank(),
        # specify appearance of ticks
        axis.text = element_blank(),
        # specify axis text size
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  xlim(0, 1) + ylim(0, 1) +
  annotation_custom(grob = eulerplot_gen_2016, xmin = 0.006, xmax = 1.006, ymin = -0.045, ymax = 0.955) +
  ggplot2::annotate("text", x = 0.5, y = 1, label = "2016 general election (pres.)", family="CMU Serif", 
                    size = 5, color = "black") +
  ggplot2::annotate("text", x = 0.93, y = 0.81, label = "90.9%", family="CMU Serif", 
                  size = 5, color = "black") +
  ggplot2::annotate("text", x = 0.92, y = 0.07, label = "9.1%", family="CMU Serif", 
                    size = 5, color = "black") +
  ggplot2::annotate("segment", x = 0.82, xend = 0.92, y = 0.5, yend = 0.78,
                    size = 1, colour = "black") +
  ggplot2::annotate("segment", x = 0.95, xend = 0.9, y = 0.4, yend = 0.1,
                    size = 1, colour = "black")
eulerplot_pri_2018 <- ggplot(data = data.frame()) +
  geom_blank() +
  geom_hline(yintercept = 0.93) +
  theme_bw() +
  coord_fixed() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_blank(),
        # specify appearance of ticks
        axis.text = element_blank(),
        # specify axis text size
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  xlim(0, 1) + ylim(0, 1) +
  annotation_custom(grob = eulerplot_pri_2018, xmin = -0.05, xmax = 0.95, ymin = -0.05, ymax = 0.95) +
  ggplot2::annotate("text", x = 0.5, y = 1, label = "2018 primary election (mid.)", family="CMU Serif", 
                    size = 5, color = "black") +
  ggplot2::annotate("text", x = 0.71, y = 0.82, label = "46%", family="CMU Serif", 
                    size = 5, color = "black") +
  ggplot2::annotate("text", x = 0.82, y = 0.07, label = "54%", family="CMU Serif", 
                    size = 5, color = "black") +
  ggplot2::annotate("segment", x = 0.6, xend = 0.7, y = 0.5, yend = 0.78,
                    size = 1, colour = "black") +
  ggplot2::annotate("segment", x = 0.75, xend = 0.8, y = 0.35, yend = 0.1,
                    size = 1, colour = "black")
eulerplot_gen_2018 <- ggplot(data = data.frame()) +
  geom_blank() +
  geom_hline(yintercept = 0.93) +
  theme_bw() +
  coord_fixed() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_blank(),
        # specify appearance of ticks
        axis.text = element_blank(),
        # specify axis text size
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  xlim(0, 1) + ylim(0, 1) +
  annotation_custom(grob = eulerplot_gen_2018, xmin = -0.004, xmax = 0.996, ymin = -0.06, ymax = 0.94) +
  ggplot2::annotate("text", x = 0.5, y = 1, label = "2018 general election (mid.)", family="CMU Serif", 
                    size = 5, color = "black") +
  ggplot2::annotate("text", x = 0.85, y = 0.81, label = "73.5%", family="CMU Serif", 
                    size = 5, color = "black") +
  ggplot2::annotate("text", x = 0.85, y = 0.07, label = "26.5%", family="CMU Serif", 
                    size = 5, color = "black") +
  ggplot2::annotate("segment", x = 0.72, xend = 0.82, y = 0.5, yend = 0.78,
                    size = 1, colour = "black") +
  ggplot2::annotate("segment", x = 0.88, xend = 0.83, y = 0.4, yend = 0.1,
                    size = 1, colour = "black")

# create separate legend -----
# legend content
legend_content <- data.frame(a = factor(c(" Voted in election      ", " Social media-based participation (2018/19)"),
                      levels = c(" Voted in election      ", " Social media-based participation (2018/19)")),
           b = c(0.5, 0.5),
           d = c(0.2, 0.1))
# build legend
legend_extra <- ggplot(data = legend_content) +
  geom_line(aes(x = b, y = d, linetype = a),
            size = 1.5) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.key.width = unit(2, "line"),
        legend.key.height = unit(1, "cm"),
        legend.text=element_text(size=13),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(l = 0.507, r = 0.507, unit = "cm"))
# extract legend
legend_extra <- get_legend(legend_extra) %>%
  as_ggplot()

# bind together
a <- eulerplot_pri_2016 + theme(plot.margin = unit(c(0, -0.08, -0.49, 0), "cm"))
b <- eulerplot_gen_2016 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
d <- eulerplot_pri_2018 + theme(plot.margin = unit(c(-0.49, -0.08, 0, 0), "cm"))
e <- eulerplot_gen_2018 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

euler <- grid.arrange(cbind(ggplotGrob(a), ggplotGrob(b), size = "first"),
                      cbind(ggplotGrob(d), ggplotGrob(e), size = "first"),
                      nrow = 2)
euler <- as_ggplot(euler)
g <- euler + theme(plot.margin = unit(c(0, 0, -7.5, 0), "cm"))
f <- legend_extra + theme(plot.margin = unit(c(-0.45, -0.1, -7, 0), "cm"))

euler_final <- grid.arrange(ggplotGrob(g), ggplotGrob(f),
                  nrow = 2)

ggsave("./figures/euler.pdf", euler_final, width = 5.9028, height = 6.6, dpi = 1200, device = cairo_pdf)


##### TIME SERIES PLOT (APPENDIX FIGURE G1) =============================================

# import data
analysis_categorized <- readRDS("./data/analysis/analysis_categorized")
# add week indicator
analysis_categorized$week <- analysis_categorized$created %>%
  cut(breaks = "week", start.on.monday = FALSE) %>%
  as.Date
# join voting data
sample_analysis <- readRDS("./data/analysis/sample_analysis") %>%
  dplyr::select(twitter_id, eligible_gen_2018, eligible_pri_2018, 
                voted_gen_2018, voted_pri_2018, terminated)
colnames(analysis_categorized)[3] <- "twitter_id"
analysis_categorized <- left_join(x = analysis_categorized,
                                  y = sample_analysis,
                                  by = "twitter_id")
# remove terminated accounts and those inelligible in 2018
analysis_categorized <- analysis_categorized %>% 
  filter(terminated == FALSE & eligible_gen_2018 == TRUE)
# build time series of politically active on social media
time_full <- analysis_categorized %>%
  filter(polact == 1)
time_all <- time_full %>%
  group_by(week) %>%
  distinct(twitter_id, .keep_all = TRUE) %>%
  summarize(count = n())
time_notvoted_gen <- time_full %>%
  filter(voted_gen_2018 == FALSE) %>%
  group_by(week) %>%
  distinct(twitter_id, .keep_all = TRUE) %>%
  summarize(count_notvoted = n()) %>%
  mutate(voted = "no")
time_voted_gen <- time_full %>%
  filter(voted_gen_2018 == TRUE) %>%
  group_by(week) %>%
  distinct(twitter_id, .keep_all = TRUE) %>%
  summarize(count_voted = n()) %>%
  mutate(voted = "yes")
time_notvoted_pri <- time_full %>%
  filter(voted_pri_2018 == FALSE) %>%
  group_by(week) %>%
  distinct(twitter_id, .keep_all = TRUE) %>%
  summarize(count = n()) %>%
  mutate(voted = "no")
time_voted_pri <- time_full %>%
  filter(voted_pri_2018 == TRUE) %>%
  group_by(week) %>%
  distinct(twitter_id, .keep_all = TRUE) %>%
  summarize(count = n()) %>%
  mutate(voted = "yes")
timeseries <- rbind(time_voted_pri, time_notvoted_pri)
timeseries2 <- cbind(time_all, time_voted_gen, time_notvoted_gen)
timeseries2 <- timeseries2[,-c(3,6)]
timeseries3 <- cbind(time_all, time_voted_pri, time_notvoted_pri)
colnames(timeseries3)[c(4,7)] <- c("voted", "notvoted")
timeseries2$share_gen <- (timeseries2$count_voted/timeseries2$count)
timeseries3$share_pri <- (timeseries3$voted/timeseries3$count)
# rescale counts to overlay plots
timeseries2$count2 <- scales::rescale(timeseries2$count, to = c(0, 1), from = c(0,5000))
timeseries2$electionweek <- c(rep("no", 3), "yes", rep("no", 9), "yes", rep("no", 21))

# produce graph
timeseries_plot <- ggplot() +
  geom_bar(data = timeseries, aes(fill = voted, y = count, x = week), stat="identity",
           position="fill", width=7, fill = "white", color = "gray90") +
  geom_errorbar(data = timeseries2, aes(x = week, ymin = share_gen, ymax = share_gen, color = "Proportion voted in general election"),
                size = 1.5) +
  geom_errorbar(data = timeseries3[,c(1,9)], aes(x = week, ymin = share_pri, ymax = share_pri, color = "Proportion voted in primary election"), 
                size = 1.5) +
  scale_color_manual(values = c("gray40", "gray80")) +
  geom_line(data = timeseries2, aes(x = week,y = count2), 
            linetype = 2) +
  geom_point(data = timeseries2, aes(x = week,y = count2), shape = 21, color = "black", fill = "white", size = 2.5) +
  geom_point(data = timeseries2[c(4,14),], aes(x = week,y = count2, fill = "Election week"), shape = 21, color = "black", size = 2.5) +
  scale_fill_manual(values = c("black", "black")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        #legend.position = "none",
        # remove legend
        text = element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 17, color = "black"),
        axis.text.x = element_text(margin=margin(1,0,0,0,"mm")),
        axis.text.y = element_text(margin=margin(0,1,0,0,"mm")),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 17, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 17, color = "black"),
        legend.position = c(0.3855, 0.122),
        #legend.title=element_blank(),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.box = "horizontal",
        legend.key.width = unit(2, "line")) +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # specify axis title size and margin
  labs(x = "", y = "Citizens participating on social media ", fill = "", color = "Composition") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0.2,0.8,0.2), labels = c("1k", "2k", "3k", "4k")) +
  scale_x_date(expand = c(0, 0), breaks = date(c("2018/09/01", "2018/10/01", "2018/11/01", "2018/12/01", "2019/01/01", "2019/02/01", "2019/03/01")), date_labels = "%b %y")
  
ggsave("./figures/timeseries.pdf", timeseries_plot, width = 8, height = 4.5, dpi = 1200, device = cairo_pdf)


#### PREDICTIVE VALIDATION PLOT (APPENDIX FIGURE E2) ====================================

# import data
analysis_categorized <- readRDS("./data/analysis/analysis_categorized")
# join voting data
sample_analysis <- readRDS("./data/analysis/sample_analysis")
sample_analysis <- filter(sample_analysis, !terminated == TRUE)
sample_analysis <- filter(sample_analysis, !is.na(income_pci))
sample_analysis <- sample_analysis[-which(is.na(sample_analysis$theta_median)),]
sample_analysis <- sample_analysis %>%
  dplyr::select(twitter_id, theta_median)
colnames(analysis_categorized)[3] <- "twitter_id"
analysis_categorized <- left_join(x = analysis_categorized,
                                  y = sample_analysis,
                                  by = "twitter_id")
analysis_categorized <- analysis_categorized %>%
  filter(polact == 1 & !is.na(theta_median))
daily_ts <- analysis_categorized %>%
  group_by(created) %>%
  summarize(count = n())

daily_ts$group <- FALSE
daily_ts[which(daily_ts$created %in% as.Date(c("2018-11-06", "2018-11-07", "2018-11-08", "2018-09-27", "2018-09-28",
                                      "2019-02-05", "2019-02-06", "2018-10-05", "2018-10-06", "2018-08-28",
                                      "2018-08-29", "2018-08-25", "2018-08-26", "2019-01-09", "2019-01-08",
                                      "2018-10-02", "2018-10-03", "2018-09-23", "2018-09-24", "2018-09-25", 
                                      "2018-09-26", "2018-09-06", "2018-10-24", "2018-10-25", "2018-10-26", 
                                      "2018-08-21", "2018-08-22", "2019-02-27", "2019-03-25", "2018-09-17", 
                                      "2018-12-20", "2018-12-21", "2018-12-22"))),]$group <- TRUE

annotations <- daily_ts[daily_ts$group == TRUE,]
annotations <- annotations[-c(1, 3, 5,9,11,12,13,15,18,20,21,22,24,25,27,28,30), ]
annotations$y <- annotations$count + 800
annotations$label <- seq(1,16,1)

predictive_val_1 <- ggplot() +
  geom_segment(data = daily_ts, aes(y = count, x = created, xend = created, yend = 0, color = group, size  = group), stat="identity") +
  scale_color_manual(values = c("gray40", "black")) +
  scale_size_manual(values = c(0.5, 0.8)) +
  geom_point(data = annotations, aes(y = y, x = created), shape = 21, size = 7) +
  geom_text(data = annotations, aes(y = y, x = created, label = label)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        legend.position = "none",
        # remove legend
        text = element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 17, color = "black"),
        axis.text.x = element_text(margin=margin(1,0,0,0,"mm")),
        axis.text.y = element_text(margin=margin(0,1,0,0,"mm")),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 17, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 17, color = "black")) +
  # specify axis title size and margin
  labs(x = "Date", y = "Count") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,20000), breaks = c(5000,10000,15000), labels = c("5k", "10k", "15k")) +
  scale_x_date(expand = c(0, 0), breaks = as.Date(c("2018-09-01", "2018-10-01",
                                                    "2018-11-01", "2018-12-01",
                                                    "2019-01-01", "2019-02-01",
                                                    "2019-03-01")), date_labels = "%m/%y")

ggsave("./figures/predictive_val_1.pdf", predictive_val_1, width = 10, height = 5.5, dpi = 1200, device = cairo_pdf)


#### DENSITY PLOT (APPENDIX FIGURE G2) ==================================================
# import data
sample_analysis <- readRDS("./data/analysis/sample_analysis")
# prepare data
gen_voted <- sample_analysis %>%
  filter(polact_full_a1 == 1 & terminated == FALSE & eligible_gen_2018 == TRUE & voted_gen_2018 == TRUE) %>%
  mutate(group = "Voted in general election")
gen_notvoted <- sample_analysis %>%
  filter(polact_full_a1 == 1 & terminated == FALSE & eligible_gen_2018 == TRUE & voted_gen_2018 == FALSE) %>%
  mutate(group = "Not voted in general election")
pri_voted <- sample_analysis %>%
  filter(polact_full_a1 == 1 & terminated == FALSE & eligible_pri_2018 == TRUE & voted_pri_2018 == TRUE) %>%
  mutate(group = "Voted in primary election")
pri_notvoted <- sample_analysis %>%
  filter(polact_full_a1 == 1 & terminated == FALSE & eligible_pri_2018 == TRUE & voted_pri_2018 == FALSE) %>%
  mutate(group = "Not voted in primary election")
dens_dat <- rbind(gen_voted, gen_notvoted, pri_voted, pri_notvoted)

density_plot <- ggplot() +
  stat_density(data = dens_dat, aes(x = theta_median, color = group, linetype = group), 
               geom = "line", size = 1, alpha = 0.7, position="identity", adjust = 2) +
  scale_color_manual(values = c("black", "gray60", "black", "gray60")) +
  scale_linetype_manual(values = c("dashed", "dashed", "solid", "solid")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 17, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 17, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 17, color = "black"),
        legend.position = c(0.793, 0.844),
        legend.text=element_text(size=13),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key.width = unit(2, "line"),
        legend.title=element_blank(),
        panel.spacing = unit(0, "lines")) +
  labs(y = "Density", x = "Propensity to vote") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = seq(0.2,0.8, 0.2)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))

ggsave("./figures/density.pdf", density_plot, width = 8, height = 4.5, dpi = 1200, device = cairo_pdf)


#### QUANTILE-QUANTILE PLOT (APPENDIX FIGURE G3) ========================================

# import sample for analysis
sample_analysis <- readRDS("./data/analysis/sample_analysis")
# remove terminated/protected accounts
sample_analysis <- filter(sample_analysis, !terminated == TRUE)
# remove voters with missing values (e.g., recorded income, theta)
sample_analysis <- filter(sample_analysis, !is.na(income_pci))
# remove NAs (not eligible in 2016, or registered in 2018), cannot be handled by Stan and 
# should not be considered in model anyway, also no theta for those
sample_analysis <- sample_analysis[-which(is.na(sample_analysis$theta_median)),]

p <- seq(0,1,length.out = 500)
b <- data.frame(quantile = quantile(sample_analysis[sample_analysis$polact_full_a2 == 1,]$theta_median, p), group = "5 political posts")
d <- data.frame(quantile = quantile(sample_analysis[sample_analysis$polact_full_a3 == 1,]$theta_median, p), group = "10 political posts")
e <- data.frame(quantile = quantile(sample_analysis[sample_analysis$polact_full_a4 == 1,]$theta_median, p), group = "25 political posts")
quantiles <- data.frame(baseline = rep(quantile(sample_analysis[sample_analysis$polact_full_a1 == 1,]$theta_median, p), times = 3))
quantiles <- cbind(quantiles, rbind(b,d,e))


qqplotc <- ggplot() +
  geom_point(data = quantiles, aes(x = baseline, y = quantile), shape = 1, size = 1.5, alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~ group) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 17),
      # remove grids
      text=element_text(family="CMU Serif"),
      # specify font family
      panel.border = element_rect(fill=NA, color = "black", size=.5),
      # create panel borders
      axis.ticks = element_line(color = "black", size =.25),
      axis.ticks.length = unit(0.25, "cm"),
      axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 17, color = "black"),
      axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 17, color = "black"),
      # specify appearance of ticks
      axis.text = element_text(size = 17, color = "black")) +
  labs(y = "Alternative measures", x = "Selected measure (based on 1 political post)")

ggsave("./figures/qqplot.pdf", qqplotc, width = 8, height = 3.5, dpi = 1200, device = cairo_pdf)


#### SMVP DENSITY PLOT (PAPER FIGURE 3) =================================================
sample_ml <- readRDS("./data/models/sample_ml")

sample_density <- sample_ml[sample_ml$polact == 1,]
sample_density$group <- "Politically engaged\non social media"
sample_density2 <- sample_ml
sample_density2$group <- "Full sample"
sample_density <- rbind(sample_density, sample_density2)

smvp_density <- ggplot() + 
  stat_density(data = sample_density, aes(x = theta, linetype = group), geom = "line", size = 1, 
               color = "black", size = 0.1, position = "identity") +
 # stat_density(data = sample_ml, aes(x = theta), geom = "area", fill = NA, linetype = "dashed", color = "black", size = 0.5, alpha = 0.5) +
  #  facet_grid(panel~., scale="free_y", space = "free_y")
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 17, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 17, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 17, color = "black"),
        legend.position = c(0.838, 0.813),
        legend.text=element_text(size=13, margin = margin(t = 0.2, b = 0.2, l = -0.4, unit = "cm")),
        legend.title=element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.box = "horizontal",
        legend.key.width = unit(2, "line"), 
        legend.spacing.x = unit(1.0, 'cm'),
        panel.spacing = unit(0, "lines")) +
  labs(y = "Density", x = "Voting propensity") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.64), breaks = c(seq(0.1,0.5, 0.2))) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.5,2.5), breaks = c(-2,-1,0,1,2))

ggsave("./figures/smvp_density.pdf", smvp_density, width = 8, height = 3.5, dpi = 1200, device = cairo_pdf)


##### SUBGROUP PLOT (PAPER FIGURE 4) ====================================================
raw_subgroups <- readRDS("./data/analysis/raw_subgroups")
pred_male <- readRDS("./data/models/pred_2_male")
pred_male$variable <- "male"
pred_female <- readRDS("./data/models/pred_2_female")
pred_female$variable <- "female"
pred_dem <- readRDS("./data/models/pred_2_dem")
pred_dem$variable <- "dem"
pred_rep <- readRDS("./data/models/pred_2_rep")
pred_rep$variable <- "rep"
pred_npa <- readRDS("./data/models/pred_2_npa")
pred_npa$variable <- "none"
pred_white <- readRDS("./data/models/pred_2_white")
pred_white$variable <- "white"
pred_black <- readRDS("./data/models/pred_2_black")
pred_black$variable <- "black"
pred_hispanic <- readRDS("./data/models/pred_2_hispanic")
pred_hispanic$variable <- "hispanic"
pred_other <- readRDS("./data/models/pred_2_otherrace")
pred_other$variable <- "other"
pred_to_15 <- readRDS("./data/models/pred_2_to_15")
pred_to_15$variable <- "<=15k"
pred_15_30 <- readRDS("./data/models/pred_2_15_30")
pred_15_30$variable <- "15-30"
pred_30_50 <- readRDS("./data/models/pred_2_30_50")
pred_30_50$variable <- "30-50"
pred_50_75 <- readRDS("./data/models/pred_2_50_75")
pred_50_75$variable <- "50-75"
pred_75plus <- readRDS("./data/models/pred_2_75plus")
pred_75plus$variable <- "75+"
pred_18_29 <- readRDS("./data/models/pred_2_18_29")
pred_18_29$variable <- "18-29"
pred_30_44 <- readRDS("./data/models/pred_2_30_44")
pred_30_44$variable <- "30-44"
pred_45_64 <- readRDS("./data/models/pred_2_45_64")
pred_45_64$variable <- "45-64"
pred_65plus <- readRDS("./data/models/pred_2_65plus")
pred_65plus$variable <- "65+"

model_subgroups <- rbind(pred_male, pred_female, pred_dem, pred_rep, pred_npa,
                     pred_white, pred_black, pred_hispanic, pred_other,
                     pred_to_15, pred_15_30, pred_30_50, pred_50_75,
                     pred_75plus, pred_18_29, pred_30_44, pred_45_64,
                     pred_65plus)
model_subgroups$variable <-factor(model_subgroups$variable, 
                                  levels = c("75+", "50-75", "30-50", "15-30",
                                             "<=15k", "none", "rep", "dem",
                                             "other", "white", "hispanic",
                                             "black", "65+", "45-64", "30-44",
                                             "18-29", "female", "male"))
subgroups <- ggplot() + 
  geom_point(data = model_subgroups, aes(x = variable, y = y*100), color = "black", 
             position = position_jitter(width = 0.3), alpha = I(1/sqrt(4000))) +
  geom_point(data = raw_subgroups, aes(x = variable, y = share), color = "gray60", size = 2.3) +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5, 16.5), linetype = "dotted", colour = "gray20") +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.2, 0.2),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("65+" = "65 +", "female" = "Female", "male" = "Male",
                              "black" = "Black", "hispanic" = "Hispanic", "white" = "White", "other" = "Other",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "15-30" = "15k-30k", "30-50" = "30k-50k", "50-75" = "50k-75k",
                              "75+" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,25),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "        Income               Party              Race                 Age            Sex")

ggsave("./figures/subgroups.png", subgroups, width = 6.5, height = 8.5, dpi = 320)


##### SUBGROUP VOTER TYPES PLOT (PAPER FIGURE 5) ========================================

pred_male_low <- readRDS("./data/models/pred_low_male")
pred_male_low$variable <- "male"
pred_female_low <- readRDS("./data/models/pred_low_female")
pred_female_low$variable <- "female"
pred_dem_low <- readRDS("./data/models/pred_low_dem")
pred_dem_low$variable <- "dem"
pred_rep_low <- readRDS("./data/models/pred_low_rep")
pred_rep_low$variable <- "rep"
pred_npa_low <- readRDS("./data/models/pred_low_npa")
pred_npa_low$variable <- "none"
pred_white_low <- readRDS("./data/models/pred_low_white")
pred_white_low$variable <- "white"
pred_black_low <- readRDS("./data/models/pred_low_black")
pred_black_low$variable <- "black"
pred_hispanic_low <- readRDS("./data/models/pred_low_hispanic")
pred_hispanic_low$variable <- "hispanic"
pred_other_low <- readRDS("./data/models/pred_low_otherrace")
pred_other_low$variable <- "other"
pred_to_15_low <- readRDS("./data/models/pred_low_to_15")
pred_to_15_low$variable <- "<=15k"
pred_15_30_low <- readRDS("./data/models/pred_low_15_30")
pred_15_30_low$variable <- "15-30"
pred_30_50_low <- readRDS("./data/models/pred_low_30_50")
pred_30_50_low$variable <- "30-50"
pred_50_75_low <- readRDS("./data/models/pred_low_50_75")
pred_50_75_low$variable <- "50-75"
pred_75plus_low <- readRDS("./data/models/pred_low_75plus")
pred_75plus_low$variable <- "75+"
pred_18_29_low <- readRDS("./data/models/pred_low_18_29")
pred_18_29_low$variable <- "18-29"
pred_30_44_low <- readRDS("./data/models/pred_low_30_44")
pred_30_44_low$variable <- "30-44"
pred_45_64_low <- readRDS("./data/models/pred_low_45_64")
pred_45_64_low$variable <- "45-64"
pred_65plus_low <- readRDS("./data/models/pred_low_65plus")
pred_65plus_low$variable <- "65+"

pred_male_ire <- readRDS("./data/models/pred_ire_male")
pred_male_ire$variable <- "male"
pred_female_ire <- readRDS("./data/models/pred_ire_female")
pred_female_ire$variable <- "female"
pred_dem_ire <- readRDS("./data/models/pred_ire_dem")
pred_dem_ire$variable <- "dem"
pred_rep_ire <- readRDS("./data/models/pred_ire_rep")
pred_rep_ire$variable <- "rep"
pred_npa_ire <- readRDS("./data/models/pred_ire_npa")
pred_npa_ire$variable <- "none"
pred_white_ire <- readRDS("./data/models/pred_ire_white")
pred_white_ire$variable <- "white"
pred_black_ire <- readRDS("./data/models/pred_ire_black")
pred_black_ire$variable <- "black"
pred_hispanic_ire <- readRDS("./data/models/pred_ire_hispanic")
pred_hispanic_ire$variable <- "hispanic"
pred_other_ire <- readRDS("./data/models/pred_ire_otherrace")
pred_other_ire$variable <- "other"
pred_to_15_ire <- readRDS("./data/models/pred_ire_to_15")
pred_to_15_ire$variable <- "<=15k"
pred_15_30_ire <- readRDS("./data/models/pred_ire_15_30")
pred_15_30_ire$variable <- "15-30"
pred_30_50_ire <- readRDS("./data/models/pred_ire_30_50")
pred_30_50_ire$variable <- "30-50"
pred_50_75_ire <- readRDS("./data/models/pred_ire_50_75")
pred_50_75_ire$variable <- "50-75"
pred_75plus_ire <- readRDS("./data/models/pred_ire_75plus")
pred_75plus_ire$variable <- "75+"
pred_18_29_ire <- readRDS("./data/models/pred_ire_18_29")
pred_18_29_ire$variable <- "18-29"
pred_30_44_ire <- readRDS("./data/models/pred_ire_30_44")
pred_30_44_ire$variable <- "30-44"
pred_45_64_ire <- readRDS("./data/models/pred_ire_45_64")
pred_45_64_ire$variable <- "45-64"
pred_65plus_ire <- readRDS("./data/models/pred_ire_65plus")
pred_65plus_ire$variable <- "65+"

pred_male_reg <- readRDS("./data/models/pred_re_male")
pred_male_reg$variable <- "male"
pred_female_reg <- readRDS("./data/models/pred_re_female")
pred_female_reg$variable <- "female"
pred_dem_reg <- readRDS("./data/models/pred_re_dem")
pred_dem_reg$variable <- "dem"
pred_rep_reg <- readRDS("./data/models/pred_re_rep")
pred_rep_reg$variable <- "rep"
pred_npa_reg <- readRDS("./data/models/pred_re_npa")
pred_npa_reg$variable <- "none"
pred_white_reg <- readRDS("./data/models/pred_re_white")
pred_white_reg$variable <- "white"
pred_black_reg <- readRDS("./data/models/pred_re_black")
pred_black_reg$variable <- "black"
pred_hispanic_reg <- readRDS("./data/models/pred_re_hispanic")
pred_hispanic_reg$variable <- "hispanic"
pred_other_reg <- readRDS("./data/models/pred_re_otherrace")
pred_other_reg$variable <- "other"
pred_to_15_reg <- readRDS("./data/models/pred_re_to_15")
pred_to_15_reg$variable <- "<=15k"
pred_15_30_reg <- readRDS("./data/models/pred_re_15_30")
pred_15_30_reg$variable <- "15-30"
pred_30_50_reg <- readRDS("./data/models/pred_re_30_50")
pred_30_50_reg$variable <- "30-50"
pred_50_75_reg <- readRDS("./data/models/pred_re_50_75")
pred_50_75_reg$variable <- "50-75"
pred_75plus_reg <- readRDS("./data/models/pred_re_75plus")
pred_75plus_reg$variable <- "75+"
pred_18_29_reg <- readRDS("./data/models/pred_re_18_29")
pred_18_29_reg$variable <- "18-29"
pred_30_44_reg <- readRDS("./data/models/pred_re_30_44")
pred_30_44_reg$variable <- "30-44"
pred_45_64_reg <- readRDS("./data/models/pred_re_45_64")
pred_45_64_reg$variable <- "45-64"
pred_65plus_reg <- readRDS("./data/models/pred_re_65plus")
pred_65plus_reg$variable <- "65+"

pred_male_hen <- readRDS("./data/models/pred_he_male")
pred_male_hen$variable <- "male"
pred_female_hen <- readRDS("./data/models/pred_he_female")
pred_female_hen$variable <- "female"
pred_dem_hen <- readRDS("./data/models/pred_he_dem")
pred_dem_hen$variable <- "dem"
pred_rep_hen <- readRDS("./data/models/pred_he_rep")
pred_rep_hen$variable <- "rep"
pred_npa_hen <- readRDS("./data/models/pred_he_npa")
pred_npa_hen$variable <- "none"
pred_white_hen <- readRDS("./data/models/pred_he_white")
pred_white_hen$variable <- "white"
pred_black_hen <- readRDS("./data/models/pred_he_black")
pred_black_hen$variable <- "black"
pred_hispanic_hen <- readRDS("./data/models/pred_he_hispanic")
pred_hispanic_hen$variable <- "hispanic"
pred_other_hen <- readRDS("./data/models/pred_he_otherrace")
pred_other_hen$variable <- "other"
pred_to_15_hen <- readRDS("./data/models/pred_he_to_15")
pred_to_15_hen$variable <- "<=15k"
pred_15_30_hen <- readRDS("./data/models/pred_he_15_30")
pred_15_30_hen$variable <- "15-30"
pred_30_50_hen <- readRDS("./data/models/pred_he_30_50")
pred_30_50_hen$variable <- "30-50"
pred_50_75_hen <- readRDS("./data/models/pred_he_50_75")
pred_50_75_hen$variable <- "50-75"
pred_75plus_hen <- readRDS("./data/models/pred_he_75plus")
pred_75plus_hen$variable <- "75+"
pred_18_29_hen <- readRDS("./data/models/pred_he_18_29")
pred_18_29_hen$variable <- "18-29"
pred_30_44_hen <- readRDS("./data/models/pred_he_30_44")
pred_30_44_hen$variable <- "30-44"
pred_45_64_hen <- readRDS("./data/models/pred_he_45_64")
pred_45_64_hen$variable <- "45-64"
pred_65plus_hen <- readRDS("./data/models/pred_he_65plus")
pred_65plus_hen$variable <- "65+"

model_subgroups_low <- rbind(pred_male_low, pred_female_low, pred_dem_low, pred_rep_low, pred_npa_low,
                         pred_white_low, pred_black_low, pred_hispanic_low, pred_other_low,
                         pred_to_15_low, pred_15_30_low, pred_30_50_low, pred_50_75_low,
                         pred_75plus_low, pred_18_29_low, pred_30_44_low, pred_45_64_low,
                         pred_65plus_low)
model_subgroups_low$group <- "Low-propensity voters\n"
model_subgroups_ire <- rbind(pred_male_ire, pred_female_ire, pred_dem_ire, pred_rep_ire, pred_npa_ire,
                             pred_white_ire, pred_black_ire, pred_hispanic_ire, pred_other_ire,
                             pred_to_15_ire, pred_15_30_ire, pred_30_50_ire, pred_50_75_ire,
                             pred_75plus_ire, pred_18_29_ire, pred_30_44_ire, pred_45_64_ire,
                             pred_65plus_ire)
model_subgroups_ire$group <- "Irregular voters\n"
model_subgroups_reg <- rbind(pred_male_reg, pred_female_reg, pred_dem_reg, pred_rep_reg, pred_npa_reg,
                             pred_white_reg, pred_black_reg, pred_hispanic_reg, pred_other_reg,
                             pred_to_15_reg, pred_15_30_reg, pred_30_50_reg, pred_50_75_reg,
                             pred_75plus_reg, pred_18_29_reg, pred_30_44_reg, pred_45_64_reg,
                             pred_65plus_reg)
model_subgroups_reg$group <- "Regular voters\n"
model_subgroups_hen <- rbind(pred_male_hen, pred_female_hen, pred_dem_hen, pred_rep_hen, pred_npa_hen,
                             pred_white_hen, pred_black_hen, pred_hispanic_hen, pred_other_hen,
                             pred_to_15_hen, pred_15_30_hen, pred_30_50_hen, pred_50_75_hen,
                             pred_75plus_hen, pred_18_29_hen, pred_30_44_hen, pred_45_64_hen,
                             pred_65plus_hen)
model_subgroups_hen$group <- "Core voters\n"

model_subgroups <- rbind(model_subgroups_low, model_subgroups_ire, model_subgroups_reg, model_subgroups_hen)

model_subgroups$variable <-factor(model_subgroups$variable, 
                                  levels = c("75+", "50-75", "30-50", "15-30",
                                             "<=15k", "none", "rep", "dem",
                                             "other", "white", "hispanic",
                                             "black", "65+", "45-64", "30-44",
                                             "18-29", "female", "male"))
model_subgroups$group <-factor(model_subgroups$group, 
                                  levels = c("Low-propensity voters\n",
                                             "Irregular voters\n", "Regular voters\n", 
                                             "Core voters\n"))

subgroups_voter_types <- ggplot() + 
  geom_point(data = model_subgroups, aes(x = variable, y = y*100), color = "black", 
             position = position_jitter(width = 0.3), alpha = I(1/sqrt(4000))) +
 # geom_text(data = data_labels, aes(x = x, y = x, label = labels)) +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5, 16.5), linetype = "dotted", colour = "gray20") +
  facet_wrap(~ group) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.2, 0.2),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("65+" = "65 +", "female" = "Female", "male" = "Male",
                              "black" = "Black", "hispanic" = "Hispanic", "white" = "White", "other" = "Other",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "15-30" = "15k-30k", "30-50" = "30k-50k", "50-75" = "50k-75k",
                              "75+" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,50),
                     breaks = seq(10, 40, by = 10),
                     labels = c("10%", "20%", "30%", "40%")) +
  labs(y = "", x = " Income   Party   Race   Age   Sex        Income   Party   Race   Age   Sex")
  
subgroups_voter_types <- ggdraw(subgroups_voter_types) + 
  draw_label("Pr(pri.<0.5 & mid.<0.5 & pres.<0.5)", fontfamily = "CMU Serif", 
             x = 0.40, y = 0.95, size = 11) + 
  draw_label("Pr(pri.<0.5 & mid.<0.5 & pres.>0.5)", fontfamily = "CMU Serif", 
             x = 0.796, y = 0.95, size = 11) +
  draw_label("Pr(pri.<0.5 & mid.>0.5 & pres.>0.5)", fontfamily = "CMU Serif", 
             x = 0.40, y = 0.483, size = 11) +
  draw_label("Pr(pri.>0.5 & mid.>0.5 & pres.>0.5)", fontfamily = "CMU Serif", 
             x = 0.796, y = 0.483, size = 11)

ggsave("./figures/subgroups_voter_types.png", subgroups_voter_types, width = 6.5, height = 8.5, dpi = 320)


#### SUBGROUP PLOT POSTSTRATIFIED (APPENDIX FIGURE G4) ==================================

poststrat_male <- readRDS("./data/models/poststrat_male")
poststrat_female <- readRDS("./data/models/poststrat_female")
poststrat_dem <- readRDS("./data/models/poststrat_dem")
poststrat_rep <- readRDS("./data/models/poststrat_rep")
poststrat_none <- readRDS("./data/models/poststrat_none")
poststrat_18_29 <- readRDS("./data/models/poststrat_18_29")
poststrat_30_44 <- readRDS("./data/models/poststrat_30_44")
poststrat_45_64 <- readRDS("./data/models/poststrat_45_64")
poststrat_65plus <- readRDS("./data/models/poststrat_65plus")
poststrat_white <- readRDS("./data/models/poststrat_white")
poststrat_black <- readRDS("./data/models/poststrat_black")
poststrat_hispanic <- readRDS("./data/models/poststrat_hispanic")
poststrat_other <- readRDS("./data/models/poststrat_other")
poststrat_to_15 <- readRDS("./data/models/poststrat_to_15")
poststrat_15_30 <- readRDS("./data/models/poststrat_15_30")
poststrat_30_50 <- readRDS("./data/models/poststrat_30_50")
poststrat_50_75 <- readRDS("./data/models/poststrat_50_75")
poststrat_75_plus <- readRDS("./data/models/poststrat_75_plus")
poststrat_subgroups <- data.frame(y = c(poststrat_male, poststrat_female, 
                         poststrat_dem, poststrat_rep, poststrat_none,
                         poststrat_18_29, poststrat_30_44, poststrat_45_64, 
                         poststrat_65plus,
                         poststrat_white, poststrat_black, poststrat_hispanic, 
                         poststrat_other,
                         poststrat_to_15, poststrat_15_30, poststrat_30_50, 
                         poststrat_50_75, poststrat_75_plus),
                         variable = c(rep("male", 4000), rep("female", 4000),
                                      rep("dem", 4000), rep("rep", 4000),
                                      rep("none", 4000), rep("18-29", 4000),
                                      rep("30-44", 4000), rep("45-64", 4000),
                                      rep("65+", 4000),rep("white", 4000),
                                      rep("black", 4000), rep("hispanic", 4000),
                                      rep("other", 4000), rep("<=15k", 4000),
                                      rep("15-30", 4000), rep("30-50", 4000),
                                      rep("50-75", 4000), rep("75+", 4000)
                         ))
poststrat_subgroups$variable <-factor(poststrat_subgroups$variable, 
                                  levels = c("75+", "50-75", "30-50", "15-30",
                                             "<=15k", "none", "rep", "dem",
                                             "other", "white", "hispanic",
                                             "black", "65+", "45-64", "30-44",
                                             "18-29", "female", "male"))

subgroups_poststrat <- ggplot() + 
  geom_point(data = poststrat_subgroups, aes(x = variable, y = y*100), color = "black", 
             position = position_jitter(width = 0.3), alpha = I(1/sqrt(4000))) +
 # geom_point(data = raw_subgroups, aes(x = variable, y = share), color = "gray60", size = 2.3) +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5, 16.5), linetype = "dotted", colour = "gray20") +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.2, 0.2),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("65+" = "65 +", "female" = "Female", "male" = "Male",
                              "black" = "Black", "hispanic" = "Hispanic", "white" = "White", "other" = "Other",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "15-30" = "15k-30k", "30-50" = "30k-50k", "50-75" = "50k-75k",
                              "75+" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,25),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "        Income               Party              Race                 Age            Sex")

ggsave("./figures/subgroups_poststrat.png", subgroups_poststrat, width = 6.5, height = 8.5, dpi = 320)


#### SUBGROUP INTERACTIONS PLOTS (APPENDIX FIGURES G6-G8) ===============================

# import population averaged predictions for interacted subgroups -----------------------
pred_white_to_15 <- readRDS("./data/models/pred_white_to_15")
pred_white_15_30 <- readRDS("./data/models/pred_white_15_30")
pred_white_30_50 <- readRDS("./data/models/pred_white_30_50")
pred_white_50_75 <- readRDS("./data/models/pred_white_50_75")
pred_white_75_plus <- readRDS("./data/models/pred_white_75_plus")
pred_black_to_15 <- readRDS("./data/models/pred_black_to_15")
pred_black_15_30 <- readRDS("./data/models/pred_black_15_30")
pred_black_30_50 <- readRDS("./data/models/pred_black_30_50")
pred_black_50_75 <- readRDS("./data/models/pred_black_50_75")
pred_black_75_plus <- readRDS("./data/models/pred_black_75_plus")
pred_hispanic_to_15 <- readRDS("./data/models/pred_hispanic_to_15")
pred_hispanic_15_30 <- readRDS("./data/models/pred_hispanic_15_30")
pred_hispanic_30_50 <- readRDS("./data/models/pred_hispanic_30_50")
pred_hispanic_50_75 <- readRDS("./data/models/pred_hispanic_50_75")
pred_hispanic_75_plus <- readRDS("./data/models/pred_hispanic_75_plus")
pred_other_to_15 <- readRDS("./data/models/pred_other_to_15")
pred_other_15_30 <- readRDS("./data/models/pred_other_15_30")
pred_other_30_50 <- readRDS("./data/models/pred_other_30_50")
pred_other_50_75 <- readRDS("./data/models/pred_other_50_75")
pred_other_75_plus <- readRDS("./data/models/pred_other_75_plus")
pred_race_income <- list(pred_white_to_15, pred_white_15_30, pred_white_30_50,
                          pred_white_50_75, pred_white_75_plus, pred_black_to_15,
                          pred_black_15_30, pred_black_30_50, pred_black_50_75,
                          pred_black_75_plus, pred_hispanic_to_15, pred_hispanic_15_30,
                          pred_hispanic_30_50, pred_hispanic_50_75, pred_hispanic_75_plus,
                          pred_other_to_15, pred_other_15_30, pred_other_30_50, pred_other_50_75,
                          pred_other_75_plus)
pred_race_income <- pred_race_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 4)
pred_race_income$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 5)

pred_white_18_29 <- readRDS("./data/models/pred_white_18_29")
pred_white_30_44 <- readRDS("./data/models/pred_white_30_44")
pred_white_45_64 <- readRDS("./data/models/pred_white_45_64")
pred_white_60plus <- readRDS("./data/models/pred_white_60plus")
pred_black_18_29 <- readRDS("./data/models/pred_black_18_29")
pred_black_30_44 <- readRDS("./data/models/pred_black_30_44")
pred_black_45_64 <- readRDS("./data/models/pred_black_45_64")
pred_black_60plus <- readRDS("./data/models/pred_black_60plus")
pred_hispanic_18_29 <- readRDS("./data/models/pred_hispanic_18_29")
pred_hispanic_30_44 <- readRDS("./data/models/pred_hispanic_30_44")
pred_hispanic_45_64 <- readRDS("./data/models/pred_hispanic_45_64")
pred_hispanic_60plus <- readRDS("./data/models/pred_hispanic_60plus")
pred_other_18_29 <- readRDS("./data/models/pred_other_18_29")
pred_other_30_44 <- readRDS("./data/models/pred_other_30_44")
pred_other_45_64 <- readRDS("./data/models/pred_other_45_64")
pred_other_60plus <- readRDS("./data/models/pred_other_60plus")
pred_race_age <- list(pred_white_18_29, pred_white_30_44, pred_white_45_64,
                         pred_white_60plus, 
                         pred_black_18_29, pred_black_30_44, pred_black_45_64, 
                         pred_black_60plus,
                         pred_hispanic_18_29, pred_hispanic_30_44, pred_hispanic_45_64, 
                         pred_hispanic_60plus,
                         pred_other_18_29, pred_other_30_44, pred_other_45_64, 
                         pred_other_60plus)
pred_race_age<- pred_race_age %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_age$variable <- rep(c("18_29", "30_44", "45_64", "60plus"), times = 4)
pred_race_age$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 4)

pred_white_dem <- readRDS("./data/models/pred_white_dem")
pred_white_rep <- readRDS("./data/models/pred_white_rep")
pred_white_none <- readRDS("./data/models/pred_white_none")
pred_black_dem <- readRDS("./data/models/pred_black_dem")
pred_black_rep <- readRDS("./data/models/pred_black_rep")
pred_black_none <- readRDS("./data/models/pred_black_none")
pred_hispanic_dem <- readRDS("./data/models/pred_hispanic_dem")
pred_hispanic_rep <- readRDS("./data/models/pred_hispanic_rep")
pred_hispanic_none <- readRDS("./data/models/pred_hispanic_none")
pred_other_dem <- readRDS("./data/models/pred_other_dem")
pred_other_rep <- readRDS("./data/models/pred_other_rep")
pred_other_none <- readRDS("./data/models/pred_other_none")
pred_race_party <- list(pred_white_dem, pred_white_rep, pred_white_none,
                        pred_black_dem, pred_black_rep, pred_black_none, 
                        pred_hispanic_dem, pred_hispanic_rep, pred_hispanic_none, 
                        pred_other_dem, pred_other_rep, pred_other_none)
pred_race_party <- pred_race_party %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_party$variable <- rep(c("dem", "rep", "none"), times = 4)
pred_race_party$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 3)

pred_white_female <- readRDS("./data/models/pred_white_female")
pred_white_male <- readRDS("./data/models/pred_white_male")
pred_black_female <- readRDS("./data/models/pred_black_female")
pred_black_male <- readRDS("./data/models/pred_black_male")
pred_hispanic_female <- readRDS("./data/models/pred_hispanic_female")
pred_hispanic_male <- readRDS("./data/models/pred_hispanic_male")
pred_other_female <- readRDS("./data/models/pred_other_female")
pred_other_male <- readRDS("./data/models/pred_other_male")
pred_race_sex <- list(pred_white_female, pred_white_male,
                      pred_black_female, pred_black_male, 
                      pred_hispanic_female, pred_hispanic_male, 
                      pred_other_female, pred_other_male)
pred_race_sex <- pred_race_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_sex$variable <- rep(c("female", "male"), times = 4)
pred_race_sex$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 2)

pred_race <- rbind(pred_race_income, pred_race_age, pred_race_party, pred_race_sex)

pred_race$variable <-factor(pred_race$variable, 
                            levels = c("75_plus", "50_75", "30_50", "15_30",
                                       "to_15", "none", "rep", "dem",
                                       "60plus", "45_64", "30_44",
                                       "18_29", "female", "male"))
pred_race$group <-factor(pred_race$group, 
                            levels = c("Other", "White", "Hispanic", "Black"))

subgroups_interactions_1 <- ggplot(pred_race, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
   geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
   geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.9, 0.9285),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("60plus" = "65 +", "45_64" = "45-64", "30_44" = "30-44",
                              "18_29" = "18-29", "female" = "Female", "male" = "Male",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "        Income                       Party                      Age             Sex")

ggsave("./figures/subgroups_interactions_1.png", subgroups_interactions_1, width = 6.5, height = 8.5, dpi = 320)

pred_18_29_to_15 <- readRDS("./data/models/pred_18_29_to_15")
pred_18_29_15_30 <- readRDS("./data/models/pred_18_29_15_30")
pred_18_29_30_50 <- readRDS("./data/models/pred_18_29_30_50")
pred_18_29_50_75 <- readRDS("./data/models/pred_18_29_50_75")
pred_18_29_75_plus <- readRDS("./data/models/pred_18_29_75_plus")
pred_30_44_to_15 <- readRDS("./data/models/pred_30_44_to_15")
pred_30_44_15_30 <- readRDS("./data/models/pred_30_44_15_30")
pred_30_44_30_50 <- readRDS("./data/models/pred_30_44_30_50")
pred_30_44_50_75 <- readRDS("./data/models/pred_30_44_50_75")
pred_30_44_75_plus <- readRDS("./data/models/pred_30_44_75_plus")
pred_45_64_to_15 <- readRDS("./data/models/pred_45_64_to_15")
pred_45_64_15_30 <- readRDS("./data/models/pred_45_64_15_30")
pred_45_64_30_50 <- readRDS("./data/models/pred_45_64_30_50")
pred_45_64_50_75 <- readRDS("./data/models/pred_45_64_50_75")
pred_45_64_75_plus <- readRDS("./data/models/pred_45_64_75_plus")
pred_65plus_to_15 <- readRDS("./data/models/pred_65plus_to_15")
pred_65plus_15_30 <- readRDS("./data/models/pred_65plus_15_30")
pred_65plus_30_50 <- readRDS("./data/models/pred_65plus_30_50")
pred_65plus_50_75 <- readRDS("./data/models/pred_65plus_50_75")
pred_65plus_75_plus <- readRDS("./data/models/pred_65plus_75_plus")
pred_age_income <- list(pred_18_29_to_15, pred_18_29_15_30, pred_18_29_30_50,
                         pred_18_29_50_75, pred_18_29_75_plus, pred_30_44_to_15,
                         pred_30_44_15_30, pred_30_44_30_50, pred_30_44_50_75,
                         pred_30_44_75_plus, pred_45_64_to_15, pred_45_64_15_30,
                         pred_45_64_30_50, pred_45_64_50_75, pred_45_64_75_plus,
                         pred_65plus_to_15, pred_65plus_15_30, pred_65plus_30_50, 
                         pred_65plus_50_75, pred_65plus_75_plus)
pred_age_income <- pred_age_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 4)
pred_age_income$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 5)

pred_18_29_dem <- readRDS("./data/models/pred_18_29_dem")
pred_18_29_rep <- readRDS("./data/models/pred_18_29_rep")
pred_18_29_none <- readRDS("./data/models/pred_18_29_none")
pred_30_44_dem <- readRDS("./data/models/pred_30_44_dem")
pred_30_44_rep <- readRDS("./data/models/pred_30_44_rep")
pred_30_44_none <- readRDS("./data/models/pred_30_44_none")
pred_45_64_dem <- readRDS("./data/models/pred_45_64_dem")
pred_45_64_rep <- readRDS("./data/models/pred_45_64_rep")
pred_45_64_none <- readRDS("./data/models/pred_45_64_none")
pred_65plus_dem <- readRDS("./data/models/pred_65plus_dem")
pred_65plus_rep <- readRDS("./data/models/pred_65plus_rep")
pred_65plus_none <- readRDS("./data/models/pred_65plus_none")
pred_age_party <- list(pred_18_29_dem, pred_18_29_rep, pred_18_29_none,
                        pred_30_44_dem, pred_30_44_rep, pred_30_44_none, 
                        pred_45_64_dem, pred_45_64_rep, pred_45_64_none, 
                        pred_65plus_dem, pred_65plus_rep, pred_65plus_none)
pred_age_party <- pred_age_party %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_party$variable <- rep(c("dem", "rep", "none"), times = 4)
pred_age_party$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 3)

pred_18_29_female <- readRDS("./data/models/pred_18_29_female")
pred_18_29_male <- readRDS("./data/models/pred_18_29_male")
pred_30_44_female <- readRDS("./data/models/pred_30_44_female")
pred_30_44_male <- readRDS("./data/models/pred_30_44_male")
pred_45_64_female <- readRDS("./data/models/pred_45_64_female")
pred_45_64_male <- readRDS("./data/models/pred_45_64_male")
pred_65plus_female <- readRDS("./data/models/pred_65plus_female")
pred_65plus_male <- readRDS("./data/models/pred_65plus_male")
pred_age_sex <- list(pred_18_29_female, pred_18_29_male,
                      pred_30_44_female, pred_30_44_male, 
                      pred_45_64_female, pred_45_64_male, 
                      pred_65plus_female, pred_65plus_male)
pred_age_sex <- pred_age_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_sex$variable <- rep(c("female", "male"), times = 4)
pred_age_sex$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 2)

pred_age <- rbind(pred_age_income, pred_age_party, pred_age_sex)

pred_age$variable <-factor(pred_age$variable, 
                                  levels = c("75_plus", "50_75", "30_50", "15_30",
                                             "to_15", "none", "rep", "dem",
                                             "female", "male"))
pred_age$group <-factor(pred_age$group, 
                         levels = c("65 +", "45-64", "30-44", "18-29"))

subgroups_interactions_2 <- ggplot(pred_age, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.924, 0.9285),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("female" = "Female", "male" = "Male",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "            Income                                 Party                        Sex")

ggsave("./figures/subgroups_interactions_2.pdf", subgroups_interactions_2, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

pred_dem_to_15 <- readRDS("./data/models/pred_dem_to_15")
pred_dem_15_30 <- readRDS("./data/models/pred_dem_15_30")
pred_dem_30_50 <- readRDS("./data/models/pred_dem_30_50")
pred_dem_50_75 <- readRDS("./data/models/pred_dem_50_75")
pred_dem_75_plus <- readRDS("./data/models/pred_dem_75_plus")
pred_rep_to_15 <- readRDS("./data/models/pred_rep_to_15")
pred_rep_15_30 <- readRDS("./data/models/pred_rep_15_30")
pred_rep_30_50 <- readRDS("./data/models/pred_rep_30_50")
pred_rep_50_75 <- readRDS("./data/models/pred_rep_50_75")
pred_rep_75_plus <- readRDS("./data/models/pred_rep_75_plus")
pred_none_to_15 <- readRDS("./data/models/pred_none_to_15")
pred_none_15_30 <- readRDS("./data/models/pred_none_15_30")
pred_none_30_50 <- readRDS("./data/models/pred_none_30_50")
pred_none_50_75 <- readRDS("./data/models/pred_none_50_75")
pred_none_75_plus <- readRDS("./data/models/pred_none_75_plus")
pred_party_income <- list(pred_dem_to_15, pred_dem_15_30, pred_dem_30_50,
                        pred_dem_50_75, pred_dem_75_plus, pred_rep_to_15,
                        pred_rep_15_30, pred_rep_30_50, pred_rep_50_75,
                        pred_rep_75_plus, pred_none_to_15, pred_none_15_30,
                        pred_none_30_50, pred_none_50_75, pred_none_75_plus)
pred_party_income <- pred_party_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_party_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 3)
pred_party_income$group <- rep(c("Democrat", "Republican", "None"), each = 5)

pred_dem_female <- readRDS("./data/models/pred_dem_female")
pred_dem_male <- readRDS("./data/models/pred_dem_male")
pred_rep_female <- readRDS("./data/models/pred_rep_female")
pred_rep_male <- readRDS("./data/models/pred_rep_male")
pred_none_female <- readRDS("./data/models/pred_none_female")
pred_none_male <- readRDS("./data/models/pred_none_male")
pred_party_sex <- list(pred_dem_female, pred_dem_male,
                     pred_rep_female, pred_rep_male, 
                     pred_none_female, pred_none_male)
pred_party_sex <- pred_party_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_party_sex$variable <- rep(c("female", "male"), times = 3)
pred_party_sex$group <- rep(c("Democrat", "Republican", "None"), each = 2)

pred_party <- rbind(pred_party_income, pred_party_sex)

pred_party$variable <-factor(pred_party$variable, 
                           levels = c("75_plus", "50_75", "30_50", "15_30",
                                      "to_15", "female", "male"))

pred_party$group <-factor(pred_party$group, 
                        levels = c("None", "Republican", "Democrat"))

subgroups_interactions_3 <- ggplot(pred_party, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.115, 0.9435),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("female" = "Female", "male" = "Male",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "              Income                                               Sex")

ggsave("./figures/subgroups_interactions_3.pdf", subgroups_interactions_3, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

pred_female_to_15 <- readRDS("./data/models/pred_female_to_15")
pred_female_15_30 <- readRDS("./data/models/pred_female_15_30")
pred_female_30_50 <- readRDS("./data/models/pred_female_30_50")
pred_female_50_75 <- readRDS("./data/models/pred_female_50_75")
pred_female_75_plus <- readRDS("./data/models/pred_female_75_plus")
pred_male_to_15 <- readRDS("./data/models/pred_male_to_15")
pred_male_15_30 <- readRDS("./data/models/pred_male_15_30")
pred_male_30_50 <- readRDS("./data/models/pred_male_30_50")
pred_male_50_75 <- readRDS("./data/models/pred_male_50_75")
pred_male_75_plus <- readRDS("./data/models/pred_male_75_plus")
pred_sex_income <- list(pred_female_to_15, pred_female_15_30, pred_female_30_50,
                        pred_female_50_75, pred_female_75_plus, pred_male_to_15,
                        pred_male_15_30, pred_male_30_50, pred_male_50_75,
                        pred_male_75_plus)
pred_sex_income <- pred_sex_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_sex_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 2)
pred_sex_income$group <- rep(c("Female", "Male"), each = 5)

pred_sex <- pred_sex_income
pred_sex$variable <-factor(pred_sex$variable, 
                             levels = c("75_plus", "50_75", "30_50", "15_30",
                                        "to_15"))
pred_sex$group <-factor(pred_sex$group, 
                          levels = c("Female", "Male"))

subgroups_interactions_4 <- ggplot(pred_sex, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.915, 0.959),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "Income")

ggsave("./figures/subgroups_interactions_4.pdf", subgroups_interactions_4, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)


#### SUBGROUP WITH VOTING PROPENSITY PLOTS (APPENDIX FIGURES G9-G15) ===================

# import population averaged predictions for subgroups ------------------
pred_male <- readRDS("./data/models/pred_male")
pred_female <- readRDS("./data/models/pred_female")
pred_dem <- readRDS("./data/models/pred_dem")
pred_rep <- readRDS("./data/models/pred_rep")
pred_npa <- readRDS("./data/models/pred_npa")
pred_white <- readRDS("./data/models/pred_white")
pred_black <- readRDS("./data/models/pred_black")
pred_hispanic <- readRDS("./data/models/pred_hispanic")
pred_other <- readRDS("./data/models/pred_otherrace")
pred_to_15 <- readRDS("./data/models/pred_to_15")
pred_15_30 <- readRDS("./data/models/pred_15_30")
pred_30_50 <- readRDS("./data/models/pred_30_50")
pred_50_75 <- readRDS("./data/models/pred_50_75")
pred_75plus <- readRDS("./data/models/pred_75plus")
pred_18_29 <- readRDS("./data/models/pred_18_29")
pred_30_44 <- readRDS("./data/models/pred_30_44")
pred_45_64 <- readRDS("./data/models/pred_45_64")
pred_65plus <- readRDS("./data/models/pred_65plus")

# summarize inferences (95% credible intervals)
sum_pred_male <- pred_male %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_female <- pred_female %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_sex <- rbind(sum_pred_male, sum_pred_female) %>%
  mutate(group = factor(rep(c("Male", "Female"), each = 50),
                        levels = c("Female", "Male")))
sum_pred_dem <- pred_dem %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_rep <- pred_rep %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_npa <- pred_npa %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_party <- rbind(sum_pred_dem, sum_pred_rep, sum_pred_npa) %>%
  mutate(group = factor(rep(c("Democrat", "Republican", "None"), each = 50),
                        levels = c("Democrat", "None", "Republican")))
sum_pred_white <- pred_white %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_black <- pred_black %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_hispanic <- pred_hispanic %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_race <- rbind(sum_pred_white, sum_pred_black, sum_pred_hispanic) %>%
  mutate(group = factor(rep(c("White", "Black", "Hispanic"), each = 50),
                        levels = c("Black", "Hispanic", "White")))
sum_pred_18_29 <- pred_18_29 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_44 <- pred_30_44 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_45_64 <- pred_45_64 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_65plus <- pred_65plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_age <- rbind(sum_pred_18_29, sum_pred_30_44, sum_pred_45_64, sum_pred_65plus) %>%
  mutate(group = factor(rep(c("18-29", "30-44", "45-64", "65+"), each = 50),
                        levels = c("18-29", "30-44", "45-64", "65+")))
sum_pred_to_15 <- pred_to_15 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_15_30 <- pred_15_30 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_50 <- pred_30_50 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_50_75 <- pred_50_75 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_75plus <- pred_75plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_income <- rbind(sum_pred_to_15, sum_pred_15_30, sum_pred_30_50, 
                         sum_pred_50_75, sum_pred_75plus) %>%
  mutate(group = factor(rep(c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k"), each = 50),
                        levels = c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k")))

# assign average difference between all groups in one panel
avg_diffs_sex <- data.frame(x = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2), 
                            y = 0, 
                            xend = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2), 
                            yend = 0.04,
                            text = c("2.3", "2.7", "3.1", "3.7", "4.2", "4.6", "5.1", "5.4", "5.6"))
avg_diffs_race <- data.frame(x = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2), 
                             y = 0, 
                             xend = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2), 
                             yend = 0.04,
                             text = c("0.3", "0.3", "0.3", "0.5", "0.9", "1.3", "1.9", "2.4", "3.1"))
avg_diffs_age <- data.frame(x = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2), 
                            y = 0, 
                            xend = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2), 
                            yend = 0.04,
                            text = c("0.8", "1.5", "2.4", "3.6", "5.7", "7.6", "10.2", "12.7", "15.8"))
avg_diffs_party <- data.frame(x = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2), 
                              y = 0, 
                              xend = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2), 
                              yend = 0.04,
                              text = c("0.4", "0.7", "0.9", "1.6", "2.4", "3.2", "4.4", "5.5", "6.8"))
avg_diffs_income <- data.frame(x = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2), 
                               y = 0, 
                               xend = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2), 
                               yend = 0.04,
                               text = c("0.4", "0.4", "0.5", "0.6", "0.7", "0.7", "0.9", "1.0", "1.1"))

sum_pred_income$x2 <- round(sum_pred_income$x, 2)
sum(c(round(abs(sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == "<=15k",]$fit -
                  sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">15k & <=30k",]$fit)*100, 1),
      round(abs(sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == "<=15k",]$fit -
                  sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">30k & <=50k",]$fit)*100, 1),
      round(abs(sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == "<=15k",]$fit -
                  sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">50k & <=75k",]$fit)*100, 1),
      round(abs(sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == "<=15k",]$fit -
                  sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">75k",]$fit)*100, 1),
      round(abs(sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">15k & <=30k",]$fit -
                  sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">30k & <=50k",]$fit)*100, 1),
      round(abs(sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">15k & <=30k",]$fit -
                  sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">50k & <=75k",]$fit)*100, 1),
      round(abs(sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">15k & <=30k",]$fit -
                  sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">75k",]$fit)*100, 1),
      round(abs(sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">30k & <=50k",]$fit -
                  sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">50k & <=75k",]$fit)*100, 1),
      round(abs(sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">30k & <=50k",]$fit -
                  sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">75k",]$fit)*100, 1),
      round(abs(sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">50k & <=75k",]$fit -
                  sum_pred_income[sum_pred_income$x2 == 0.04 & sum_pred_income$group == ">75k",]$fit)*100, 1)))/10


# create graphs
# sex subgroups
sex <- ggplot() +
  geom_ribbon(data = sum_pred_sex, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_sex, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  #geom_segment(data = avg_diffs, aes(x = x, y = y, xend = xend, yend = yend), 
  #             color = "gray70", lwd = 1, lineend = "round") +
  #geom_text(data = avg_diffs_sex, aes(x = x-0.095, y = 0.02, label = text), angle = 90,
  #          family = "CMU Serif", size = 4.8, color = "gray30") +
  theme_bw() +
  ggtitle("Sex") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.15, 0.9),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# race subgroups
race <- ggplot() +
  geom_ribbon(data = sum_pred_race, aes(x = x, ymin = lower, ymax = upper, 
                                        group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_race, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  #geom_segment(data = avg_diffs, aes(x = x, y = y, xend = xend, yend = yend), 
  #             color = "gray70", lwd = 1, lineend = "round") +
  #geom_text(data = avg_diffs_race, aes(x = x-0.095, y = 0.02, label = text), angle = 90,
  #         family = "CMU Serif", size = 4.3, color = "gray30") +
  theme_bw() +
  ggtitle("Race") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.16, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# age subgroups
age <- ggplot() +
  geom_ribbon(data = sum_pred_age, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_age, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash")) +
  #geom_segment(data = avg_diffs, aes(x = x, y = y, xend = xend, yend = yend), 
  #             color = "gray70", lwd = 1, lineend = "round") +
  #geom_text(data = avg_diffs_age, aes(x = x-0.095, y = 0.02, label = text), angle = 90,
  #          family = "CMU Serif", size = 4.3, color = "gray30") +
  theme_bw() +
  ggtitle("Age") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.14, 0.835),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# party subgroups
party <- ggplot() +
  geom_ribbon(data = sum_pred_party, aes(x = x, ymin = lower, ymax = upper, 
                                         group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_party, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray40", "gray70", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  #geom_segment(data = avg_diffs, aes(x = x, y = y, xend = xend, yend = yend), 
  #             color = "gray70", lwd = 1, lineend = "round") +
  #geom_text(data = avg_diffs_party, aes(x = x-0.095, y = 0.02, label = text), angle = 90,
  #          family = "CMU Serif", size = 4.3, color = "gray30") +
  theme_bw() +
  ggtitle("Party") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.183, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# income
income <- ggplot() +
  geom_ribbon(data = sum_pred_income, aes(x = x, ymin = lower, ymax = upper, 
                                          group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_income, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray60", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash", "dashed")) +
  #geom_segment(data = avg_diffs, aes(x = x, y = y, xend = xend, yend = yend), 
  #             color = "gray70", lwd = 1, lineend = "round") +
  #geom_text(data = avg_diffs_income, aes(x = x-0.095, y = 0.02, label = text), angle = 90,
  #          family = "CMU Serif", size = 4.3, color = "gray30") +
  theme_bw() +
  ggtitle("Income") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.22, 0.81),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))

# bind together
a <- sex + theme(plot.margin = unit(c(0, -0.5, -0.32, 0), "cm"))
b <- race + theme(plot.margin = unit(c(0, -0.25, -0.32, -0.25), "cm"))
d <- age + theme(plot.margin = unit(c(0, 0, -0.3, -0.5), "cm"))
e <- party + theme(plot.margin = unit(c(-0.32, -0.37, 0, 6.4), "cm"))
f <- income + theme(plot.margin = unit(c(-0.32, 6.4, 0, -0.37), "cm"))
model_rc_0 <- grid.arrange(cbind(ggplotGrob(a), ggplotGrob(b), ggplotGrob(d), size = "first"),
                         cbind(ggplotGrob(e), ggplotGrob(f), size = "first"),
                         nrow = 2,
                         bottom = textGrob("Propensity to vote", 
                                           gp=gpar(fontsize=20,
                                                   fontfamily="CMU Serif"), 
                                           y = 1.3, x = 0.523),
                         left = textGrob("Probability of social media-based participation", 
                                         gp=gpar(fontsize=20,fontfamily="CMU Serif"),
                                         rot = 90, x = 1))
ggsave("model_rc_0.pdf", model_1a, width = 16, height = 10, dpi = 1200, device = cairo_pdf)


# import population averaged predictions for subgroups ------------------
pred_male <- readRDS("./data/models/pred_rc_1_male")
pred_female <- readRDS("./data/models/pred_rc_1_female")
pred_dem <- readRDS("./data/models/pred_rc_1_dem")
pred_rep <- readRDS("./data/models/pred_rc_1_rep")
pred_npa <- readRDS("./data/models/pred_rc_1_npa")
pred_white <- readRDS("./data/models/pred_rc_1_white")
pred_black <- readRDS("./data/models/pred_rc_1_black")
pred_hispanic <- readRDS("./data/models/pred_rc_1_hispanic")
pred_other <- readRDS("./data/models/pred_rc_1_otherrace")
pred_to_15 <- readRDS("./data/models/pred_rc_1_to_15")
pred_15_30 <- readRDS("./data/models/pred_rc_1_15_30")
pred_30_50 <- readRDS("./data/models/pred_rc_1_30_50")
pred_50_75 <- readRDS("./data/models/pred_rc_1_50_75")
pred_75plus <- readRDS("./data/models/pred_rc_1_75plus")
pred_18_29 <- readRDS("./data/models/pred_rc_1_18_29")
pred_30_44 <- readRDS("./data/models/pred_rc_1_30_44")
pred_45_64 <- readRDS("./data/models/pred_rc_1_45_64")
pred_65plus <- readRDS("./data/models/pred_rc_1_65plus")

# summarize inferences (95% credible intervals)
sum_pred_male <- pred_male %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_female <- pred_female %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_sex <- rbind(sum_pred_male, sum_pred_female) %>%
  mutate(group = factor(rep(c("Male", "Female"), each = 50),
                        levels = c("Female", "Male")))
sum_pred_dem <- pred_dem %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_rep <- pred_rep %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_npa <- pred_npa %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_party <- rbind(sum_pred_dem, sum_pred_rep, sum_pred_npa) %>%
  mutate(group = factor(rep(c("Democrat", "Republican", "None"), each = 50),
                        levels = c("Democrat", "None", "Republican")))
sum_pred_white <- pred_white %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_black <- pred_black %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_hispanic <- pred_hispanic %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_race <- rbind(sum_pred_white, sum_pred_black, sum_pred_hispanic) %>%
  mutate(group = factor(rep(c("White", "Black", "Hispanic"), each = 50),
                        levels = c("Black", "Hispanic", "White")))
sum_pred_18_29 <- pred_18_29 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_44 <- pred_30_44 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_45_64 <- pred_45_64 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_65plus <- pred_65plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_age <- rbind(sum_pred_18_29, sum_pred_30_44, sum_pred_45_64, sum_pred_65plus) %>%
  mutate(group = factor(rep(c("18-29", "30-44", "45-64", "65+"), each = 50),
                        levels = c("18-29", "30-44", "45-64", "65+")))
sum_pred_to_15 <- pred_to_15 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_15_30 <- pred_15_30 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_50 <- pred_30_50 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_50_75 <- pred_50_75 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_75plus <- pred_75plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_income <- rbind(sum_pred_to_15, sum_pred_15_30, sum_pred_30_50, 
                         sum_pred_50_75, sum_pred_75plus) %>%
  mutate(group = factor(rep(c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k"), each = 50),
                        levels = c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k")))

# create graphs
# sex subgroups
sex <- ggplot() +
  geom_ribbon(data = sum_pred_sex, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_sex, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  theme_bw() +
  ggtitle("Sex") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.15, 0.9),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# race subgroups
race <- ggplot() +
  geom_ribbon(data = sum_pred_race, aes(x = x, ymin = lower, ymax = upper, 
                                        group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_race, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Race") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.16, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# age subgroups
age <- ggplot() +
  geom_ribbon(data = sum_pred_age, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_age, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash")) +
  theme_bw() +
  ggtitle("Age") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.14, 0.835),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# party subgroups
party <- ggplot() +
  geom_ribbon(data = sum_pred_party, aes(x = x, ymin = lower, ymax = upper, 
                                         group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_party, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray40", "gray70", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Party") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.183, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# income
income <- ggplot() +
  geom_ribbon(data = sum_pred_income, aes(x = x, ymin = lower, ymax = upper, 
                                          group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_income, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray60", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash", "dashed")) +
  theme_bw() +
  ggtitle("Income") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.22, 0.81),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))

# bind together
a <- sex + theme(plot.margin = unit(c(0, -0.5, -0.32, 0), "cm"))
b <- race + theme(plot.margin = unit(c(0, -0.25, -0.32, -0.25), "cm"))
d <- age + theme(plot.margin = unit(c(0, 0, -0.3, -0.5), "cm"))
e <- party + theme(plot.margin = unit(c(-0.32, -0.37, 0, 6.4), "cm"))
f <- income + theme(plot.margin = unit(c(-0.32, 6.4, 0, -0.37), "cm"))
model_rc_1 <- grid.arrange(cbind(ggplotGrob(a), ggplotGrob(b), ggplotGrob(d), size = "first"),
                         cbind(ggplotGrob(e), ggplotGrob(f), size = "first"),
                         nrow = 2,
                         bottom = textGrob("Propensity to vote", 
                                           gp=gpar(fontsize=20,
                                                   fontfamily="CMU Serif"), 
                                           y = 1.3, x = 0.523),
                         left = textGrob("Probability of social media-based participation", 
                                         gp=gpar(fontsize=20,fontfamily="CMU Serif"),
                                         rot = 90, x = 1))
ggsave("./figures/model_rc_1.pdf", model_rc_1, width = 16, height = 10, dpi = 1200, device = cairo_pdf)

# import population averaged predictions for subgroups ------------------
pred_male <- readRDS("./data/models/pred_rc_2_male")
pred_female <- readRDS("./data/models/pred_rc_2_female")
pred_dem <- readRDS("./data/models/pred_rc_2_dem")
pred_rep <- readRDS("./data/models/pred_rc_2_rep")
pred_npa <- readRDS("./data/models/pred_rc_2_npa")
pred_white <- readRDS("./data/models/pred_rc_2_white")
pred_black <- readRDS("./data/models/pred_rc_2_black")
pred_hispanic <- readRDS("./data/models/pred_rc_2_hispanic")
pred_other <- readRDS("./data/models/pred_rc_2_otherrace")
pred_to_15 <- readRDS("./data/models/pred_rc_2_to_15")
pred_15_30 <- readRDS("./data/models/pred_rc_2_15_30")
pred_30_50 <- readRDS("./data/models/pred_rc_2_30_50")
pred_50_75 <- readRDS("./data/models/pred_rc_2_50_75")
pred_75plus <- readRDS("./data/models/pred_rc_2_75plus")
pred_18_29 <- readRDS("./data/models/pred_rc_2_18_29")
pred_30_44 <- readRDS("./data/models/pred_rc_2_30_44")
pred_45_64 <- readRDS("./data/models/pred_rc_2_45_64")
pred_65plus <- readRDS("./data/models/pred_rc_2_65plus")

# summarize inferences (95% credible intervals)
sum_pred_male <- pred_male %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_female <- pred_female %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_sex <- rbind(sum_pred_male, sum_pred_female) %>%
  mutate(group = factor(rep(c("Male", "Female"), each = 50),
                        levels = c("Female", "Male")))
sum_pred_dem <- pred_dem %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_rep <- pred_rep %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_npa <- pred_npa %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_party <- rbind(sum_pred_dem, sum_pred_rep, sum_pred_npa) %>%
  mutate(group = factor(rep(c("Democrat", "Republican", "None"), each = 50),
                        levels = c("Democrat", "None", "Republican")))
sum_pred_white <- pred_white %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_black <- pred_black %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_hispanic <- pred_hispanic %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_race <- rbind(sum_pred_white, sum_pred_black, sum_pred_hispanic) %>%
  mutate(group = factor(rep(c("White", "Black", "Hispanic"), each = 50),
                        levels = c("Black", "Hispanic", "White")))
sum_pred_18_29 <- pred_18_29 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_44 <- pred_30_44 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_45_64 <- pred_45_64 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_65plus <- pred_65plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_age <- rbind(sum_pred_18_29, sum_pred_30_44, sum_pred_45_64, sum_pred_65plus) %>%
  mutate(group = factor(rep(c("18-29", "30-44", "45-64", "65+"), each = 50),
                        levels = c("18-29", "30-44", "45-64", "65+")))
sum_pred_to_15 <- pred_to_15 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_15_30 <- pred_15_30 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_50 <- pred_30_50 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_50_75 <- pred_50_75 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_75plus <- pred_75plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_income <- rbind(sum_pred_to_15, sum_pred_15_30, sum_pred_30_50, 
                         sum_pred_50_75, sum_pred_75plus) %>%
  mutate(group = factor(rep(c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k"), each = 50),
                        levels = c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k")))

# create graphs
# sex subgroups
sex <- ggplot() +
  geom_ribbon(data = sum_pred_sex, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_sex, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  theme_bw() +
  ggtitle("Sex") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.15, 0.9),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.55), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# race subgroups
race <- ggplot() +
  geom_ribbon(data = sum_pred_race, aes(x = x, ymin = lower, ymax = upper, 
                                        group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_race, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Race") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.16, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.55), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# age subgroups
age <- ggplot() +
  geom_ribbon(data = sum_pred_age, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_age, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash")) +
  theme_bw() +
  ggtitle("Age") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.14, 0.835),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.55), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# party subgroups
party <- ggplot() +
  geom_ribbon(data = sum_pred_party, aes(x = x, ymin = lower, ymax = upper, 
                                         group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_party, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray40", "gray70", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Party") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.183, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.55), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# income
income <- ggplot() +
  geom_ribbon(data = sum_pred_income, aes(x = x, ymin = lower, ymax = upper, 
                                          group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_income, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray60", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash", "dashed")) +
  theme_bw() +
  ggtitle("Income") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.22, 0.81),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.55), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))

# bind together
a <- sex + theme(plot.margin = unit(c(0, -0.5, -0.32, 0), "cm"))
b <- race + theme(plot.margin = unit(c(0, -0.25, -0.32, -0.25), "cm"))
d <- age + theme(plot.margin = unit(c(0, 0, -0.3, -0.5), "cm"))
e <- party + theme(plot.margin = unit(c(-0.32, -0.37, 0, 6.4), "cm"))
f <- income + theme(plot.margin = unit(c(-0.32, 6.4, 0, -0.37), "cm"))
model_rc_2 <- grid.arrange(cbind(ggplotGrob(a), ggplotGrob(b), ggplotGrob(d), size = "first"),
                           cbind(ggplotGrob(e), ggplotGrob(f), size = "first"),
                           nrow = 2,
                           bottom = textGrob("Propensity to vote", 
                                             gp=gpar(fontsize=20,
                                                     fontfamily="CMU Serif"), 
                                             y = 1.3, x = 0.523),
                           left = textGrob("Probability of social media-based participation", 
                                           gp=gpar(fontsize=20,fontfamily="CMU Serif"),
                                           rot = 90, x = 1))
ggsave("./figures/model_rc_2.pdf", model_rc_2, width = 16, height = 10, dpi = 1200, device = cairo_pdf)

# import population averaged predictions for subgroups ------------------
pred_male <- readRDS("./data/models/pred_rc_3_male")
pred_female <- readRDS("./data/models/pred_rc_3_female")
pred_dem <- readRDS("./data/models/pred_rc_3_dem")
pred_rep <- readRDS("./data/models/pred_rc_3_rep")
pred_npa <- readRDS("./data/models/pred_rc_3_npa")
pred_white <- readRDS("./data/models/pred_rc_3_white")
pred_black <- readRDS("./data/models/pred_rc_3_black")
pred_hispanic <- readRDS("./data/models/pred_rc_3_hispanic")
pred_other <- readRDS("./data/models/pred_rc_3_otherrace")
pred_to_15 <- readRDS("./data/models/pred_rc_3_to_15")
pred_15_30 <- readRDS("./data/models/pred_rc_3_15_30")
pred_30_50 <- readRDS("./data/models/pred_rc_3_30_50")
pred_50_75 <- readRDS("./data/models/pred_rc_3_50_75")
pred_75plus <- readRDS("./data/models/pred_rc_3_75plus")
pred_18_29 <- readRDS("./data/models/pred_rc_3_18_29")
pred_30_44 <- readRDS("./data/models/pred_rc_3_30_44")
pred_45_64 <- readRDS("./data/models/pred_rc_3_45_64")
pred_65plus <- readRDS("./data/models/pred_rc_3_65plus")

# summarize inferences (95% credible intervals)
sum_pred_male <- pred_male %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_female <- pred_female %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_sex <- rbind(sum_pred_male, sum_pred_female) %>%
  mutate(group = factor(rep(c("Male", "Female"), each = 50),
                        levels = c("Female", "Male")))
sum_pred_dem <- pred_dem %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_rep <- pred_rep %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_npa <- pred_npa %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_party <- rbind(sum_pred_dem, sum_pred_rep, sum_pred_npa) %>%
  mutate(group = factor(rep(c("Democrat", "Republican", "None"), each = 50),
                        levels = c("Democrat", "None", "Republican")))
sum_pred_white <- pred_white %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_black <- pred_black %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_hispanic <- pred_hispanic %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_race <- rbind(sum_pred_white, sum_pred_black, sum_pred_hispanic) %>%
  mutate(group = factor(rep(c("White", "Black", "Hispanic"), each = 50),
                        levels = c("Black", "Hispanic", "White")))
sum_pred_18_29 <- pred_18_29 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_44 <- pred_30_44 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_45_64 <- pred_45_64 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_65plus <- pred_65plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_age <- rbind(sum_pred_18_29, sum_pred_30_44, sum_pred_45_64, sum_pred_65plus) %>%
  mutate(group = factor(rep(c("18-29", "30-44", "45-64", "65+"), each = 50),
                        levels = c("18-29", "30-44", "45-64", "65+")))
sum_pred_to_15 <- pred_to_15 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_15_30 <- pred_15_30 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_50 <- pred_30_50 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_50_75 <- pred_50_75 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_75plus <- pred_75plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_income <- rbind(sum_pred_to_15, sum_pred_15_30, sum_pred_30_50, 
                         sum_pred_50_75, sum_pred_75plus) %>%
  mutate(group = factor(rep(c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k"), each = 50),
                        levels = c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k")))

# create graphs
# sex subgroups
sex <- ggplot() +
  geom_ribbon(data = sum_pred_sex, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_sex, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  theme_bw() +
  ggtitle("Sex") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.15, 0.9),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# race subgroups
race <- ggplot() +
  geom_ribbon(data = sum_pred_race, aes(x = x, ymin = lower, ymax = upper, 
                                        group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_race, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Race") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.16, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# age subgroups
age <- ggplot() +
  geom_ribbon(data = sum_pred_age, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_age, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash")) +
  theme_bw() +
  ggtitle("Age") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.14, 0.835),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# party subgroups
party <- ggplot() +
  geom_ribbon(data = sum_pred_party, aes(x = x, ymin = lower, ymax = upper, 
                                         group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_party, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray40", "gray70", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Party") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.183, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# income
income <- ggplot() +
  geom_ribbon(data = sum_pred_income, aes(x = x, ymin = lower, ymax = upper, 
                                          group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_income, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray60", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash", "dashed")) +
  theme_bw() +
  ggtitle("Income") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.22, 0.81),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))

# bind together
a <- sex + theme(plot.margin = unit(c(0, -0.5, -0.32, 0), "cm"))
b <- race + theme(plot.margin = unit(c(0, -0.25, -0.32, -0.25), "cm"))
d <- age + theme(plot.margin = unit(c(0, 0, -0.3, -0.5), "cm"))
e <- party + theme(plot.margin = unit(c(-0.32, -0.37, 0, 6.4), "cm"))
f <- income + theme(plot.margin = unit(c(-0.32, 6.4, 0, -0.37), "cm"))
model_rc_3 <- grid.arrange(cbind(ggplotGrob(a), ggplotGrob(b), ggplotGrob(d), size = "first"),
                           cbind(ggplotGrob(e), ggplotGrob(f), size = "first"),
                           nrow = 2,
                           bottom = textGrob("Propensity to vote", 
                                             gp=gpar(fontsize=20,
                                                     fontfamily="CMU Serif"), 
                                             y = 1.3, x = 0.523),
                           left = textGrob("Probability of social media-based participation", 
                                           gp=gpar(fontsize=20,fontfamily="CMU Serif"),
                                           rot = 90, x = 1))
ggsave("./figures/model_rc_3.pdf", model_rc_3, width = 16, height = 10, dpi = 1200, device = cairo_pdf)

# import population averaged predictions for subgroups ------------------
pred_male <- readRDS("./data/models/pred_rc_4_male")
pred_female <- readRDS("./data/models/pred_rc_4_female")
pred_dem <- readRDS("./data/models/pred_rc_4_dem")
pred_rep <- readRDS("./data/models/pred_rc_4_rep")
pred_npa <- readRDS("./data/models/pred_rc_4_npa")
pred_white <- readRDS("./data/models/pred_rc_4_white")
pred_black <- readRDS("./data/models/pred_rc_4_black")
pred_hispanic <- readRDS("./data/models/pred_rc_4_hispanic")
pred_other <- readRDS("./data/models/pred_rc_4_otherrace")
pred_to_15 <- readRDS("./data/models/pred_rc_4_to_15")
pred_15_30 <- readRDS("./data/models/pred_rc_4_15_30")
pred_30_50 <- readRDS("./data/models/pred_rc_4_30_50")
pred_50_75 <- readRDS("./data/models/pred_rc_4_50_75")
pred_75plus <- readRDS("./data/models/pred_rc_4_75plus")
pred_18_29 <- readRDS("./data/models/pred_rc_4_18_29")
pred_30_44 <- readRDS("./data/models/pred_rc_4_30_44")
pred_45_64 <- readRDS("./data/models/pred_rc_4_45_64")
pred_65plus <- readRDS("./data/models/pred_rc_4_65plus")

# summarize inferences (95% credible intervals)
sum_pred_male <- pred_male %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_female <- pred_female %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_sex <- rbind(sum_pred_male, sum_pred_female) %>%
  mutate(group = factor(rep(c("Male", "Female"), each = 50),
                        levels = c("Female", "Male")))
sum_pred_dem <- pred_dem %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_rep <- pred_rep %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_npa <- pred_npa %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_party <- rbind(sum_pred_dem, sum_pred_rep, sum_pred_npa) %>%
  mutate(group = factor(rep(c("Democrat", "Republican", "None"), each = 50),
                        levels = c("Democrat", "None", "Republican")))
sum_pred_white <- pred_white %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_black <- pred_black %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_hispanic <- pred_hispanic %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_race <- rbind(sum_pred_white, sum_pred_black, sum_pred_hispanic) %>%
  mutate(group = factor(rep(c("White", "Black", "Hispanic"), each = 50),
                        levels = c("Black", "Hispanic", "White")))
sum_pred_18_29 <- pred_18_29 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_44 <- pred_30_44 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_45_64 <- pred_45_64 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_65plus <- pred_65plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_age <- rbind(sum_pred_18_29, sum_pred_30_44, sum_pred_45_64, sum_pred_65plus) %>%
  mutate(group = factor(rep(c("18-29", "30-44", "45-64", "65+"), each = 50),
                        levels = c("18-29", "30-44", "45-64", "65+")))
sum_pred_to_15 <- pred_to_15 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_15_30 <- pred_15_30 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_50 <- pred_30_50 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_50_75 <- pred_50_75 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_75plus <- pred_75plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_income <- rbind(sum_pred_to_15, sum_pred_15_30, sum_pred_30_50, 
                         sum_pred_50_75, sum_pred_75plus) %>%
  mutate(group = factor(rep(c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k"), each = 50),
                        levels = c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k")))
# create graphs
# sex subgroups
sex <- ggplot() +
  geom_ribbon(data = sum_pred_sex, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_sex, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  theme_bw() +
  ggtitle("Sex") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.15, 0.9),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# race subgroups
race <- ggplot() +
  geom_ribbon(data = sum_pred_race, aes(x = x, ymin = lower, ymax = upper, 
                                        group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_race, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray40", "black")) +
  theme_bw() +
  ggtitle("Race") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.16, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# age subgroups
age <- ggplot() +
  geom_ribbon(data = sum_pred_age, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_age, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash")) +
  theme_bw() +
  ggtitle("Age") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.14, 0.835),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# party subgroups
party <- ggplot() +
  geom_ribbon(data = sum_pred_party, aes(x = x, ymin = lower, ymax = upper, 
                                         group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_party, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray40", "gray70", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Party") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.183, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# income
income <- ggplot() +
  geom_ribbon(data = sum_pred_income, aes(x = x, ymin = lower, ymax = upper, 
                                          group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_income, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray60", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash", "dashed")) +
  theme_bw() +
  ggtitle("Income") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.22, 0.81),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.5), breaks = c(0.1,0.2,0.3,0.4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))

# bind together
a <- sex + theme(plot.margin = unit(c(0, -0.5, -0.32, 0), "cm"))
b <- race + theme(plot.margin = unit(c(0, -0.25, -0.32, -0.25), "cm"))
d <- age + theme(plot.margin = unit(c(0, 0, -0.3, -0.5), "cm"))
e <- party + theme(plot.margin = unit(c(-0.32, -0.37, 0, 6.4), "cm"))
f <- income + theme(plot.margin = unit(c(-0.32, 6.4, 0, -0.37), "cm"))
model_rc_4 <- grid.arrange(cbind(ggplotGrob(a), ggplotGrob(b), ggplotGrob(d), size = "first"),
                           cbind(ggplotGrob(e), ggplotGrob(f), size = "first"),
                           nrow = 2,
                           bottom = textGrob("Propensity to vote", 
                                             gp=gpar(fontsize=20,
                                                     fontfamily="CMU Serif"), 
                                             y = 1.3, x = 0.523),
                           left = textGrob("Probability of social media-based participation", 
                                           gp=gpar(fontsize=20,fontfamily="CMU Serif"),
                                           rot = 90, x = 1))
ggsave("./figures/model_rc_4.pdf", model_rc_4, width = 16, height = 10, dpi = 1200, device = cairo_pdf)

# import population averaged predictions for subgroups ------------------
pred_male <- readRDS("./data/models/pred_rc_5_male")
pred_female <- readRDS("./data/models/pred_rc_5_female")
pred_dem <- readRDS("./data/models/pred_rc_5_dem")
pred_rep <- readRDS("./data/models/pred_rc_5_rep")
pred_npa <- readRDS("./data/models/pred_rc_5_npa")
pred_white <- readRDS("./data/models/pred_rc_5_white")
pred_black <- readRDS("./data/models/pred_rc_5_black")
pred_hispanic <- readRDS("./data/models/pred_rc_5_hispanic")
pred_other <- readRDS("./data/models/pred_rc_5_otherrace")
pred_to_15 <- readRDS("./data/models/pred_rc_5_to_15")
pred_15_30 <- readRDS("./data/models/pred_rc_5_15_30")
pred_30_50 <- readRDS("./data/models/pred_rc_5_30_50")
pred_50_75 <- readRDS("./data/models/pred_rc_5_50_75")
pred_75plus <- readRDS("./data/models/pred_rc_5_75plus")
pred_18_29 <- readRDS("./data/models/pred_rc_5_18_29")
pred_30_44 <- readRDS("./data/models/pred_rc_5_30_44")
pred_45_64 <- readRDS("./data/models/pred_rc_5_45_64")
pred_65plus <- readRDS("./data/models/pred_rc_5_65plus")

# summarize inferences (95% credible intervals)
sum_pred_male <- pred_male %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_female <- pred_female %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_sex <- rbind(sum_pred_male, sum_pred_female) %>%
  mutate(group = factor(rep(c("Male", "Female"), each = 50),
                        levels = c("Female", "Male")))
sum_pred_dem <- pred_dem %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_rep <- pred_rep %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_npa <- pred_npa %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_party <- rbind(sum_pred_dem, sum_pred_rep, sum_pred_npa) %>%
  mutate(group = factor(rep(c("Democrat", "Republican", "None"), each = 50),
                        levels = c("Democrat", "None", "Republican")))
sum_pred_white <- pred_white %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_black <- pred_black %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_hispanic <- pred_hispanic %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_race <- rbind(sum_pred_white, sum_pred_black, sum_pred_hispanic) %>%
  mutate(group = factor(rep(c("White", "Black", "Hispanic"), each = 50),
                        levels = c("Black", "Hispanic", "White")))
sum_pred_18_29 <- pred_18_29 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_44 <- pred_30_44 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_45_64 <- pred_45_64 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_65plus <- pred_65plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_age <- rbind(sum_pred_18_29, sum_pred_30_44, sum_pred_45_64, sum_pred_65plus) %>%
  mutate(group = factor(rep(c("18-29", "30-44", "45-64", "65+"), each = 50),
                        levels = c("18-29", "30-44", "45-64", "65+")))
sum_pred_to_15 <- pred_to_15 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_15_30 <- pred_15_30 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_50 <- pred_30_50 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_50_75 <- pred_50_75 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_75plus <- pred_75plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_income <- rbind(sum_pred_to_15, sum_pred_15_30, sum_pred_30_50, 
                         sum_pred_50_75, sum_pred_75plus) %>%
  mutate(group = factor(rep(c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k"), each = 50),
                        levels = c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k")))

# create graphs
# sex subgroups
sex <- ggplot() +
  geom_ribbon(data = sum_pred_sex, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_sex, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  theme_bw() +
  ggtitle("Sex") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.15, 0.9),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.6), breaks = c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# race subgroups
race <- ggplot() +
  geom_ribbon(data = sum_pred_race, aes(x = x, ymin = lower, ymax = upper, 
                                        group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_race, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Race") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.16, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.6), breaks = c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# age subgroups
age <- ggplot() +
  geom_ribbon(data = sum_pred_age, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_age, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash")) +
  theme_bw() +
  ggtitle("Age") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.14, 0.835),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.6), breaks = c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# party subgroups
party <- ggplot() +
  geom_ribbon(data = sum_pred_party, aes(x = x, ymin = lower, ymax = upper, 
                                         group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_party, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray40", "gray70", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Party") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.183, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.6), breaks = c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# income
income <- ggplot() +
  geom_ribbon(data = sum_pred_income, aes(x = x, ymin = lower, ymax = upper, 
                                          group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_income, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray60", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash", "dashed")) +
  theme_bw() +
  ggtitle("Income") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.22, 0.81),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.6), breaks = c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))

# bind together
a <- sex + theme(plot.margin = unit(c(0, -0.5, -0.32, 0), "cm"))
b <- race + theme(plot.margin = unit(c(0, -0.25, -0.32, -0.25), "cm"))
d <- age + theme(plot.margin = unit(c(0, 0, -0.3, -0.5), "cm"))
e <- party + theme(plot.margin = unit(c(-0.32, -0.37, 0, 6.4), "cm"))
f <- income + theme(plot.margin = unit(c(-0.32, 6.4, 0, -0.37), "cm"))
model_rc_5 <- grid.arrange(cbind(ggplotGrob(a), ggplotGrob(b), ggplotGrob(d), size = "first"),
                           cbind(ggplotGrob(e), ggplotGrob(f), size = "first"),
                           nrow = 2,
                           bottom = textGrob("Propensity to vote", 
                                             gp=gpar(fontsize=20,
                                                     fontfamily="CMU Serif"), 
                                             y = 1.3, x = 0.523),
                           left = textGrob("Probability of social media-based participation", 
                                           gp=gpar(fontsize=20,fontfamily="CMU Serif"),
                                           rot = 90, x = 1))
ggsave("./figures/model_rc_5.pdf", model_rc_5, width = 16, height = 10, dpi = 1200, device = cairo_pdf)

# import population averaged predictions for subgroups ------------------
pred_male <- readRDS("./data/models/pred_rc_6_male")
pred_female <- readRDS("./data/models/pred_rc_6_female")
pred_dem <- readRDS("./data/models/pred_rc_6_dem")
pred_rep <- readRDS("./data/models/pred_rc_6_rep")
pred_npa <- readRDS("./data/models/pred_rc_6_npa")
pred_white <- readRDS("./data/models/pred_rc_6_white")
pred_black <- readRDS("./data/models/pred_rc_6_black")
pred_hispanic <- readRDS("./data/models/pred_rc_6_hispanic")
pred_other <- readRDS("./data/models/pred_rc_6_otherrace")
pred_to_15 <- readRDS("./data/models/pred_rc_6_to_15")
pred_15_30 <- readRDS("./data/models/pred_rc_6_15_30")
pred_30_50 <- readRDS("./data/models/pred_rc_6_30_50")
pred_50_75 <- readRDS("./data/models/pred_rc_6_50_75")
pred_75plus <- readRDS("./data/models/pred_rc_6_75plus")
pred_18_29 <- readRDS("./data/models/pred_rc_6_18_29")
pred_30_44 <- readRDS("./data/models/pred_rc_6_30_44")
pred_45_64 <- readRDS("./data/models/pred_rc_6_45_64")
pred_65plus <- readRDS("./data/models/pred_rc_6_65plus")

# summarize inferences (95% credible intervals)
sum_pred_male <- pred_male %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_female <- pred_female %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_sex <- rbind(sum_pred_male, sum_pred_female) %>%
  mutate(group = factor(rep(c("Male", "Female"), each = 50),
                        levels = c("Female", "Male")))
sum_pred_dem <- pred_dem %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_rep <- pred_rep %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_npa <- pred_npa %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_party <- rbind(sum_pred_dem, sum_pred_rep, sum_pred_npa) %>%
  mutate(group = factor(rep(c("Democrat", "Republican", "None"), each = 50),
                        levels = c("Democrat", "None", "Republican")))
sum_pred_white <- pred_white %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_black <- pred_black %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_hispanic <- pred_hispanic %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_race <- rbind(sum_pred_white, sum_pred_black, sum_pred_hispanic) %>%
  mutate(group = factor(rep(c("White", "Black", "Hispanic"), each = 50),
                        levels = c("Black", "Hispanic", "White")))
sum_pred_18_29 <- pred_18_29 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_44 <- pred_30_44 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_45_64 <- pred_45_64 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_65plus <- pred_65plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_age <- rbind(sum_pred_18_29, sum_pred_30_44, sum_pred_45_64, sum_pred_65plus) %>%
  mutate(group = factor(rep(c("18-29", "30-44", "45-64", "65+"), each = 50),
                        levels = c("18-29", "30-44", "45-64", "65+")))
sum_pred_to_15 <- pred_to_15 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_15_30 <- pred_15_30 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_50 <- pred_30_50 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_50_75 <- pred_50_75 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_75plus <- pred_75plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_income <- rbind(sum_pred_to_15, sum_pred_15_30, sum_pred_30_50, 
                         sum_pred_50_75, sum_pred_75plus) %>%
  mutate(group = factor(rep(c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k"), each = 50),
                        levels = c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k")))

# create graphs
# sex subgroups
sex <- ggplot() +
  geom_ribbon(data = sum_pred_sex, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_sex, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  theme_bw() +
  ggtitle("Sex") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.15, 0.9),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.6), breaks = c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# race subgroups
race <- ggplot() +
  geom_ribbon(data = sum_pred_race, aes(x = x, ymin = lower, ymax = upper, 
                                        group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_race, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Race") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.16, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.6), breaks = c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# age subgroups
age <- ggplot() +
  geom_ribbon(data = sum_pred_age, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_age, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash")) +
  theme_bw() +
  ggtitle("Age") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.14, 0.835),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.6), breaks = c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# party subgroups
party <- ggplot() +
  geom_ribbon(data = sum_pred_party, aes(x = x, ymin = lower, ymax = upper, 
                                         group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_party, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray40", "gray70", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Party") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.183, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.6), breaks = c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# income
income <- ggplot() +
  geom_ribbon(data = sum_pred_income, aes(x = x, ymin = lower, ymax = upper, 
                                          group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_income, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray60", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash", "dashed")) +
  theme_bw() +
  ggtitle("Income") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.22, 0.81),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,0.6), breaks = c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))

# bind together
a <- sex + theme(plot.margin = unit(c(0, -0.5, -0.32, 0), "cm"))
b <- race + theme(plot.margin = unit(c(0, -0.25, -0.32, -0.25), "cm"))
d <- age + theme(plot.margin = unit(c(0, 0, -0.3, -0.5), "cm"))
e <- party + theme(plot.margin = unit(c(-0.32, -0.37, 0, 6.4), "cm"))
f <- income + theme(plot.margin = unit(c(-0.32, 6.4, 0, -0.37), "cm"))
model_rc_6 <- grid.arrange(cbind(ggplotGrob(a), ggplotGrob(b), ggplotGrob(d), size = "first"),
                           cbind(ggplotGrob(e), ggplotGrob(f), size = "first"),
                           nrow = 2,
                           bottom = textGrob("Propensity to vote", 
                                             gp=gpar(fontsize=20,
                                                     fontfamily="CMU Serif"), 
                                             y = 1.3, x = 0.523),
                           left = textGrob("Probability of social media-based participation", 
                                           gp=gpar(fontsize=20,fontfamily="CMU Serif"),
                                           rot = 90, x = 1))
ggsave("./figures/model_rc_6.pdf", model_rc_6, width = 16, height = 10, dpi = 1200, device = cairo_pdf)

# import population averaged predictions for subgroups ------------------
pred_male <- readRDS("./data/models/pred_rc_7_male")
pred_female <- readRDS("./data/models/pred_rc_7_female")
pred_dem <- readRDS("./data/models/pred_rc_7_dem")
pred_rep <- readRDS("./data/models/pred_rc_7_rep")
pred_npa <- readRDS("./data/models/pred_rc_7_npa")
pred_white <- readRDS("./data/models/pred_rc_7_white")
pred_black <- readRDS("./data/models/pred_rc_7_black")
pred_hispanic <- readRDS("./data/models/pred_rc_7_hispanic")
pred_other <- readRDS("./data/models/pred_rc_7_otherrace")
pred_to_15 <- readRDS("./data/models/pred_rc_7_to_15")
pred_15_30 <- readRDS("./data/models/pred_rc_7_15_30")
pred_30_50 <- readRDS("./data/models/pred_rc_7_30_50")
pred_50_75 <- readRDS("./data/models/pred_rc_7_50_75")
pred_75plus <- readRDS("./data/models/pred_rc_7_75plus")
pred_18_29 <- readRDS("./data/models/pred_rc_7_18_29")
pred_30_44 <- readRDS("./data/models/pred_rc_7_30_44")
pred_45_64 <- readRDS("./data/models/pred_rc_7_45_64")
pred_65plus <- readRDS("./data/models/pred_rc_7_65plus")

# summarize inferences (95% credible intervals)
sum_pred_male <- pred_male %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_female <- pred_female %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_sex <- rbind(sum_pred_male, sum_pred_female) %>%
  mutate(group = factor(rep(c("Male", "Female"), each = 50),
                        levels = c("Female", "Male")))
sum_pred_dem <- pred_dem %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_rep <- pred_rep %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_npa <- pred_npa %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_party <- rbind(sum_pred_dem, sum_pred_rep, sum_pred_npa) %>%
  mutate(group = factor(rep(c("Democrat", "Republican", "None"), each = 50),
                        levels = c("Democrat", "None", "Republican")))
sum_pred_white <- pred_white %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_black <- pred_black %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_hispanic <- pred_hispanic %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_race <- rbind(sum_pred_white, sum_pred_black, sum_pred_hispanic) %>%
  mutate(group = factor(rep(c("White", "Black", "Hispanic"), each = 50),
                        levels = c("Black", "Hispanic", "White")))
sum_pred_18_29 <- pred_18_29 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_44 <- pred_30_44 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_45_64 <- pred_45_64 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_65plus <- pred_65plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_age <- rbind(sum_pred_18_29, sum_pred_30_44, sum_pred_45_64, sum_pred_65plus) %>%
  mutate(group = factor(rep(c("18-29", "30-44", "45-64", "65+"), each = 50),
                        levels = c("18-29", "30-44", "45-64", "65+")))
sum_pred_to_15 <- pred_to_15 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_15_30 <- pred_15_30 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_30_50 <- pred_30_50 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_50_75 <- pred_50_75 %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_75plus <- pred_75plus %>% 
  group_by(x) %>% 
  summarize(lower = quantile(y, prob = 0.025),
            upper = quantile(y, prob = 0.975),
            fit = quantile(y, prob = 0.50))
sum_pred_income <- rbind(sum_pred_to_15, sum_pred_15_30, sum_pred_30_50, 
                         sum_pred_50_75, sum_pred_75plus) %>%
  mutate(group = factor(rep(c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k"), each = 50),
                        levels = c("<=15k", ">15k & <=30k", ">30k & <=50k", ">50k & <=75k", ">75k")))
# create graphs
# sex subgroups
sex <- ggplot() +
  geom_ribbon(data = sum_pred_sex, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_sex, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  theme_bw() +
  ggtitle("Sex") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.15, 0.9),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = c(0.2,0.4,0.6,0.8)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# race subgroups
race <- ggplot() +
  geom_ribbon(data = sum_pred_race, aes(x = x, ymin = lower, ymax = upper, 
                                        group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_race, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Race") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.16, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = c(0.2,0.4,0.6,0.8)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# age subgroups
age <- ggplot() +
  geom_ribbon(data = sum_pred_age, aes(x = x, ymin = lower, ymax = upper, 
                                       group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_age, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash")) +
  theme_bw() +
  ggtitle("Age") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.14, 0.835),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = c(0.2,0.4,0.6,0.8)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# party subgroups
party <- ggplot() +
  geom_ribbon(data = sum_pred_party, aes(x = x, ymin = lower, ymax = upper, 
                                         group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_party, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray40", "gray70", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_bw() +
  ggtitle("Party") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.183, 0.87),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = c(0.2,0.4,0.6,0.8)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))
# income
income <- ggplot() +
  geom_ribbon(data = sum_pred_income, aes(x = x, ymin = lower, ymax = upper, 
                                          group = group, fill = group), alpha=0.3) +
  geom_line(data = sum_pred_income, aes(x = x, y = fit, group = group, linetype = group), size = 1) +
  scale_fill_manual(values = c("gray70", "gray20", "gray60", "gray40", "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash", "dashed")) +
  theme_bw() +
  ggtitle("Income") +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20), #16
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 16, color = "black"), #12
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 20, color = "black"), # 16
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 20, color = "black"), # 16
        legend.position = c(0.22, 0.81),
        legend.title=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.text=element_text(size=13), #9
        panel.spacing = unit(0, "lines")) +
  labs(y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = c(0.2,0.4,0.6,0.8)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2.2,2.2), breaks = c(-2,-1,0,1,2))

# bind together
a <- sex + theme(plot.margin = unit(c(0, -0.5, -0.32, 0), "cm"))
b <- race + theme(plot.margin = unit(c(0, -0.25, -0.32, -0.25), "cm"))
d <- age + theme(plot.margin = unit(c(0, 0, -0.3, -0.5), "cm"))
e <- party + theme(plot.margin = unit(c(-0.32, -0.37, 0, 6.4), "cm"))
f <- income + theme(plot.margin = unit(c(-0.32, 6.4, 0, -0.37), "cm"))
model_rc_7 <- grid.arrange(cbind(ggplotGrob(a), ggplotGrob(b), ggplotGrob(d), size = "first"),
                           cbind(ggplotGrob(e), ggplotGrob(f), size = "first"),
                           nrow = 2,
                           bottom = textGrob("Propensity to vote", 
                                             gp=gpar(fontsize=20,
                                                     fontfamily="CMU Serif"), 
                                             y = 1.3, x = 0.523),
                           left = textGrob("Probability of social media-based participation", 
                                           gp=gpar(fontsize=20,fontfamily="CMU Serif"),
                                           rot = 90, x = 1))
ggsave("./figures/model_rc_7.pdf", model_rc_7, width = 16, height = 10, dpi = 1200, device = cairo_pdf)


#### SUBGROUP PLOTS WITH ALTERNATIVE THRESHOLDS (APPENDIX FIGURE G5) ====================

pred_male <- readRDS("./data/models/pred_2_male")
pred_male$variable <- "male"
pred_female <- readRDS("./data/models/pred_2_female")
pred_female$variable <- "female"
pred_dem <- readRDS("./data/models/pred_2_dem")
pred_dem$variable <- "dem"
pred_rep <- readRDS("./data/models/pred_2_rep")
pred_rep$variable <- "rep"
pred_npa <- readRDS("./data/models/pred_2_npa")
pred_npa$variable <- "none"
pred_white <- readRDS("./data/models/pred_2_white")
pred_white$variable <- "white"
pred_black <- readRDS("./data/models/pred_2_black")
pred_black$variable <- "black"
pred_hispanic <- readRDS("./data/models/pred_2_hispanic")
pred_hispanic$variable <- "hispanic"
pred_other <- readRDS("./data/models/pred_2_otherrace")
pred_other$variable <- "other"
pred_to_15 <- readRDS("./data/models/pred_2_to_15")
pred_to_15$variable <- "<=15k"
pred_15_30 <- readRDS("./data/models/pred_2_15_30")
pred_15_30$variable <- "15-30"
pred_30_50 <- readRDS("./data/models/pred_2_30_50")
pred_30_50$variable <- "30-50"
pred_50_75 <- readRDS("./data/models/pred_2_50_75")
pred_50_75$variable <- "50-75"
pred_75plus <- readRDS("./data/models/pred_2_75plus")
pred_75plus$variable <- "75+"
pred_18_29 <- readRDS("./data/models/pred_2_18_29")
pred_18_29$variable <- "18-29"
pred_30_44 <- readRDS("./data/models/pred_2_30_44")
pred_30_44$variable <- "30-44"
pred_45_64 <- readRDS("./data/models/pred_2_45_64")
pred_45_64$variable <- "45-64"
pred_65plus <- readRDS("./data/models/pred_2_65plus")
pred_65plus$variable <- "65+"

model_subgroups_a <- rbind(pred_male, pred_female, pred_dem, pred_rep, pred_npa,
                           pred_white, pred_black, pred_hispanic, pred_other,
                           pred_to_15, pred_15_30, pred_30_50, pred_50_75,
                           pred_75plus, pred_18_29, pred_30_44, pred_45_64,
                           pred_65plus)
model_subgroups_a$variable <-factor(model_subgroups_a$variable, 
                                    levels = c("75+", "50-75", "30-50", "15-30",
                                               "<=15k", "none", "rep", "dem",
                                               "other", "white", "hispanic",
                                               "black", "65+", "45-64", "30-44",
                                               "18-29", "female", "male"))

pred_male <- readRDS("./data/models/pred_rc_9_male")
pred_male$variable <- "male"
pred_female <- readRDS("./data/models/pred_rc_9_female")
pred_female$variable <- "female"
pred_dem <- readRDS("./data/models/pred_rc_9_dem")
pred_dem$variable <- "dem"
pred_rep <- readRDS("./data/models/pred_rc_9_rep")
pred_rep$variable <- "rep"
pred_npa <- readRDS("./data/models/pred_rc_9_npa")
pred_npa$variable <- "none"
pred_white <- readRDS("./data/models/pred_rc_9_white")
pred_white$variable <- "white"
pred_black <- readRDS("./data/models/pred_rc_9_black")
pred_black$variable <- "black"
pred_hispanic <- readRDS("./data/models/pred_rc_9_hispanic")
pred_hispanic$variable <- "hispanic"
pred_other <- readRDS("./data/models/pred_rc_9_otherrace")
pred_other$variable <- "other"
pred_to_15 <- readRDS("./data/models/pred_rc_9_to_15")
pred_to_15$variable <- "<=15k"
pred_15_30 <- readRDS("./data/models/pred_rc_9_15_30")
pred_15_30$variable <- "15-30"
pred_30_50 <- readRDS("./data/models/pred_rc_9_30_50")
pred_30_50$variable <- "30-50"
pred_50_75 <- readRDS("./data/models/pred_rc_9_50_75")
pred_50_75$variable <- "50-75"
pred_75plus <- readRDS("./data/models/pred_rc_9_75plus")
pred_75plus$variable <- "75+"
pred_18_29 <- readRDS("./data/models/pred_rc_9_18_29")
pred_18_29$variable <- "18-29"
pred_30_44 <- readRDS("./data/models/pred_rc_9_30_44")
pred_30_44$variable <- "30-44"
pred_45_64 <- readRDS("./data/models/pred_rc_9_45_64")
pred_45_64$variable <- "45-64"
pred_65plus <- readRDS("./data/models/pred_rc_9_65plus")
pred_65plus$variable <- "65+"

model_subgroups_b <- rbind(pred_male, pred_female, pred_dem, pred_rep, pred_npa,
                         pred_white, pred_black, pred_hispanic, pred_other,
                         pred_to_15, pred_15_30, pred_30_50, pred_50_75,
                         pred_75plus, pred_18_29, pred_30_44, pred_45_64,
                         pred_65plus)
model_subgroups_b$variable <-factor(model_subgroups_b$variable, 
                                  levels = c("75+", "50-75", "30-50", "15-30",
                                             "<=15k", "none", "rep", "dem",
                                             "other", "white", "hispanic",
                                             "black", "65+", "45-64", "30-44",
                                             "18-29", "female", "male"))

pred_male <- readRDS("./data/models/pred_rc_10_male")
pred_male$variable <- "male"
pred_female <- readRDS("./data/models/pred_rc_10_female")
pred_female$variable <- "female"
pred_dem <- readRDS("./data/models/pred_rc_10_dem")
pred_dem$variable <- "dem"
pred_rep <- readRDS("./data/models/pred_rc_10_rep")
pred_rep$variable <- "rep"
pred_npa <- readRDS("./data/models/pred_rc_10_npa")
pred_npa$variable <- "none"
pred_white <- readRDS("./data/models/pred_rc_10_white")
pred_white$variable <- "white"
pred_black <- readRDS("./data/models/pred_rc_10_black")
pred_black$variable <- "black"
pred_hispanic <- readRDS("./data/models/pred_rc_10_hispanic")
pred_hispanic$variable <- "hispanic"
pred_other <- readRDS("./data/models/pred_rc_10_otherrace")
pred_other$variable <- "other"
pred_to_15 <- readRDS("./data/models/pred_rc_10_to_15")
pred_to_15$variable <- "<=15k"
pred_15_30 <- readRDS("./data/models/pred_rc_10_15_30")
pred_15_30$variable <- "15-30"
pred_30_50 <- readRDS("./data/models/pred_rc_10_30_50")
pred_30_50$variable <- "30-50"
pred_50_75 <- readRDS("./data/models/pred_rc_10_50_75")
pred_50_75$variable <- "50-75"
pred_75plus <- readRDS("./data/models/pred_rc_10_75plus")
pred_75plus$variable <- "75+"
pred_18_29 <- readRDS("./data/models/pred_rc_10_18_29")
pred_18_29$variable <- "18-29"
pred_30_44 <- readRDS("./data/models/pred_rc_10_30_44")
pred_30_44$variable <- "30-44"
pred_45_64 <- readRDS("./data/models/pred_rc_10_45_64")
pred_45_64$variable <- "45-64"
pred_65plus <- readRDS("./data/models/pred_rc_10_65plus")
pred_65plus$variable <- "65+"

model_subgroups_c <- rbind(pred_male, pred_female, pred_dem, pred_rep, pred_npa,
                         pred_white, pred_black, pred_hispanic, pred_other,
                         pred_to_15, pred_15_30, pred_30_50, pred_50_75,
                         pred_75plus, pred_18_29, pred_30_44, pred_45_64,
                         pred_65plus)
model_subgroups_c$variable <-factor(model_subgroups_c$variable, 
                                  levels = c("75+", "50-75", "30-50", "15-30",
                                             "<=15k", "none", "rep", "dem",
                                             "other", "white", "hispanic",
                                             "black", "65+", "45-64", "30-44",
                                             "18-29", "female", "male"))

pred_male <- readRDS("./data/models/pred_rc_11_male")
pred_male$variable <- "male"
pred_female <- readRDS("./data/models/pred_rc_11_female")
pred_female$variable <- "female"
pred_dem <- readRDS("./data/models/pred_rc_11_dem")
pred_dem$variable <- "dem"
pred_rep <- readRDS("./data/models/pred_rc_11_rep")
pred_rep$variable <- "rep"
pred_npa <- readRDS("./data/models/pred_rc_11_npa")
pred_npa$variable <- "none"
pred_white <- readRDS("./data/models/pred_rc_11_white")
pred_white$variable <- "white"
pred_black <- readRDS("./data/models/pred_rc_11_black")
pred_black$variable <- "black"
pred_hispanic <- readRDS("./data/models/pred_rc_11_hispanic")
pred_hispanic$variable <- "hispanic"
pred_other <- readRDS("./data/models/pred_rc_11_otherrace")
pred_other$variable <- "other"
pred_to_15 <- readRDS("./data/models/pred_rc_11_to_15")
pred_to_15$variable <- "<=15k"
pred_15_30 <- readRDS("./data/models/pred_rc_11_15_30")
pred_15_30$variable <- "15-30"
pred_30_50 <- readRDS("./data/models/pred_rc_11_30_50")
pred_30_50$variable <- "30-50"
pred_50_75 <- readRDS("./data/models/pred_rc_11_50_75")
pred_50_75$variable <- "50-75"
pred_75plus <- readRDS("./data/models/pred_rc_11_75plus")
pred_75plus$variable <- "75+"
pred_18_29 <- readRDS("./data/models/pred_rc_11_18_29")
pred_18_29$variable <- "18-29"
pred_30_44 <- readRDS("./data/models/pred_rc_11_30_44")
pred_30_44$variable <- "30-44"
pred_45_64 <- readRDS("./data/models/pred_rc_11_45_64")
pred_45_64$variable <- "45-64"
pred_65plus <- readRDS("./data/models/pred_rc_11_65plus")
pred_65plus$variable <- "65+"

model_subgroups_d <- rbind(pred_male, pred_female, pred_dem, pred_rep, pred_npa,
                         pred_white, pred_black, pred_hispanic, pred_other,
                         pred_to_15, pred_15_30, pred_30_50, pred_50_75,
                         pred_75plus, pred_18_29, pred_30_44, pred_45_64,
                         pred_65plus)
model_subgroups_d$variable <-factor(model_subgroups_d$variable, 
                                  levels = c("75+", "50-75", "30-50", "15-30",
                                             "<=15k", "none", "rep", "dem",
                                             "other", "white", "hispanic",
                                             "black", "65+", "45-64", "30-44",
                                             "18-29", "female", "male"))

model_subgroups_a$group <- "1 political post"
model_subgroups_b$group <- "5 political posts"
model_subgroups_c$group <- "10 political posts"
model_subgroups_d$group <- "25 political posts"
model_subgroups <- rbind(model_subgroups_a, model_subgroups_b,
                         model_subgroups_c, model_subgroups_d)
model_subgroups$group <- factor(model_subgroups$group, 
                                    levels = c("1 political post",
                                               "5 political posts",
                                               "10 political posts",
                                               "25 political posts"))

subgroups <- ggplot() + 
  geom_point(data = model_subgroups, aes(x = variable, y = y*100), color = "black", 
             position = position_jitter(width = 0.3), alpha = I(1/sqrt(4000))) +
 # geom_point(data = raw_subgroups, aes(x = variable, y = share), color = "gray60", size = 2.3) +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5, 16.5), linetype = "dotted", colour = "gray20") +
  coord_flip() + 
  facet_wrap(~ group) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black")) +
  scale_x_discrete(labels = c("65+" = "65 +", "female" = "Female", "male" = "Male",
                              "black" = "Black", "hispanic" = "Hispanic", "white" = "White", "other" = "Other",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "15-30" = "15k-30k", "30-50" = "30k-50k", "50-75" = "50k-75k",
                              "75+" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,25),
                     breaks = seq(5, 20, by = 5),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "  Income  Party   Race    Age    Sex       Income  Party   Race    Age    Sex")

ggsave("./figures/subgroups_altmeasures.png", subgroups, width = 6.5, height = 8.5, dpi = 320)


#### SUBGROUP INTERACTION PLOTS FOR VOTER TYPES (APPENDIX FIGURES G16-31) ===============

# import population averaged predictions for interacted subgroups -----------------------
pred_white_to_15 <- readRDS("./data/models/pred_lo_white_to_15")
pred_white_15_30 <- readRDS("./data/models/pred_lo_white_15_30")
pred_white_30_50 <- readRDS("./data/models/pred_lo_white_30_50")
pred_white_50_75 <- readRDS("./data/models/pred_lo_white_50_75")
pred_white_75_plus <- readRDS("./data/models/pred_lo_white_75_plus")
pred_black_to_15 <- readRDS("./data/models/pred_lo_black_to_15")
pred_black_15_30 <- readRDS("./data/models/pred_lo_black_15_30")
pred_black_30_50 <- readRDS("./data/models/pred_lo_black_30_50")
pred_black_50_75 <- readRDS("./data/models/pred_lo_black_50_75")
pred_black_75_plus <- readRDS("./data/models/pred_lo_black_75_plus")
pred_hispanic_to_15 <- readRDS("./data/models/pred_lo_hispanic_to_15")
pred_hispanic_15_30 <- readRDS("./data/models/pred_lo_hispanic_15_30")
pred_hispanic_30_50 <- readRDS("./data/models/pred_lo_hispanic_30_50")
pred_hispanic_50_75 <- readRDS("./data/models/pred_lo_hispanic_50_75")
pred_hispanic_75_plus <- readRDS("./data/models/pred_lo_hispanic_75_plus")
pred_other_to_15 <- readRDS("./data/models/pred_lo_other_to_15")
pred_other_15_30 <- readRDS("./data/models/pred_lo_other_15_30")
pred_other_30_50 <- readRDS("./data/models/pred_lo_other_30_50")
pred_other_50_75 <- readRDS("./data/models/pred_lo_other_50_75")
pred_other_75_plus <- readRDS("./data/models/pred_lo_other_75_plus")
pred_race_income <- list(pred_white_to_15, pred_white_15_30, pred_white_30_50,
                         pred_white_50_75, pred_white_75_plus, pred_black_to_15,
                         pred_black_15_30, pred_black_30_50, pred_black_50_75,
                         pred_black_75_plus, pred_hispanic_to_15, pred_hispanic_15_30,
                         pred_hispanic_30_50, pred_hispanic_50_75, pred_hispanic_75_plus,
                         pred_other_to_15, pred_other_15_30, pred_other_30_50, pred_other_50_75,
                         pred_other_75_plus)
pred_race_income <- pred_race_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 4)
pred_race_income$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 5)

pred_white_18_29 <- readRDS("./data/models/pred_lo_white_18_29")
pred_white_30_44 <- readRDS("./data/models/pred_lo_white_30_44")
pred_white_45_64 <- readRDS("./data/models/pred_lo_white_45_64")
pred_white_60plus <- readRDS("./data/models/pred_lo_white_60plus")
pred_black_18_29 <- readRDS("./data/models/pred_lo_black_18_29")
pred_black_30_44 <- readRDS("./data/models/pred_lo_black_30_44")
pred_black_45_64 <- readRDS("./data/models/pred_lo_black_45_64")
pred_black_60plus <- readRDS("./data/models/pred_lo_black_60plus")
pred_hispanic_18_29 <- readRDS("./data/models/pred_lo_hispanic_18_29")
pred_hispanic_30_44 <- readRDS("./data/models/pred_lo_hispanic_30_44")
pred_hispanic_45_64 <- readRDS("./data/models/pred_lo_hispanic_45_64")
pred_hispanic_60plus <- readRDS("./data/models/pred_lo_hispanic_60plus")
pred_other_18_29 <- readRDS("./data/models/pred_lo_other_18_29")
pred_other_30_44 <- readRDS("./data/models/pred_lo_other_30_44")
pred_other_45_64 <- readRDS("./data/models/pred_lo_other_45_64")
pred_other_60plus <- readRDS("./data/models/pred_lo_other_60plus")
pred_race_age <- list(pred_white_18_29, pred_white_30_44, pred_white_45_64,
                      pred_white_60plus, 
                      pred_black_18_29, pred_black_30_44, pred_black_45_64, 
                      pred_black_60plus,
                      pred_hispanic_18_29, pred_hispanic_30_44, pred_hispanic_45_64, 
                      pred_hispanic_60plus,
                      pred_other_18_29, pred_other_30_44, pred_other_45_64, 
                      pred_other_60plus)
pred_race_age<- pred_race_age %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_age$variable <- rep(c("18_29", "30_44", "45_64", "60plus"), times = 4)
pred_race_age$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 4)

pred_white_dem <- readRDS("./data/models/pred_lo_white_dem")
pred_white_rep <- readRDS("./data/models/pred_lo_white_rep")
pred_white_none <- readRDS("./data/models/pred_lo_white_none")
pred_black_dem <- readRDS("./data/models/pred_lo_black_dem")
pred_black_rep <- readRDS("./data/models/pred_lo_black_rep")
pred_black_none <- readRDS("./data/models/pred_lo_black_none")
pred_hispanic_dem <- readRDS("./data/models/pred_lo_hispanic_dem")
pred_hispanic_rep <- readRDS("./data/models/pred_lo_hispanic_rep")
pred_hispanic_none <- readRDS("./data/models/pred_lo_hispanic_none")
pred_other_dem <- readRDS("./data/models/pred_lo_other_dem")
pred_other_rep <- readRDS("./data/models/pred_lo_other_rep")
pred_other_none <- readRDS("./data/models/pred_lo_other_none")
pred_race_party <- list(pred_white_dem, pred_white_rep, pred_white_none,
                        pred_black_dem, pred_black_rep, pred_black_none, 
                        pred_hispanic_dem, pred_hispanic_rep, pred_hispanic_none, 
                        pred_other_dem, pred_other_rep, pred_other_none)
pred_race_party <- pred_race_party %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_party$variable <- rep(c("dem", "rep", "none"), times = 4)
pred_race_party$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 3)

pred_white_female <- readRDS("./data/models/pred_lo_white_female")
pred_white_male <- readRDS("./data/models/pred_lo_white_male")
pred_black_female <- readRDS("./data/models/pred_lo_black_female")
pred_black_male <- readRDS("./data/models/pred_lo_black_male")
pred_hispanic_female <- readRDS("./data/models/pred_lo_hispanic_female")
pred_hispanic_male <- readRDS("./data/models/pred_lo_hispanic_male")
pred_other_female <- readRDS("./data/models/pred_lo_other_female")
pred_other_male <- readRDS("./data/models/pred_lo_other_male")
pred_race_sex <- list(pred_white_female, pred_white_male,
                      pred_black_female, pred_black_male, 
                      pred_hispanic_female, pred_hispanic_male, 
                      pred_other_female, pred_other_male)
pred_race_sex <- pred_race_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_sex$variable <- rep(c("female", "male"), times = 4)
pred_race_sex$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 2)

pred_race <- rbind(pred_race_income, pred_race_age, pred_race_party, pred_race_sex)

pred_race$variable <-factor(pred_race$variable, 
                            levels = c("75_plus", "50_75", "30_50", "15_30",
                                       "to_15", "none", "rep", "dem",
                                       "60plus", "45_64", "30_44",
                                       "18_29", "female", "male"))
pred_race$group <-factor(pred_race$group, 
                         levels = c("Other", "White", "Hispanic", "Black"))

subgroups_interactions_1_lo <- ggplot(pred_race, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.9, 0.9285),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("60plus" = "65 +", "45_64" = "45-64", "30_44" = "30-44",
                              "18_29" = "18-29", "female" = "Female", "male" = "Male",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "        Income                       Party                      Age             Sex")

ggsave("./figures/subgroups_interactions_1_lo.png", subgroups_interactions_1_lo, width = 6.5, height = 8.5, dpi = 320)

pred_18_29_to_15 <- readRDS("./data/models/pred_lo_18_29_to_15")
pred_18_29_15_30 <- readRDS("./data/models/pred_lo_18_29_15_30")
pred_18_29_30_50 <- readRDS("./data/models/pred_lo_18_29_30_50")
pred_18_29_50_75 <- readRDS("./data/models/pred_lo_18_29_50_75")
pred_18_29_75_plus <- readRDS("./data/models/pred_lo_18_29_75_plus")
pred_30_44_to_15 <- readRDS("./data/models/pred_lo_30_44_to_15")
pred_30_44_15_30 <- readRDS("./data/models/pred_lo_30_44_15_30")
pred_30_44_30_50 <- readRDS("./data/models/pred_lo_30_44_30_50")
pred_30_44_50_75 <- readRDS("./data/models/pred_lo_30_44_50_75")
pred_30_44_75_plus <- readRDS("./data/models/pred_lo_30_44_75_plus")
pred_45_64_to_15 <- readRDS("./data/models/pred_lo_45_64_to_15")
pred_45_64_15_30 <- readRDS("./data/models/pred_lo_45_64_15_30")
pred_45_64_30_50 <- readRDS("./data/models/pred_lo_45_64_30_50")
pred_45_64_50_75 <- readRDS("./data/models/pred_lo_45_64_50_75")
pred_45_64_75_plus <- readRDS("./data/models/pred_lo_45_64_75_plus")
pred_65plus_to_15 <- readRDS("./data/models/pred_lo_65plus_to_15")
pred_65plus_15_30 <- readRDS("./data/models/pred_lo_65plus_15_30")
pred_65plus_30_50 <- readRDS("./data/models/pred_lo_65plus_30_50")
pred_65plus_50_75 <- readRDS("./data/models/pred_lo_65plus_50_75")
pred_65plus_75_plus <- readRDS("./data/models/pred_lo_65plus_75_plus")
pred_age_income <- list(pred_18_29_to_15, pred_18_29_15_30, pred_18_29_30_50,
                        pred_18_29_50_75, pred_18_29_75_plus, pred_30_44_to_15,
                        pred_30_44_15_30, pred_30_44_30_50, pred_30_44_50_75,
                        pred_30_44_75_plus, pred_45_64_to_15, pred_45_64_15_30,
                        pred_45_64_30_50, pred_45_64_50_75, pred_45_64_75_plus,
                        pred_65plus_to_15, pred_65plus_15_30, pred_65plus_30_50, 
                        pred_65plus_50_75, pred_65plus_75_plus)
pred_age_income <- pred_age_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 4)
pred_age_income$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 5)

pred_18_29_dem <- readRDS("./data/models/pred_lo_18_29_dem")
pred_18_29_rep <- readRDS("./data/models/pred_lo_18_29_rep")
pred_18_29_none <- readRDS("./data/models/pred_lo_18_29_none")
pred_30_44_dem <- readRDS("./data/models/pred_lo_30_44_dem")
pred_30_44_rep <- readRDS("./data/models/pred_lo_30_44_rep")
pred_30_44_none <- readRDS("./data/models/pred_lo_30_44_none")
pred_45_64_dem <- readRDS("./data/models/pred_lo_45_64_dem")
pred_45_64_rep <- readRDS("./data/models/pred_lo_45_64_rep")
pred_45_64_none <- readRDS("./data/models/pred_lo_45_64_none")
pred_65plus_dem <- readRDS("./data/models/pred_lo_65plus_dem")
pred_65plus_rep <- readRDS("./data/models/pred_lo_65plus_rep")
pred_65plus_none <- readRDS("./data/models/pred_lo_65plus_none")
pred_age_party <- list(pred_18_29_dem, pred_18_29_rep, pred_18_29_none,
                       pred_30_44_dem, pred_30_44_rep, pred_30_44_none, 
                       pred_45_64_dem, pred_45_64_rep, pred_45_64_none, 
                       pred_65plus_dem, pred_65plus_rep, pred_65plus_none)
pred_age_party <- pred_age_party %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_party$variable <- rep(c("dem", "rep", "none"), times = 4)
pred_age_party$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 3)


pred_18_29_female <- readRDS("./data/models/pred_lo_18_29_female")
pred_18_29_male <- readRDS("./data/models/pred_lo_18_29_male")
pred_30_44_female <- readRDS("./data/models/pred_lo_30_44_female")
pred_30_44_male <- readRDS("./data/models/pred_lo_30_44_male")
pred_45_64_female <- readRDS("./data/models/pred_lo_45_64_female")
pred_45_64_male <- readRDS("./data/models/pred_lo_45_64_male")
pred_65plus_female <- readRDS("./data/models/pred_lo_65plus_female")
pred_65plus_male <- readRDS("./data/models/pred_lo_65plus_male")
pred_age_sex <- list(pred_18_29_female, pred_18_29_male,
                     pred_30_44_female, pred_30_44_male, 
                     pred_45_64_female, pred_45_64_male, 
                     pred_65plus_female, pred_65plus_male)
pred_age_sex <- pred_age_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_sex$variable <- rep(c("female", "male"), times = 4)
pred_age_sex$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 2)


pred_age <- rbind(pred_age_income, pred_age_party, pred_age_sex)

pred_age$variable <-factor(pred_age$variable, 
                           levels = c("75_plus", "50_75", "30_50", "15_30",
                                      "to_15", "none", "rep", "dem",
                                      "female", "male"))

pred_age$group <-factor(pred_age$group, 
                        levels = c("65 +", "45-64", "30-44", "18-29"))

subgroups_interactions_2_lo <- ggplot(pred_age, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.924, 0.9285),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("female" = "Female", "male" = "Male",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "            Income                                 Party                        Sex")

ggsave("./figures/subgroups_interactions_2_lo.pdf", subgroups_interactions_2_lo, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

pred_dem_to_15 <- readRDS("./data/models/pred_lo_dem_to_15")
pred_dem_15_30 <- readRDS("./data/models/pred_lo_dem_15_30")
pred_dem_30_50 <- readRDS("./data/models/pred_lo_dem_30_50")
pred_dem_50_75 <- readRDS("./data/models/pred_lo_dem_50_75")
pred_dem_75_plus <- readRDS("./data/models/pred_lo_dem_75_plus")
pred_rep_to_15 <- readRDS("./data/models/pred_lo_rep_to_15")
pred_rep_15_30 <- readRDS("./data/models/pred_lo_rep_15_30")
pred_rep_30_50 <- readRDS("./data/models/pred_lo_rep_30_50")
pred_rep_50_75 <- readRDS("./data/models/pred_lo_rep_50_75")
pred_rep_75_plus <- readRDS("./data/models/pred_lo_rep_75_plus")
pred_none_to_15 <- readRDS("./data/models/pred_lo_none_to_15")
pred_none_15_30 <- readRDS("./data/models/pred_lo_none_15_30")
pred_none_30_50 <- readRDS("./data/models/pred_lo_none_30_50")
pred_none_50_75 <- readRDS("./data/models/pred_lo_none_50_75")
pred_none_75_plus <- readRDS("./data/models/pred_lo_none_75_plus")
pred_party_income <- list(pred_dem_to_15, pred_dem_15_30, pred_dem_30_50,
                          pred_dem_50_75, pred_dem_75_plus, pred_rep_to_15,
                          pred_rep_15_30, pred_rep_30_50, pred_rep_50_75,
                          pred_rep_75_plus, pred_none_to_15, pred_none_15_30,
                          pred_none_30_50, pred_none_50_75, pred_none_75_plus)
pred_party_income <- pred_party_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_party_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 3)
pred_party_income$group <- rep(c("Democrat", "Republican", "None"), each = 5)

pred_dem_female <- readRDS("./data/models/pred_lo_dem_female")
pred_dem_male <- readRDS("./data/models/pred_lo_dem_male")
pred_rep_female <- readRDS("./data/models/pred_lo_rep_female")
pred_rep_male <- readRDS("./data/models/pred_lo_rep_male")
pred_none_female <- readRDS("./data/models/pred_lo_none_female")
pred_none_male <- readRDS("./data/models/pred_lo_none_male")
pred_party_sex <- list(pred_dem_female, pred_dem_male,
                       pred_rep_female, pred_rep_male, 
                       pred_none_female, pred_none_male)
pred_party_sex <- pred_party_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_party_sex$variable <- rep(c("female", "male"), times = 3)
pred_party_sex$group <- rep(c("Democrat", "Republican", "None"), each = 2)

pred_party <- rbind(pred_party_income, pred_party_sex)

pred_party$variable <-factor(pred_party$variable, 
                             levels = c("75_plus", "50_75", "30_50", "15_30",
                                        "to_15", "female", "male"))

pred_party$group <-factor(pred_party$group, 
                          levels = c("None", "Republican", "Democrat"))

subgroups_interactions_3_lo <- ggplot(pred_party, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.115, 0.9435),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("female" = "Female", "male" = "Male",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "              Income                                               Sex")

ggsave("./figures/subgroups_interactions_3_lo.pdf", subgroups_interactions_3_lo, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

pred_female_to_15 <- readRDS("./data/models/pred_lo_female_to_15")
pred_female_15_30 <- readRDS("./data/models/pred_lo_female_15_30")
pred_female_30_50 <- readRDS("./data/models/pred_lo_female_30_50")
pred_female_50_75 <- readRDS("./data/models/pred_lo_female_50_75")
pred_female_75_plus <- readRDS("./data/models/pred_lo_female_75_plus")
pred_male_to_15 <- readRDS("./data/models/pred_lo_male_to_15")
pred_male_15_30 <- readRDS("./data/models/pred_lo_male_15_30")
pred_male_30_50 <- readRDS("./data/models/pred_lo_male_30_50")
pred_male_50_75 <- readRDS("./data/models/pred_lo_male_50_75")
pred_male_75_plus <- readRDS("./data/models/pred_lo_male_75_plus")
pred_sex_income <- list(pred_female_to_15, pred_female_15_30, pred_female_30_50,
                        pred_female_50_75, pred_female_75_plus, pred_male_to_15,
                        pred_male_15_30, pred_male_30_50, pred_male_50_75,
                        pred_male_75_plus)
pred_sex_income <- pred_sex_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_sex_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 2)
pred_sex_income$group <- rep(c("Female", "Male"), each = 5)

pred_sex <- pred_sex_income
pred_sex$variable <-factor(pred_sex$variable, 
                           levels = c("75_plus", "50_75", "30_50", "15_30",
                                      "to_15"))
pred_sex$group <-factor(pred_sex$group, 
                        levels = c("Female", "Male"))

subgroups_interactions_4_lo <- ggplot(pred_sex, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.915, 0.959),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "Income")


ggsave("./figures/subgroups_interactions_4_lo.pdf", subgroups_interactions_4_lo, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

# import population averaged predictions for interacted subgroups -----------------------
pred_white_to_15 <- readRDS("./data/models/pred_ir_white_to_15")
pred_white_15_30 <- readRDS("./data/models/pred_ir_white_15_30")
pred_white_30_50 <- readRDS("./data/models/pred_ir_white_30_50")
pred_white_50_75 <- readRDS("./data/models/pred_ir_white_50_75")
pred_white_75_plus <- readRDS("./data/models/pred_ir_white_75_plus")
pred_black_to_15 <- readRDS("./data/models/pred_ir_black_to_15")
pred_black_15_30 <- readRDS("./data/models/pred_ir_black_15_30")
pred_black_30_50 <- readRDS("./data/models/pred_ir_black_30_50")
pred_black_50_75 <- readRDS("./data/models/pred_ir_black_50_75")
pred_black_75_plus <- readRDS("./data/models/pred_ir_black_75_plus")
pred_hispanic_to_15 <- readRDS("./data/models/pred_ir_hispanic_to_15")
pred_hispanic_15_30 <- readRDS("./data/models/pred_ir_hispanic_15_30")
pred_hispanic_30_50 <- readRDS("./data/models/pred_ir_hispanic_30_50")
pred_hispanic_50_75 <- readRDS("./data/models/pred_ir_hispanic_50_75")
pred_hispanic_75_plus <- readRDS("./data/models/pred_ir_hispanic_75_plus")
pred_other_to_15 <- readRDS("./data/models/pred_ir_other_to_15")
pred_other_15_30 <- readRDS("./data/models/pred_ir_other_15_30")
pred_other_30_50 <- readRDS("./data/models/pred_ir_other_30_50")
pred_other_50_75 <- readRDS("./data/models/pred_ir_other_50_75")
pred_other_75_plus <- readRDS("./data/models/pred_ir_other_75_plus")
pred_race_income <- list(pred_white_to_15, pred_white_15_30, pred_white_30_50,
                         pred_white_50_75, pred_white_75_plus, pred_black_to_15,
                         pred_black_15_30, pred_black_30_50, pred_black_50_75,
                         pred_black_75_plus, pred_hispanic_to_15, pred_hispanic_15_30,
                         pred_hispanic_30_50, pred_hispanic_50_75, pred_hispanic_75_plus,
                         pred_other_to_15, pred_other_15_30, pred_other_30_50, pred_other_50_75,
                         pred_other_75_plus)
pred_race_income <- pred_race_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 4)
pred_race_income$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 5)

pred_white_18_29 <- readRDS("./data/models/pred_ir_white_18_29")
pred_white_30_44 <- readRDS("./data/models/pred_ir_white_30_44")
pred_white_45_64 <- readRDS("./data/models/pred_ir_white_45_64")
pred_white_60plus <- readRDS("./data/models/pred_ir_white_60plus")
pred_black_18_29 <- readRDS("./data/models/pred_ir_black_18_29")
pred_black_30_44 <- readRDS("./data/models/pred_ir_black_30_44")
pred_black_45_64 <- readRDS("./data/models/pred_ir_black_45_64")
pred_black_60plus <- readRDS("./data/models/pred_ir_black_60plus")
pred_hispanic_18_29 <- readRDS("./data/models/pred_ir_hispanic_18_29")
pred_hispanic_30_44 <- readRDS("./data/models/pred_ir_hispanic_30_44")
pred_hispanic_45_64 <- readRDS("./data/models/pred_ir_hispanic_45_64")
pred_hispanic_60plus <- readRDS("./data/models/pred_ir_hispanic_60plus")
pred_other_18_29 <- readRDS("./data/models/pred_ir_other_18_29")
pred_other_30_44 <- readRDS("./data/models/pred_ir_other_30_44")
pred_other_45_64 <- readRDS("./data/models/pred_ir_other_45_64")
pred_other_60plus <- readRDS("./data/models/pred_ir_other_60plus")
pred_race_age <- list(pred_white_18_29, pred_white_30_44, pred_white_45_64,
                      pred_white_60plus, 
                      pred_black_18_29, pred_black_30_44, pred_black_45_64, 
                      pred_black_60plus,
                      pred_hispanic_18_29, pred_hispanic_30_44, pred_hispanic_45_64, 
                      pred_hispanic_60plus,
                      pred_other_18_29, pred_other_30_44, pred_other_45_64, 
                      pred_other_60plus)
pred_race_age<- pred_race_age %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_age$variable <- rep(c("18_29", "30_44", "45_64", "60plus"), times = 4)
pred_race_age$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 4)

pred_white_dem <- readRDS("./data/models/pred_ir_white_dem")
pred_white_rep <- readRDS("./data/models/pred_ir_white_rep")
pred_white_none <- readRDS("./data/models/pred_ir_white_none")
pred_black_dem <- readRDS("./data/models/pred_ir_black_dem")
pred_black_rep <- readRDS("./data/models/pred_ir_black_rep")
pred_black_none <- readRDS("./data/models/pred_ir_black_none")
pred_hispanic_dem <- readRDS("./data/models/pred_ir_hispanic_dem")
pred_hispanic_rep <- readRDS("./data/models/pred_ir_hispanic_rep")
pred_hispanic_none <- readRDS("./data/models/pred_ir_hispanic_none")
pred_other_dem <- readRDS("./data/models/pred_ir_other_dem")
pred_other_rep <- readRDS("./data/models/pred_ir_other_rep")
pred_other_none <- readRDS("./data/models/pred_ir_other_none")
pred_race_party <- list(pred_white_dem, pred_white_rep, pred_white_none,
                        pred_black_dem, pred_black_rep, pred_black_none, 
                        pred_hispanic_dem, pred_hispanic_rep, pred_hispanic_none, 
                        pred_other_dem, pred_other_rep, pred_other_none)
pred_race_party <- pred_race_party %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_party$variable <- rep(c("dem", "rep", "none"), times = 4)
pred_race_party$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 3)

pred_white_female <- readRDS("./data/models/pred_ir_white_female")
pred_white_male <- readRDS("./data/models/pred_ir_white_male")
pred_black_female <- readRDS("./data/models/pred_ir_black_female")
pred_black_male <- readRDS("./data/models/pred_ir_black_male")
pred_hispanic_female <- readRDS("./data/models/pred_ir_hispanic_female")
pred_hispanic_male <- readRDS("./data/models/pred_ir_hispanic_male")
pred_other_female <- readRDS("./data/models/pred_ir_other_female")
pred_other_male <- readRDS("./data/models/pred_ir_other_male")
pred_race_sex <- list(pred_white_female, pred_white_male,
                      pred_black_female, pred_black_male, 
                      pred_hispanic_female, pred_hispanic_male, 
                      pred_other_female, pred_other_male)
pred_race_sex <- pred_race_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_sex$variable <- rep(c("female", "male"), times = 4)
pred_race_sex$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 2)

pred_race <- rbind(pred_race_income, pred_race_age, pred_race_party, pred_race_sex)

pred_race$variable <-factor(pred_race$variable, 
                            levels = c("75_plus", "50_75", "30_50", "15_30",
                                       "to_15", "none", "rep", "dem",
                                       "60plus", "45_64", "30_44",
                                       "18_29", "female", "male"))
pred_race$group <-factor(pred_race$group, 
                         levels = c("Other", "White", "Hispanic", "Black"))

subgroups_interactions_1_ir <- ggplot(pred_race, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.9, 0.9285),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("60plus" = "65 +", "45_64" = "45-64", "30_44" = "30-44",
                              "18_29" = "18-29", "female" = "Female", "male" = "Male",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "        Income                       Party                      Age             Sex")

ggsave("./figures/subgroups_interactions_1_ir.png", subgroups_interactions_1_ir, width = 6.5, height = 8.5, dpi = 320)

pred_18_29_to_15 <- readRDS("./data/models/pred_ir_18_29_to_15")
pred_18_29_15_30 <- readRDS("./data/models/pred_ir_18_29_15_30")
pred_18_29_30_50 <- readRDS("./data/models/pred_ir_18_29_30_50")
pred_18_29_50_75 <- readRDS("./data/models/pred_ir_18_29_50_75")
pred_18_29_75_plus <- readRDS("./data/models/pred_ir_18_29_75_plus")
pred_30_44_to_15 <- readRDS("./data/models/pred_ir_30_44_to_15")
pred_30_44_15_30 <- readRDS("./data/models/pred_ir_30_44_15_30")
pred_30_44_30_50 <- readRDS("./data/models/pred_ir_30_44_30_50")
pred_30_44_50_75 <- readRDS("./data/models/pred_ir_30_44_50_75")
pred_30_44_75_plus <- readRDS("./data/models/pred_ir_30_44_75_plus")
pred_45_64_to_15 <- readRDS("./data/models/pred_ir_45_64_to_15")
pred_45_64_15_30 <- readRDS("./data/models/pred_ir_45_64_15_30")
pred_45_64_30_50 <- readRDS("./data/models/pred_ir_45_64_30_50")
pred_45_64_50_75 <- readRDS("./data/models/pred_ir_45_64_50_75")
pred_45_64_75_plus <- readRDS("./data/models/pred_ir_45_64_75_plus")
pred_65plus_to_15 <- readRDS("./data/models/pred_ir_65plus_to_15")
pred_65plus_15_30 <- readRDS("./data/models/pred_ir_65plus_15_30")
pred_65plus_30_50 <- readRDS("./data/models/pred_ir_65plus_30_50")
pred_65plus_50_75 <- readRDS("./data/models/pred_ir_65plus_50_75")
pred_65plus_75_plus <- readRDS("./data/models/pred_ir_65plus_75_plus")
pred_age_income <- list(pred_18_29_to_15, pred_18_29_15_30, pred_18_29_30_50,
                        pred_18_29_50_75, pred_18_29_75_plus, pred_30_44_to_15,
                        pred_30_44_15_30, pred_30_44_30_50, pred_30_44_50_75,
                        pred_30_44_75_plus, pred_45_64_to_15, pred_45_64_15_30,
                        pred_45_64_30_50, pred_45_64_50_75, pred_45_64_75_plus,
                        pred_65plus_to_15, pred_65plus_15_30, pred_65plus_30_50, 
                        pred_65plus_50_75, pred_65plus_75_plus)
pred_age_income <- pred_age_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 4)
pred_age_income$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 5)

pred_18_29_dem <- readRDS("./data/models/pred_ir_18_29_dem")
pred_18_29_rep <- readRDS("./data/models/pred_ir_18_29_rep")
pred_18_29_none <- readRDS("./data/models/pred_ir_18_29_none")
pred_30_44_dem <- readRDS("./data/models/pred_ir_30_44_dem")
pred_30_44_rep <- readRDS("./data/models/pred_ir_30_44_rep")
pred_30_44_none <- readRDS("./data/models/pred_ir_30_44_none")
pred_45_64_dem <- readRDS("./data/models/pred_ir_45_64_dem")
pred_45_64_rep <- readRDS("./data/models/pred_ir_45_64_rep")
pred_45_64_none <- readRDS("./data/models/pred_ir_45_64_none")
pred_65plus_dem <- readRDS("./data/models/pred_ir_65plus_dem")
pred_65plus_rep <- readRDS("./data/models/pred_ir_65plus_rep")
pred_65plus_none <- readRDS("./data/models/pred_ir_65plus_none")
pred_age_party <- list(pred_18_29_dem, pred_18_29_rep, pred_18_29_none,
                       pred_30_44_dem, pred_30_44_rep, pred_30_44_none, 
                       pred_45_64_dem, pred_45_64_rep, pred_45_64_none, 
                       pred_65plus_dem, pred_65plus_rep, pred_65plus_none)
pred_age_party <- pred_age_party %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_party$variable <- rep(c("dem", "rep", "none"), times = 4)
pred_age_party$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 3)


pred_18_29_female <- readRDS("./data/models/pred_ir_18_29_female")
pred_18_29_male <- readRDS("./data/models/pred_ir_18_29_male")
pred_30_44_female <- readRDS("./data/models/pred_ir_30_44_female")
pred_30_44_male <- readRDS("./data/models/pred_ir_30_44_male")
pred_45_64_female <- readRDS("./data/models/pred_ir_45_64_female")
pred_45_64_male <- readRDS("./data/models/pred_ir_45_64_male")
pred_65plus_female <- readRDS("./data/models/pred_ir_65plus_female")
pred_65plus_male <- readRDS("./data/models/pred_ir_65plus_male")
pred_age_sex <- list(pred_18_29_female, pred_18_29_male,
                     pred_30_44_female, pred_30_44_male, 
                     pred_45_64_female, pred_45_64_male, 
                     pred_65plus_female, pred_65plus_male)
pred_age_sex <- pred_age_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_sex$variable <- rep(c("female", "male"), times = 4)
pred_age_sex$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 2)


pred_age <- rbind(pred_age_income, pred_age_party, pred_age_sex)

pred_age$variable <-factor(pred_age$variable, 
                           levels = c("75_plus", "50_75", "30_50", "15_30",
                                      "to_15", "none", "rep", "dem",
                                      "female", "male"))

pred_age$group <-factor(pred_age$group, 
                        levels = c("65 +", "45-64", "30-44", "18-29"))

subgroups_interactions_2_ir <- ggplot(pred_age, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.924, 0.9285),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("female" = "Female", "male" = "Male",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "            Income                                 Party                        Sex")

ggsave("./figures/subgroups_interactions_2_ir.pdf", subgroups_interactions_2_ir, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

pred_dem_to_15 <- readRDS("./data/models/pred_ir_dem_to_15")
pred_dem_15_30 <- readRDS("./data/models/pred_ir_dem_15_30")
pred_dem_30_50 <- readRDS("./data/models/pred_ir_dem_30_50")
pred_dem_50_75 <- readRDS("./data/models/pred_ir_dem_50_75")
pred_dem_75_plus <- readRDS("./data/models/pred_ir_dem_75_plus")
pred_rep_to_15 <- readRDS("./data/models/pred_ir_rep_to_15")
pred_rep_15_30 <- readRDS("./data/models/pred_ir_rep_15_30")
pred_rep_30_50 <- readRDS("./data/models/pred_ir_rep_30_50")
pred_rep_50_75 <- readRDS("./data/models/pred_ir_rep_50_75")
pred_rep_75_plus <- readRDS("./data/models/pred_ir_rep_75_plus")
pred_none_to_15 <- readRDS("./data/models/pred_ir_none_to_15")
pred_none_15_30 <- readRDS("./data/models/pred_ir_none_15_30")
pred_none_30_50 <- readRDS("./data/models/pred_ir_none_30_50")
pred_none_50_75 <- readRDS("./data/models/pred_ir_none_50_75")
pred_none_75_plus <- readRDS("./data/models/pred_ir_none_75_plus")
pred_party_income <- list(pred_dem_to_15, pred_dem_15_30, pred_dem_30_50,
                          pred_dem_50_75, pred_dem_75_plus, pred_rep_to_15,
                          pred_rep_15_30, pred_rep_30_50, pred_rep_50_75,
                          pred_rep_75_plus, pred_none_to_15, pred_none_15_30,
                          pred_none_30_50, pred_none_50_75, pred_none_75_plus)
pred_party_income <- pred_party_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_party_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 3)
pred_party_income$group <- rep(c("Democrat", "Republican", "None"), each = 5)

pred_dem_female <- readRDS("./data/models/pred_ir_dem_female")
pred_dem_male <- readRDS("./data/models/pred_ir_dem_male")
pred_rep_female <- readRDS("./data/models/pred_ir_rep_female")
pred_rep_male <- readRDS("./data/models/pred_ir_rep_male")
pred_none_female <- readRDS("./data/models/pred_ir_none_female")
pred_none_male <- readRDS("./data/models/pred_ir_none_male")
pred_party_sex <- list(pred_dem_female, pred_dem_male,
                       pred_rep_female, pred_rep_male, 
                       pred_none_female, pred_none_male)
pred_party_sex <- pred_party_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_party_sex$variable <- rep(c("female", "male"), times = 3)
pred_party_sex$group <- rep(c("Democrat", "Republican", "None"), each = 2)

pred_party <- rbind(pred_party_income, pred_party_sex)

pred_party$variable <-factor(pred_party$variable, 
                             levels = c("75_plus", "50_75", "30_50", "15_30",
                                        "to_15", "female", "male"))

pred_party$group <-factor(pred_party$group, 
                          levels = c("None", "Republican", "Democrat"))

subgroups_interactions_3_ir <- ggplot(pred_party, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.115, 0.9435),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("female" = "Female", "male" = "Male",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "              Income                                               Sex")

ggsave("./figures/subgroups_interactions_3_ir.pdf", subgroups_interactions_3_ir, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

pred_female_to_15 <- readRDS("./data/models/pred_ir_female_to_15")
pred_female_15_30 <- readRDS("./data/models/pred_ir_female_15_30")
pred_female_30_50 <- readRDS("./data/models/pred_ir_female_30_50")
pred_female_50_75 <- readRDS("./data/models/pred_ir_female_50_75")
pred_female_75_plus <- readRDS("./data/models/pred_ir_female_75_plus")
pred_male_to_15 <- readRDS("./data/models/pred_ir_male_to_15")
pred_male_15_30 <- readRDS("./data/models/pred_ir_male_15_30")
pred_male_30_50 <- readRDS("./data/models/pred_ir_male_30_50")
pred_male_50_75 <- readRDS("./data/models/pred_ir_male_50_75")
pred_male_75_plus <- readRDS("./data/models/pred_ir_male_75_plus")
pred_sex_income <- list(pred_female_to_15, pred_female_15_30, pred_female_30_50,
                        pred_female_50_75, pred_female_75_plus, pred_male_to_15,
                        pred_male_15_30, pred_male_30_50, pred_male_50_75,
                        pred_male_75_plus)
pred_sex_income <- pred_sex_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_sex_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 2)
pred_sex_income$group <- rep(c("Female", "Male"), each = 5)

pred_sex <- pred_sex_income
pred_sex$variable <-factor(pred_sex$variable, 
                           levels = c("75_plus", "50_75", "30_50", "15_30",
                                      "to_15"))
pred_sex$group <-factor(pred_sex$group, 
                        levels = c("Female", "Male"))

subgroups_interactions_4_ir <- ggplot(pred_sex, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.915, 0.959),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25),
                     breaks = seq(0.05, 0.20, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%")) +
  labs(y = "", x = "Income")

ggsave("./figures/subgroups_interactions_4_ir.pdf", subgroups_interactions_4_ir, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

# import population averaged predictions for interacted subgroups -----------------------
pred_white_to_15 <- readRDS("./data/models/pred_re_white_to_15")
pred_white_15_30 <- readRDS("./data/models/pred_re_white_15_30")
pred_white_30_50 <- readRDS("./data/models/pred_re_white_30_50")
pred_white_50_75 <- readRDS("./data/models/pred_re_white_50_75")
pred_white_75_plus <- readRDS("./data/models/pred_re_white_75_plus")
pred_black_to_15 <- readRDS("./data/models/pred_re_black_to_15")
pred_black_15_30 <- readRDS("./data/models/pred_re_black_15_30")
pred_black_30_50 <- readRDS("./data/models/pred_re_black_30_50")
pred_black_50_75 <- readRDS("./data/models/pred_re_black_50_75")
pred_black_75_plus <- readRDS("./data/models/pred_re_black_75_plus")
pred_hispanic_to_15 <- readRDS("./data/models/pred_re_hispanic_to_15")
pred_hispanic_15_30 <- readRDS("./data/models/pred_re_hispanic_15_30")
pred_hispanic_30_50 <- readRDS("./data/models/pred_re_hispanic_30_50")
pred_hispanic_50_75 <- readRDS("./data/models/pred_re_hispanic_50_75")
pred_hispanic_75_plus <- readRDS("./data/models/pred_re_hispanic_75_plus")
pred_other_to_15 <- readRDS("./data/models/pred_re_other_to_15")
pred_other_15_30 <- readRDS("./data/models/pred_re_other_15_30")
pred_other_30_50 <- readRDS("./data/models/pred_re_other_30_50")
pred_other_50_75 <- readRDS("./data/models/pred_re_other_50_75")
pred_other_75_plus <- readRDS("./data/models/pred_re_other_75_plus")
pred_race_income <- list(pred_white_to_15, pred_white_15_30, pred_white_30_50,
                         pred_white_50_75, pred_white_75_plus, pred_black_to_15,
                         pred_black_15_30, pred_black_30_50, pred_black_50_75,
                         pred_black_75_plus, pred_hispanic_to_15, pred_hispanic_15_30,
                         pred_hispanic_30_50, pred_hispanic_50_75, pred_hispanic_75_plus,
                         pred_other_to_15, pred_other_15_30, pred_other_30_50, pred_other_50_75,
                         pred_other_75_plus)
pred_race_income <- pred_race_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 4)
pred_race_income$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 5)

pred_white_18_29 <- readRDS("./data/models/pred_re_white_18_29")
pred_white_30_44 <- readRDS("./data/models/pred_re_white_30_44")
pred_white_45_64 <- readRDS("./data/models/pred_re_white_45_64")
pred_white_60plus <- readRDS("./data/models/pred_re_white_60plus")
pred_black_18_29 <- readRDS("./data/models/pred_re_black_18_29")
pred_black_30_44 <- readRDS("./data/models/pred_re_black_30_44")
pred_black_45_64 <- readRDS("./data/models/pred_re_black_45_64")
pred_black_60plus <- readRDS("./data/models/pred_re_black_60plus")
pred_hispanic_18_29 <- readRDS("./data/models/pred_re_hispanic_18_29")
pred_hispanic_30_44 <- readRDS("./data/models/pred_re_hispanic_30_44")
pred_hispanic_45_64 <- readRDS("./data/models/pred_re_hispanic_45_64")
pred_hispanic_60plus <- readRDS("./data/models/pred_re_hispanic_60plus")
pred_other_18_29 <- readRDS("./data/models/pred_re_other_18_29")
pred_other_30_44 <- readRDS("./data/models/pred_re_other_30_44")
pred_other_45_64 <- readRDS("./data/models/pred_re_other_45_64")
pred_other_60plus <- readRDS("./data/models/pred_re_other_60plus")
pred_race_age <- list(pred_white_18_29, pred_white_30_44, pred_white_45_64,
                      pred_white_60plus, 
                      pred_black_18_29, pred_black_30_44, pred_black_45_64, 
                      pred_black_60plus,
                      pred_hispanic_18_29, pred_hispanic_30_44, pred_hispanic_45_64, 
                      pred_hispanic_60plus,
                      pred_other_18_29, pred_other_30_44, pred_other_45_64, 
                      pred_other_60plus)
pred_race_age<- pred_race_age %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_age$variable <- rep(c("18_29", "30_44", "45_64", "60plus"), times = 4)
pred_race_age$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 4)

pred_white_dem <- readRDS("./data/models/pred_re_white_dem")
pred_white_rep <- readRDS("./data/models/pred_re_white_rep")
pred_white_none <- readRDS("./data/models/pred_re_white_none")
pred_black_dem <- readRDS("./data/models/pred_re_black_dem")
pred_black_rep <- readRDS("./data/models/pred_re_black_rep")
pred_black_none <- readRDS("./data/models/pred_re_black_none")
pred_hispanic_dem <- readRDS("./data/models/pred_re_hispanic_dem")
pred_hispanic_rep <- readRDS("./data/models/pred_re_hispanic_rep")
pred_hispanic_none <- readRDS("./data/models/pred_re_hispanic_none")
pred_other_dem <- readRDS("./data/models/pred_re_other_dem")
pred_other_rep <- readRDS("./data/models/pred_re_other_rep")
pred_other_none <- readRDS("./data/models/pred_re_other_none")
pred_race_party <- list(pred_white_dem, pred_white_rep, pred_white_none,
                        pred_black_dem, pred_black_rep, pred_black_none, 
                        pred_hispanic_dem, pred_hispanic_rep, pred_hispanic_none, 
                        pred_other_dem, pred_other_rep, pred_other_none)
pred_race_party <- pred_race_party %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_party$variable <- rep(c("dem", "rep", "none"), times = 4)
pred_race_party$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 3)

pred_white_female <- readRDS("./data/models/pred_re_white_female")
pred_white_male <- readRDS("./data/models/pred_re_white_male")
pred_black_female <- readRDS("./data/models/pred_re_black_female")
pred_black_male <- readRDS("./data/models/pred_re_black_male")
pred_hispanic_female <- readRDS("./data/models/pred_re_hispanic_female")
pred_hispanic_male <- readRDS("./data/models/pred_re_hispanic_male")
pred_other_female <- readRDS("./data/models/pred_re_other_female")
pred_other_male <- readRDS("./data/models/pred_re_other_male")
pred_race_sex <- list(pred_white_female, pred_white_male,
                      pred_black_female, pred_black_male, 
                      pred_hispanic_female, pred_hispanic_male, 
                      pred_other_female, pred_other_male)
pred_race_sex <- pred_race_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_sex$variable <- rep(c("female", "male"), times = 4)
pred_race_sex$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 2)

pred_race <- rbind(pred_race_income, pred_race_age, pred_race_party, pred_race_sex)

pred_race$variable <-factor(pred_race$variable, 
                            levels = c("75_plus", "50_75", "30_50", "15_30",
                                       "to_15", "none", "rep", "dem",
                                       "60plus", "45_64", "30_44",
                                       "18_29", "female", "male"))
pred_race$group <-factor(pred_race$group, 
                         levels = c("Other", "White", "Hispanic", "Black"))

subgroups_interactions_1_re <- ggplot(pred_race, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.9, 0.9285),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("60plus" = "65 +", "45_64" = "45-64", "30_44" = "30-44",
                              "18_29" = "18-29", "female" = "Female", "male" = "Male",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.35),
                     breaks = seq(0.05, 0.3, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%", "25%", "30%")) +
  labs(y = "", x = "        Income                       Party                      Age             Sex")

ggsave("./figures/subgroups_interactions_1_re.png", subgroups_interactions_1_re, width = 6.5, height = 8.5, dpi = 320)

pred_18_29_to_15 <- readRDS("./data/models/pred_re_18_29_to_15")
pred_18_29_15_30 <- readRDS("./data/models/pred_re_18_29_15_30")
pred_18_29_30_50 <- readRDS("./data/models/pred_re_18_29_30_50")
pred_18_29_50_75 <- readRDS("./data/models/pred_re_18_29_50_75")
pred_18_29_75_plus <- readRDS("./data/models/pred_re_18_29_75_plus")
pred_30_44_to_15 <- readRDS("./data/models/pred_re_30_44_to_15")
pred_30_44_15_30 <- readRDS("./data/models/pred_re_30_44_15_30")
pred_30_44_30_50 <- readRDS("./data/models/pred_re_30_44_30_50")
pred_30_44_50_75 <- readRDS("./data/models/pred_re_30_44_50_75")
pred_30_44_75_plus <- readRDS("./data/models/pred_re_30_44_75_plus")
pred_45_64_to_15 <- readRDS("./data/models/pred_re_45_64_to_15")
pred_45_64_15_30 <- readRDS("./data/models/pred_re_45_64_15_30")
pred_45_64_30_50 <- readRDS("./data/models/pred_re_45_64_30_50")
pred_45_64_50_75 <- readRDS("./data/models/pred_re_45_64_50_75")
pred_45_64_75_plus <- readRDS("./data/models/pred_re_45_64_75_plus")
pred_65plus_to_15 <- readRDS("./data/models/pred_re_65plus_to_15")
pred_65plus_15_30 <- readRDS("./data/models/pred_re_65plus_15_30")
pred_65plus_30_50 <- readRDS("./data/models/pred_re_65plus_30_50")
pred_65plus_50_75 <- readRDS("./data/models/pred_re_65plus_50_75")
pred_65plus_75_plus <- readRDS("./data/models/pred_re_65plus_75_plus")
pred_age_income <- list(pred_18_29_to_15, pred_18_29_15_30, pred_18_29_30_50,
                        pred_18_29_50_75, pred_18_29_75_plus, pred_30_44_to_15,
                        pred_30_44_15_30, pred_30_44_30_50, pred_30_44_50_75,
                        pred_30_44_75_plus, pred_45_64_to_15, pred_45_64_15_30,
                        pred_45_64_30_50, pred_45_64_50_75, pred_45_64_75_plus,
                        pred_65plus_to_15, pred_65plus_15_30, pred_65plus_30_50, 
                        pred_65plus_50_75, pred_65plus_75_plus)
pred_age_income <- pred_age_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 4)
pred_age_income$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 5)

pred_18_29_dem <- readRDS("./data/models/pred_re_18_29_dem")
pred_18_29_rep <- readRDS("./data/models/pred_re_18_29_rep")
pred_18_29_none <- readRDS("./data/models/pred_re_18_29_none")
pred_30_44_dem <- readRDS("./data/models/pred_re_30_44_dem")
pred_30_44_rep <- readRDS("./data/models/pred_re_30_44_rep")
pred_30_44_none <- readRDS("./data/models/pred_re_30_44_none")
pred_45_64_dem <- readRDS("./data/models/pred_re_45_64_dem")
pred_45_64_rep <- readRDS("./data/models/pred_re_45_64_rep")
pred_45_64_none <- readRDS("./data/models/pred_re_45_64_none")
pred_65plus_dem <- readRDS("./data/models/pred_re_65plus_dem")
pred_65plus_rep <- readRDS("./data/models/pred_re_65plus_rep")
pred_65plus_none <- readRDS("./data/models/pred_re_65plus_none")
pred_age_party <- list(pred_18_29_dem, pred_18_29_rep, pred_18_29_none,
                       pred_30_44_dem, pred_30_44_rep, pred_30_44_none, 
                       pred_45_64_dem, pred_45_64_rep, pred_45_64_none, 
                       pred_65plus_dem, pred_65plus_rep, pred_65plus_none)
pred_age_party <- pred_age_party %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_party$variable <- rep(c("dem", "rep", "none"), times = 4)
pred_age_party$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 3)

pred_18_29_female <- readRDS("./data/models/pred_re_18_29_female")
pred_18_29_male <- readRDS("./data/models/pred_re_18_29_male")
pred_30_44_female <- readRDS("./data/models/pred_re_30_44_female")
pred_30_44_male <- readRDS("./data/models/pred_re_30_44_male")
pred_45_64_female <- readRDS("./data/models/pred_re_45_64_female")
pred_45_64_male <- readRDS("./data/models/pred_re_45_64_male")
pred_65plus_female <- readRDS("./data/models/pred_re_65plus_female")
pred_65plus_male <- readRDS("./data/models/pred_re_65plus_male")
pred_age_sex <- list(pred_18_29_female, pred_18_29_male,
                     pred_30_44_female, pred_30_44_male, 
                     pred_45_64_female, pred_45_64_male, 
                     pred_65plus_female, pred_65plus_male)
pred_age_sex <- pred_age_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_sex$variable <- rep(c("female", "male"), times = 4)
pred_age_sex$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 2)

pred_age <- rbind(pred_age_income, pred_age_party, pred_age_sex)

pred_age$variable <-factor(pred_age$variable, 
                           levels = c("75_plus", "50_75", "30_50", "15_30",
                                      "to_15", "none", "rep", "dem",
                                      "female", "male"))
pred_age$group <-factor(pred_age$group, 
                        levels = c("65 +", "45-64", "30-44", "18-29"))

subgroups_interactions_2_re <- ggplot(pred_age, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.924, 0.9285),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("female" = "Female", "male" = "Male",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.35),
                     breaks = seq(0.05, 0.3, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%", "25%", "30%")) +
  labs(y = "", x = "            Income                                 Party                        Sex")

ggsave("./figures/subgroups_interactions_2_re.pdf", subgroups_interactions_2_re, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

pred_dem_to_15 <- readRDS("./data/models/pred_re_dem_to_15")
pred_dem_15_30 <- readRDS("./data/models/pred_re_dem_15_30")
pred_dem_30_50 <- readRDS("./data/models/pred_re_dem_30_50")
pred_dem_50_75 <- readRDS("./data/models/pred_re_dem_50_75")
pred_dem_75_plus <- readRDS("./data/models/pred_re_dem_75_plus")
pred_rep_to_15 <- readRDS("./data/models/pred_re_rep_to_15")
pred_rep_15_30 <- readRDS("./data/models/pred_re_rep_15_30")
pred_rep_30_50 <- readRDS("./data/models/pred_re_rep_30_50")
pred_rep_50_75 <- readRDS("./data/models/pred_re_rep_50_75")
pred_rep_75_plus <- readRDS("./data/models/pred_re_rep_75_plus")
pred_none_to_15 <- readRDS("./data/models/pred_re_none_to_15")
pred_none_15_30 <- readRDS("./data/models/pred_re_none_15_30")
pred_none_30_50 <- readRDS("./data/models/pred_re_none_30_50")
pred_none_50_75 <- readRDS("./data/models/pred_re_none_50_75")
pred_none_75_plus <- readRDS("./data/models/pred_re_none_75_plus")
pred_party_income <- list(pred_dem_to_15, pred_dem_15_30, pred_dem_30_50,
                          pred_dem_50_75, pred_dem_75_plus, pred_rep_to_15,
                          pred_rep_15_30, pred_rep_30_50, pred_rep_50_75,
                          pred_rep_75_plus, pred_none_to_15, pred_none_15_30,
                          pred_none_30_50, pred_none_50_75, pred_none_75_plus)
pred_party_income <- pred_party_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_party_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 3)
pred_party_income$group <- rep(c("Democrat", "Republican", "None"), each = 5)

pred_dem_female <- readRDS("./data/models/pred_re_dem_female")
pred_dem_male <- readRDS("./data/models/pred_re_dem_male")
pred_rep_female <- readRDS("./data/models/pred_re_rep_female")
pred_rep_male <- readRDS("./data/models/pred_re_rep_male")
pred_none_female <- readRDS("./data/models/pred_re_none_female")
pred_none_male <- readRDS("./data/models/pred_re_none_male")
pred_party_sex <- list(pred_dem_female, pred_dem_male,
                       pred_rep_female, pred_rep_male, 
                       pred_none_female, pred_none_male)
pred_party_sex <- pred_party_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_party_sex$variable <- rep(c("female", "male"), times = 3)
pred_party_sex$group <- rep(c("Democrat", "Republican", "None"), each = 2)

pred_party <- rbind(pred_party_income, pred_party_sex)

pred_party$variable <-factor(pred_party$variable, 
                             levels = c("75_plus", "50_75", "30_50", "15_30",
                                        "to_15", "female", "male"))

pred_party$group <-factor(pred_party$group, 
                          levels = c("None", "Republican", "Democrat"))

subgroups_interactions_3_re <- ggplot(pred_party, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.115, 0.9435),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("female" = "Female", "male" = "Male",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.35),
                     breaks = seq(0.05, 0.3, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%", "25%", "30%")) +
  labs(y = "", x = "              Income                                               Sex")

ggsave("./figures/subgroups_interactions_3_re.pdf", subgroups_interactions_3_re, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

pred_female_to_15 <- readRDS("./data/models/pred_re_female_to_15")
pred_female_15_30 <- readRDS("./data/models/pred_re_female_15_30")
pred_female_30_50 <- readRDS("./data/models/pred_re_female_30_50")
pred_female_50_75 <- readRDS("./data/models/pred_re_female_50_75")
pred_female_75_plus <- readRDS("./data/models/pred_re_female_75_plus")
pred_male_to_15 <- readRDS("./data/models/pred_re_male_to_15")
pred_male_15_30 <- readRDS("./data/models/pred_re_male_15_30")
pred_male_30_50 <- readRDS("./data/models/pred_re_male_30_50")
pred_male_50_75 <- readRDS("./data/models/pred_re_male_50_75")
pred_male_75_plus <- readRDS("./data/models/pred_re_male_75_plus")
pred_sex_income <- list(pred_female_to_15, pred_female_15_30, pred_female_30_50,
                        pred_female_50_75, pred_female_75_plus, pred_male_to_15,
                        pred_male_15_30, pred_male_30_50, pred_male_50_75,
                        pred_male_75_plus)
pred_sex_income <- pred_sex_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_sex_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 2)
pred_sex_income$group <- rep(c("Female", "Male"), each = 5)

pred_sex <- pred_sex_income
pred_sex$variable <-factor(pred_sex$variable, 
                           levels = c("75_plus", "50_75", "30_50", "15_30",
                                      "to_15"))
pred_sex$group <-factor(pred_sex$group, 
                        levels = c("Female", "Male"))

subgroups_interactions_4_re <- ggplot(pred_sex, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.915, 0.959),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.35),
                     breaks = seq(0.05, 0.3, by = 0.05),
                     labels = c("5%", "10%", "15%", "20%", "25%", "30%")) +
  labs(y = "", x = "Income")

ggsave("./figures/subgroups_interactions_4_re.pdf", subgroups_interactions_4_re, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

# import population averaged predictions for interacted subgroups -----------------------
pred_white_to_15 <- readRDS("./data/models/pred_he_white_to_15")
pred_white_15_30 <- readRDS("./data/models/pred_he_white_15_30")
pred_white_30_50 <- readRDS("./data/models/pred_he_white_30_50")
pred_white_50_75 <- readRDS("./data/models/pred_he_white_50_75")
pred_white_75_plus <- readRDS("./data/models/pred_he_white_75_plus")
pred_black_to_15 <- readRDS("./data/models/pred_he_black_to_15")
pred_black_15_30 <- readRDS("./data/models/pred_he_black_15_30")
pred_black_30_50 <- readRDS("./data/models/pred_he_black_30_50")
pred_black_50_75 <- readRDS("./data/models/pred_he_black_50_75")
pred_black_75_plus <- readRDS("./data/models/pred_he_black_75_plus")
pred_hispanic_to_15 <- readRDS("./data/models/pred_he_hispanic_to_15")
pred_hispanic_15_30 <- readRDS("./data/models/pred_he_hispanic_15_30")
pred_hispanic_30_50 <- readRDS("./data/models/pred_he_hispanic_30_50")
pred_hispanic_50_75 <- readRDS("./data/models/pred_he_hispanic_50_75")
pred_hispanic_75_plus <- readRDS("./data/models/pred_he_hispanic_75_plus")
pred_other_to_15 <- readRDS("./data/models/pred_he_other_to_15")
pred_other_15_30 <- readRDS("./data/models/pred_he_other_15_30")
pred_other_30_50 <- readRDS("./data/models/pred_he_other_30_50")
pred_other_50_75 <- readRDS("./data/models/pred_he_other_50_75")
pred_other_75_plus <- readRDS("./data/models/pred_he_other_75_plus")
pred_race_income <- list(pred_white_to_15, pred_white_15_30, pred_white_30_50,
                         pred_white_50_75, pred_white_75_plus, pred_black_to_15,
                         pred_black_15_30, pred_black_30_50, pred_black_50_75,
                         pred_black_75_plus, pred_hispanic_to_15, pred_hispanic_15_30,
                         pred_hispanic_30_50, pred_hispanic_50_75, pred_hispanic_75_plus,
                         pred_other_to_15, pred_other_15_30, pred_other_30_50, pred_other_50_75,
                         pred_other_75_plus)
pred_race_income <- pred_race_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 4)
pred_race_income$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 5)

pred_white_18_29 <- readRDS("./data/models/pred_he_white_18_29")
pred_white_30_44 <- readRDS("./data/models/pred_he_white_30_44")
pred_white_45_64 <- readRDS("./data/models/pred_he_white_45_64")
pred_white_60plus <- readRDS("./data/models/pred_he_white_60plus")
pred_black_18_29 <- readRDS("./data/models/pred_he_black_18_29")
pred_black_30_44 <- readRDS("./data/models/pred_he_black_30_44")
pred_black_45_64 <- readRDS("./data/models/pred_he_black_45_64")
pred_black_60plus <- readRDS("./data/models/pred_he_black_60plus")
pred_hispanic_18_29 <- readRDS("./data/models/pred_he_hispanic_18_29")
pred_hispanic_30_44 <- readRDS("./data/models/pred_he_hispanic_30_44")
pred_hispanic_45_64 <- readRDS("./data/models/pred_he_hispanic_45_64")
pred_hispanic_60plus <- readRDS("./data/models/pred_he_hispanic_60plus")
pred_other_18_29 <- readRDS("./data/models/pred_he_other_18_29")
pred_other_30_44 <- readRDS("./data/models/pred_he_other_30_44")
pred_other_45_64 <- readRDS("./data/models/pred_he_other_45_64")
pred_other_60plus <- readRDS("./data/models/pred_he_other_60plus")
pred_race_age <- list(pred_white_18_29, pred_white_30_44, pred_white_45_64,
                      pred_white_60plus, 
                      pred_black_18_29, pred_black_30_44, pred_black_45_64, 
                      pred_black_60plus,
                      pred_hispanic_18_29, pred_hispanic_30_44, pred_hispanic_45_64, 
                      pred_hispanic_60plus,
                      pred_other_18_29, pred_other_30_44, pred_other_45_64, 
                      pred_other_60plus)
pred_race_age<- pred_race_age %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_age$variable <- rep(c("18_29", "30_44", "45_64", "60plus"), times = 4)
pred_race_age$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 4)

pred_white_dem <- readRDS("./data/models/pred_he_white_dem")
pred_white_rep <- readRDS("./data/models/pred_he_white_rep")
pred_white_none <- readRDS("./data/models/pred_he_white_none")
pred_black_dem <- readRDS("./data/models/pred_he_black_dem")
pred_black_rep <- readRDS("./data/models/pred_he_black_rep")
pred_black_none <- readRDS("./data/models/pred_he_black_none")
pred_hispanic_dem <- readRDS("./data/models/pred_he_hispanic_dem")
pred_hispanic_rep <- readRDS("./data/models/pred_he_hispanic_rep")
pred_hispanic_none <- readRDS("./data/models/pred_he_hispanic_none")
pred_other_dem <- readRDS("./data/models/pred_he_other_dem")
pred_other_rep <- readRDS("./data/models/pred_he_other_rep")
pred_other_none <- readRDS("./data/models/pred_he_other_none")
pred_race_party <- list(pred_white_dem, pred_white_rep, pred_white_none,
                        pred_black_dem, pred_black_rep, pred_black_none, 
                        pred_hispanic_dem, pred_hispanic_rep, pred_hispanic_none, 
                        pred_other_dem, pred_other_rep, pred_other_none)
pred_race_party <- pred_race_party %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_party$variable <- rep(c("dem", "rep", "none"), times = 4)
pred_race_party$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 3)

pred_white_female <- readRDS("./data/models/pred_he_white_female")
pred_white_male <- readRDS("./data/models/pred_he_white_male")
pred_black_female <- readRDS("./data/models/pred_he_black_female")
pred_black_male <- readRDS("./data/models/pred_he_black_male")
pred_hispanic_female <- readRDS("./data/models/pred_he_hispanic_female")
pred_hispanic_male <- readRDS("./data/models/pred_he_hispanic_male")
pred_other_female <- readRDS("./data/models/pred_he_other_female")
pred_other_male <- readRDS("./data/models/pred_he_other_male")
pred_race_sex <- list(pred_white_female, pred_white_male,
                      pred_black_female, pred_black_male, 
                      pred_hispanic_female, pred_hispanic_male, 
                      pred_other_female, pred_other_male)
pred_race_sex <- pred_race_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_race_sex$variable <- rep(c("female", "male"), times = 4)
pred_race_sex$group <- rep(c("White", "Black", "Hispanic", "Other"), each = 2)

pred_race <- rbind(pred_race_income, pred_race_age, pred_race_party, pred_race_sex)

pred_race$variable <-factor(pred_race$variable, 
                            levels = c("75_plus", "50_75", "30_50", "15_30",
                                       "to_15", "none", "rep", "dem",
                                       "60plus", "45_64", "30_44",
                                       "18_29", "female", "male"))
pred_race$group <-factor(pred_race$group, 
                         levels = c("Other", "White", "Hispanic", "Black"))



subgroups_interactions_1_he <- ggplot(pred_race, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.9, 0.9285),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("60plus" = "65 +", "45_64" = "45-64", "30_44" = "30-44",
                              "18_29" = "18-29", "female" = "Female", "male" = "Male",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6),
                     breaks = seq(0.1, 0.5, by = 0.1),
                     labels = c("10%", "20%", "30%", "40%", "50%")) +
  labs(y = "", x = "        Income                       Party                      Age             Sex")

ggsave("./figures/subgroups_interactions_1_he.png", subgroups_interactions_1_he, width = 6.5, height = 8.5, dpi = 320)

pred_18_29_to_15 <- readRDS("./data/models/pred_he_18_29_to_15")
pred_18_29_15_30 <- readRDS("./data/models/pred_he_18_29_15_30")
pred_18_29_30_50 <- readRDS("./data/models/pred_he_18_29_30_50")
pred_18_29_50_75 <- readRDS("./data/models/pred_he_18_29_50_75")
pred_18_29_75_plus <- readRDS("./data/models/pred_he_18_29_75_plus")
pred_30_44_to_15 <- readRDS("./data/models/pred_he_30_44_to_15")
pred_30_44_15_30 <- readRDS("./data/models/pred_he_30_44_15_30")
pred_30_44_30_50 <- readRDS("./data/models/pred_he_30_44_30_50")
pred_30_44_50_75 <- readRDS("./data/models/pred_he_30_44_50_75")
pred_30_44_75_plus <- readRDS("./data/models/pred_he_30_44_75_plus")
pred_45_64_to_15 <- readRDS("./data/models/pred_he_45_64_to_15")
pred_45_64_15_30 <- readRDS("./data/models/pred_he_45_64_15_30")
pred_45_64_30_50 <- readRDS("./data/models/pred_he_45_64_30_50")
pred_45_64_50_75 <- readRDS("./data/models/pred_he_45_64_50_75")
pred_45_64_75_plus <- readRDS("./data/models/pred_he_45_64_75_plus")
pred_65plus_to_15 <- readRDS("./data/models/pred_he_65plus_to_15")
pred_65plus_15_30 <- readRDS("./data/models/pred_he_65plus_15_30")
pred_65plus_30_50 <- readRDS("./data/models/pred_he_65plus_30_50")
pred_65plus_50_75 <- readRDS("./data/models/pred_he_65plus_50_75")
pred_65plus_75_plus <- readRDS("./data/models/pred_he_65plus_75_plus")
pred_age_income <- list(pred_18_29_to_15, pred_18_29_15_30, pred_18_29_30_50,
                        pred_18_29_50_75, pred_18_29_75_plus, pred_30_44_to_15,
                        pred_30_44_15_30, pred_30_44_30_50, pred_30_44_50_75,
                        pred_30_44_75_plus, pred_45_64_to_15, pred_45_64_15_30,
                        pred_45_64_30_50, pred_45_64_50_75, pred_45_64_75_plus,
                        pred_65plus_to_15, pred_65plus_15_30, pred_65plus_30_50, 
                        pred_65plus_50_75, pred_65plus_75_plus)
pred_age_income <- pred_age_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 4)
pred_age_income$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 5)

pred_18_29_dem <- readRDS("./data/models/pred_he_18_29_dem")
pred_18_29_rep <- readRDS("./data/models/pred_he_18_29_rep")
pred_18_29_none <- readRDS("./data/models/pred_he_18_29_none")
pred_30_44_dem <- readRDS("./data/models/pred_he_30_44_dem")
pred_30_44_rep <- readRDS("./data/models/pred_he_30_44_rep")
pred_30_44_none <- readRDS("./data/models/pred_he_30_44_none")
pred_45_64_dem <- readRDS("./data/models/pred_he_45_64_dem")
pred_45_64_rep <- readRDS("./data/models/pred_he_45_64_rep")
pred_45_64_none <- readRDS("./data/models/pred_he_45_64_none")
pred_65plus_dem <- readRDS("./data/models/pred_he_65plus_dem")
pred_65plus_rep <- readRDS("./data/models/pred_he_65plus_rep")
pred_65plus_none <- readRDS("./data/models/pred_he_65plus_none")
pred_age_party <- list(pred_18_29_dem, pred_18_29_rep, pred_18_29_none,
                       pred_30_44_dem, pred_30_44_rep, pred_30_44_none, 
                       pred_45_64_dem, pred_45_64_rep, pred_45_64_none, 
                       pred_65plus_dem, pred_65plus_rep, pred_65plus_none)
pred_age_party <- pred_age_party %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_party$variable <- rep(c("dem", "rep", "none"), times = 4)
pred_age_party$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 3)

pred_18_29_female <- readRDS("./data/models/pred_he_18_29_female")
pred_18_29_male <- readRDS("./data/models/pred_he_18_29_male")
pred_30_44_female <- readRDS("./data/models/pred_he_30_44_female")
pred_30_44_male <- readRDS("./data/models/pred_he_30_44_male")
pred_45_64_female <- readRDS("./data/models/pred_he_45_64_female")
pred_45_64_male <- readRDS("./data/models/pred_he_45_64_male")
pred_65plus_female <- readRDS("./data/models/pred_he_65plus_female")
pred_65plus_male <- readRDS("./data/models/pred_he_65plus_male")
pred_age_sex <- list(pred_18_29_female, pred_18_29_male,
                     pred_30_44_female, pred_30_44_male, 
                     pred_45_64_female, pred_45_64_male, 
                     pred_65plus_female, pred_65plus_male)
pred_age_sex <- pred_age_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_age_sex$variable <- rep(c("female", "male"), times = 4)
pred_age_sex$group <- rep(c("18-29", "30-44", "45-64", "65 +"), each = 2)


pred_age <- rbind(pred_age_income, pred_age_party, pred_age_sex)

pred_age$variable <-factor(pred_age$variable, 
                           levels = c("75_plus", "50_75", "30_50", "15_30",
                                      "to_15", "none", "rep", "dem",
                                      "female", "male"))

pred_age$group <-factor(pred_age$group, 
                        levels = c("65 +", "45-64", "30-44", "18-29"))

subgroups_interactions_2_he <- ggplot(pred_age, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.924, 0.9285),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("female" = "Female", "male" = "Male",
                              "dem" = "Democrat", "rep" = "Republican", "none" = "None",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6),
                     breaks = seq(0.1, 0.5, by = 0.1),
                     labels = c("10%", "20%", "30%", "40%", "50%")) +
  labs(y = "", x = "            Income                                 Party                        Sex")

ggsave("./figures/subgroups_interactions_2_he.pdf", subgroups_interactions_2_he, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

pred_dem_to_15 <- readRDS("./data/models/pred_he_dem_to_15")
pred_dem_15_30 <- readRDS("./data/models/pred_he_dem_15_30")
pred_dem_30_50 <- readRDS("./data/models/pred_he_dem_30_50")
pred_dem_50_75 <- readRDS("./data/models/pred_he_dem_50_75")
pred_dem_75_plus <- readRDS("./data/models/pred_he_dem_75_plus")
pred_rep_to_15 <- readRDS("./data/models/pred_he_rep_to_15")
pred_rep_15_30 <- readRDS("./data/models/pred_he_rep_15_30")
pred_rep_30_50 <- readRDS("./data/models/pred_he_rep_30_50")
pred_rep_50_75 <- readRDS("./data/models/pred_he_rep_50_75")
pred_rep_75_plus <- readRDS("./data/models/pred_he_rep_75_plus")
pred_none_to_15 <- readRDS("./data/models/pred_he_none_to_15")
pred_none_15_30 <- readRDS("./data/models/pred_he_none_15_30")
pred_none_30_50 <- readRDS("./data/models/pred_he_none_30_50")
pred_none_50_75 <- readRDS("./data/models/pred_he_none_50_75")
pred_none_75_plus <- readRDS("./data/models/pred_he_none_75_plus")
pred_party_income <- list(pred_dem_to_15, pred_dem_15_30, pred_dem_30_50,
                          pred_dem_50_75, pred_dem_75_plus, pred_rep_to_15,
                          pred_rep_15_30, pred_rep_30_50, pred_rep_50_75,
                          pred_rep_75_plus, pred_none_to_15, pred_none_15_30,
                          pred_none_30_50, pred_none_50_75, pred_none_75_plus)
pred_party_income <- pred_party_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_party_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 3)
pred_party_income$group <- rep(c("Democrat", "Republican", "None"), each = 5)

pred_dem_female <- readRDS("./data/models/pred_he_dem_female")
pred_dem_male <- readRDS("./data/models/pred_he_dem_male")
pred_rep_female <- readRDS("./data/models/pred_he_rep_female")
pred_rep_male <- readRDS("./data/models/pred_he_rep_male")
pred_none_female <- readRDS("./data/models/pred_he_none_female")
pred_none_male <- readRDS("./data/models/pred_he_none_male")
pred_party_sex <- list(pred_dem_female, pred_dem_male,
                       pred_rep_female, pred_rep_male, 
                       pred_none_female, pred_none_male)
pred_party_sex <- pred_party_sex %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_party_sex$variable <- rep(c("female", "male"), times = 3)
pred_party_sex$group <- rep(c("Democrat", "Republican", "None"), each = 2)

pred_party <- rbind(pred_party_income, pred_party_sex)

pred_party$variable <-factor(pred_party$variable, 
                             levels = c("75_plus", "50_75", "30_50", "15_30",
                                        "to_15", "female", "male"))

pred_party$group <-factor(pred_party$group, 
                          levels = c("None", "Republican", "Democrat"))

subgroups_interactions_3_he <- ggplot(pred_party, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.115, 0.9435),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("female" = "Female", "male" = "Male",
                              "to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6),
                     breaks = seq(0.1, 0.5, by = 0.1),
                     labels = c("10%", "20%", "30%", "40%", "50%")) +
  labs(y = "", x = "              Income                                               Sex")

ggsave("./figures/subgroups_interactions_3_he.pdf", subgroups_interactions_3_he, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

pred_female_to_15 <- readRDS("./data/models/pred_he_female_to_15")
pred_female_15_30 <- readRDS("./data/models/pred_he_female_15_30")
pred_female_30_50 <- readRDS("./data/models/pred_he_female_30_50")
pred_female_50_75 <- readRDS("./data/models/pred_he_female_50_75")
pred_female_75_plus <- readRDS("./data/models/pred_he_female_75_plus")
pred_male_to_15 <- readRDS("./data/models/pred_he_male_to_15")
pred_male_15_30 <- readRDS("./data/models/pred_he_male_15_30")
pred_male_30_50 <- readRDS("./data/models/pred_he_male_30_50")
pred_male_50_75 <- readRDS("./data/models/pred_he_male_50_75")
pred_male_75_plus <- readRDS("./data/models/pred_he_male_75_plus")
pred_sex_income <- list(pred_female_to_15, pred_female_15_30, pred_female_30_50,
                        pred_female_50_75, pred_female_75_plus, pred_male_to_15,
                        pred_male_15_30, pred_male_30_50, pred_male_50_75,
                        pred_male_75_plus)
pred_sex_income <- pred_sex_income %>%
  map_dfr(~ .x %>% 
            group_by(x) %>%
            summarize(lower = quantile(y, prob = 0.025),
                      upper = quantile(y, prob = 0.975),
                      fit = quantile(y, prob = 0.50)))
pred_sex_income$variable <- rep(c("to_15", "15_30", "30_50", "50_75", "75_plus"), times = 2)
pred_sex_income$group <- rep(c("Female", "Male"), each = 5)

pred_sex <- pred_sex_income
pred_sex$variable <-factor(pred_sex$variable, 
                           levels = c("75_plus", "50_75", "30_50", "15_30",
                                      "to_15"))
pred_sex$group <-factor(pred_sex$group, 
                        levels = c("Female", "Male"))

subgroups_interactions_4_he <- ggplot(pred_sex, aes(fill = group)) + 
  geom_linerange(aes(x = variable, ymin = lower,
                     ymax = upper), size = 1, position = position_dodge(width = 0.8)) + 
  geom_point(aes(x = variable, y = fit, shape = group), size = 2.3, fill = "white", 
             position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept =  seq(1.5, 13.5, 1), linetype = "dotted", colour = "gray80") +
  geom_vline(xintercept =  c(5.5, 8.5, 12.5), linetype = "dotted", colour = "gray20") +
  scale_shape_manual(values=c(23, 25, 8, 21, 19),
                     guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 13, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), vjust = 0.3),
        # specify axis text size
        axis.title = element_text(margin = unit(c(5, 0, 0, 0), "mm"), face = "bold", size = 14, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black"),
        legend.position = c(0.915, 0.959),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(labels = c("to_15" = "< 15k", "15_30" = "15k-30k", "30_50" = "30k-50k", "50_75" = "50k-75k",
                              "75_plus" = "75k +"),
                   expand = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6),
                     breaks = seq(0.1, 0.5, by = 0.1),
                     labels = c("10%", "20%", "30%", "40%", "50%")) +
  labs(y = "", x = "Income")

ggsave("./figures/subgroups_interactions_4_he.pdf", subgroups_interactions_4_he, width = 6.5, height = 8.5, dpi = 1200, device = cairo_pdf)

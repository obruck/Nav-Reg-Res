# Title: Secondary Use of Health Data in Finland
# Date: 11/2023
# Author: Oscar Brück


# Clear memory
rm(list=ls())


# Load libraries
library(tidyverse)
library(readxl)
library(reshape2)
library(sf)
library(mapsFinland)


# Create directories for results
dir.create("../results", showWarnings = FALSE, recursive = TRUE)

# Read data
df <- readxl::read_xlsx("../data/data.xlsx")


# Parameters
year_min1 = 2015                # Start of follow-up (years) for analyses 1
year_min2 = 2016                # Start of follow-up (years) for analyses 2
year_max = 2023                 # End of follow-up (years) for analyses 1 and 2
gdpr_year = 2018                # Implementation year of the GDPR
secondary_use_act_year = 2020   # Implementation year of the Secondary Use Act in Finland


################################# FINNISH MAP OF REGISTRY-BASED STUDIES ###################################################


# Map
df_rek1 <- df %>%
  dplyr::filter(ResearchType == "Registry") %>%
  dplyr::mutate(Year = factor(Year))
kunnat = kunnat2019 %>%
  dplyr::filter(nimi %in% c("Helsinki", "Turku", "Tampere", "Oulu", "Kuopio")) %>%
  dplyr::left_join(df_rek1 %>%
                     dplyr::filter(Year %in% c(2020, 2021, 2022, 2023)) %>%
                     group_by(PermitApprovedBy) %>%
                     summarise(StudyPermits = sum(StudyPermitCount)) %>%
                     dplyr::filter(!PermitApprovedBy == "Findata") %>%
                     dplyr::mutate(nimi = ifelse(PermitApprovedBy == "Helsinki", "Helsinki",
                                                 ifelse(PermitApprovedBy == "Tampere", "Tampere",
                                                        ifelse(PermitApprovedBy == "Turku", "Turku",
                                                               ifelse(PermitApprovedBy == "Kuopio", "Kuopio",
                                                                      ifelse(PermitApprovedBy == "Oulu", "Oulu", PermitApprovedBy)))))))

university_hospitals = erva2019 %>%
  st_join(kunnat %>%
            dplyr::mutate(erva_name = ifelse(nimi == "Helsinki", "HYKS erva",
                                             ifelse(nimi == "Turku", "Turku erva",
                                                    ifelse(nimi == "Tampere", "Tampere erva",
                                                           ifelse(nimi == "Oulu", "Oulu erva",
                                                                  ifelse(nimi == "Kuopio", "Kuopio erva", nimi)))))))

# Extract x and y coordinates from the geometry variable
university_hospitals$points <- st_centroid(university_hospitals$geom)  # Assuming the hospital locations are centroids
kunnat$points <- st_centroid(kunnat$geometry)  # Assuming the hospital locations are centroids


# Plot
png("../results/map.png", width = 5, height = 5, res = 300, units = "in")
ggplot() +
  geom_sf(data=university_hospitals, aes(fill = StudyPermits)) +
  geom_sf_text(data = kunnat, aes(label = nimi), color = "black", size = 4, nudge_y = 40000) +
  geom_sf(data=kunnat$points, size = 2, fill = "black") +
  theme_minimal() +
  ylab("Latitude") +
  xlab("Longitude") +
  scale_fill_gradient(low = "white", high = "red", limits = c(200, 600)) + 
  guides(fill = guide_colorbar(title = "Data Permits", title.position = "top", title.hjust = 0.5)) +
  theme(axis.text.x = element_text(size=12, colour = "black", face = "plain"),
        axis.text.y = element_text(size=12, colour = "black", face = "plain"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"))
dev.off()


################################# LINE PLOTS OF REGISTRY-BASED STUDIES ###################################################


# Subset data for registry-based studies
df_rek <- df %>%
  dplyr::mutate(StudyPermitCount = round(StudyPermitCount * NumberOfHospitalsPerPermit)) %>%
  dplyr::select(-NumberOfHospitalsPerPermit) %>%
  dplyr::filter(ResearchType == "Registry")


# Plot for Findata study counts only 2020-2023
png("../results/StudyPermitCount1_0.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df_rek %>%
         dplyr::filter(PermitApprovedBy == "FINDATA"), aes(x = Year, y=StudyPermitCount, color=PermitApprovedBy)) +
  geom_line(size = 3) +
  geom_point(size = 2, color="black") +
  xlim(gdpr_year, year_max) +
  ylim(0, 200) +
  xlab("Year") +
  ylab("Data permit count") +
  guides(color = guide_legend(title = "Data source ")) +
  theme_bw() + 
  scale_color_brewer(palette="Set1") +
  theme(text =  element_text(face = "bold"),
        axis.text.x = element_text(size=12, colour = "black", face = "plain"),
        axis.text.y = element_text(size=12, colour = "black", face = "plain"),
        axis.title = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, face="bold", colour = "black"),
        legend.position = "bottom")
dev.off()


# Plot for all registry-based study counts 2015-2023. Keep registry holders separate.
png("../results/StudyPermitCount1.png", width = 6, height = 5, res = 300, units = "in")
p_js = ggplot(df_rek %>%
                dplyr::filter(!PermitApprovedBy == "Turku") %>% # Remove Turku
                dplyr::filter(Year >= year_min1) %>%
                dplyr::mutate(PermitApprovedBy = ifelse(PermitApprovedBy == "FINDATA", "Findata",
                                                ifelse(PermitApprovedBy == "Helsinki", "Helsinki",
                                                       ifelse(PermitApprovedBy == "Tampere", "Tampere",
                                                              ifelse(PermitApprovedBy == "Kuopio", "Kuopio",
                                                                     ifelse(PermitApprovedBy == "Oulu", "Oulu", PermitApprovedBy))))),
                              PermitApprovedBy = factor(PermitApprovedBy, levels = c("Helsinki", "Kuopio", "Oulu", "Tampere", "Findata"))),
              aes(x = Year, y=StudyPermitCount, color=PermitApprovedBy)) +
  geom_line(size = 3) +
  geom_point(size = 2, color="black") +
  ylim(0, max(df_rek[df_rek$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.15) +
  xlab("Year") +
  ylab("Data permit count") +
  guides(color = guide_legend(title = "Data source ", nrow = 2)) +
  theme_bw() + 
  scale_color_brewer(palette="Set1") +
  scale_x_continuous(breaks = seq(year_min1, year_max, by = 1))
p_js + 
  theme(text =  element_text(face = "bold"),
        axis.text.x = element_text(size=12, colour = "black", face = "plain"),
        axis.text.y = element_text(size=12, colour = "black", face = "plain"),
        axis.title = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, face="bold", colour = "black"),
        legend.position = "bottom") +
  geom_segment(aes(x = gdpr_year,
                   y = max(df_rek[df_rek$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.1,
                   xend = gdpr_year,
                   yend = max(df_rek[df_rek$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.05),
               arrow = arrow(length = unit(0.2, "inches"), type = "closed"), size = 1.5, color = "black") +
  geom_segment(aes(x = secondary_use_act_year,
                   y = max(df_rek[df_rek$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.1,
                   xend = secondary_use_act_year,
                   yend = max(df_rek[df_rek$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.05),
               arrow = arrow(length = unit(0.2, "inches"), type = "closed"), size = 1.5, color = "black") +
  annotate("label", x=gdpr_year, y=max(df_rek[df_rek$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.13, label= "GDPR", hjust=0.5, vjust=0) +
  annotate("label", x=secondary_use_act_year, y=max(df_rek[df_rek$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.13, label= "Secondary Use Act", hjust=0.5, vjust=0)
dev.off()


# Plot for all registry-based study counts 2016-2023. Pool university hospital registry holders.
## Subset data
tmp1 = df_rek %>%
  dplyr::filter(!PermitApprovedBy == "Turku") %>%
  dplyr::mutate(PermitApprovedBy = ifelse(PermitApprovedBy == "FINDATA", "Findata", PermitApprovedBy),
                group = ifelse(PermitApprovedBy=="Findata", "Findata", "Hospitals")) %>%
  group_by(Year, group) %>%
  summarise(StudyPermitCount_med = median(StudyPermitCount, na.rm = TRUE),
            StudyPermitCount25 = quantile(StudyPermitCount, 0.25, na.rm = TRUE),
            StudyPermitCount75 = quantile(StudyPermitCount, 0.75, na.rm = TRUE))
df_rek[df_rek$Year=="2016",]
## Plot
png("../results/StudyPermitCount2.png", width = 6, height = 5, res = 300, units = "in")
ggplot(
  data = tmp1 %>%
    dplyr::filter(Year >= year_min2),
  aes(x = Year, y=StudyPermitCount_med)) +
  geom_ribbon(data = tmp1 %>%
                dplyr::filter(Year >= year_min2 & group == "Hospitals"),
    aes(ymin = StudyPermitCount25, ymax = StudyPermitCount75, group=group),
    fill = "grey", alpha = 0.3, color="black",
    position = position_dodge(0.2)
  ) +
  geom_line(aes(color=group), size = 2) +
  geom_point(size = 3, color="black") +
  ylim(0, max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount_med, na.rm = TRUE)*1.15) +
  xlab("Year") +
  ylab("Data permit count") +
  scale_color_manual(values = c("#ff7f00", "black")) +
  guides(color = guide_legend(title = "Data source ")) +
  theme_bw() + 
  scale_x_continuous(breaks = seq(year_min2, year_max, by = 1)) + 
  theme(text =  element_text(face = "bold"),
        axis.text.x = element_text(size=12, colour = "black", face = "plain"),
        axis.text.y = element_text(size=12, colour = "black", face = "plain"),
        axis.title = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, face="bold", colour = "black"),
        legend.position = "bottom") +
  geom_segment(aes(x = gdpr_year,
                   y = max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount_med, na.rm = TRUE)*1.1,
                   xend = gdpr_year,
                   yend = max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount_med, na.rm = TRUE)*1.05),
               arrow = arrow(length = unit(0.2, "inches"), type = "closed"), size = 1.5, color = "black") +
  geom_segment(aes(x = secondary_use_act_year,
                   y = max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount_med, na.rm = TRUE)*1.1,
                   xend = secondary_use_act_year,
                   yend = max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount_med, na.rm = TRUE)*1.05),
               arrow = arrow(length = unit(0.2, "inches"), type = "closed"), size = 1.5, color = "black") +
  annotate("label", x=gdpr_year, y=max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount_med, na.rm = TRUE)*1.13, label= "GDPR", hjust=0.5, vjust=0) +
  annotate("label", x=secondary_use_act_year, y=max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount_med, na.rm = TRUE)*1.13, label= "Secondary Use Act", hjust=0.5, vjust=0)
dev.off()


# Save for later
p_js2 = ggplot(df_rek %>%
                dplyr::filter(Year >= year_min1) %>%
                 dplyr::filter(!PermitApprovedBy %in% c("FINDATA", "Turku")) %>%
                dplyr::mutate(PermitApprovedBy = ifelse(PermitApprovedBy == "FINDATA", "Findata",
                                                ifelse(PermitApprovedBy == "Helsinki", "Helsinki",
                                                       ifelse(PermitApprovedBy == "Tampere", "Tampere",
                                                              ifelse(PermitApprovedBy == "Turku", "Turku",
                                                                     ifelse(PermitApprovedBy == "Kuopio", "Kuopio",
                                                                            ifelse(PermitApprovedBy == "Oulu", "Oulu", PermitApprovedBy))))))), aes(x = Year, y=StudyPermitCount, color=PermitApprovedBy)) +
  geom_line(size = 3) +
  geom_point(size = 2, color="black") +
  ylim(0, max(df_rek[df_rek$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.15) +
  xlab("Year") +
  ylab("Data permit count") +
  guides(color = guide_legend(title = "Data source ")) +
  theme_bw() + 
  scale_color_brewer(palette="Set1") +
  scale_x_continuous(breaks = seq(year_min1, year_max, by = 1))


################################# LINEAR REGRESSION OF REGISTRY-BASED STUDIES ###################################################



# Add temp rows for Helsinki
for (i in year_min1:2015) {
  df_rek = rbind(df_rek, data.frame(Year = i, StudyPermitCount = df_rek[df_rek$Year==2016 & df_rek$PermitApprovedBy=="Helsinki",]$StudyPermitCount, ResearchType = "Registry", PermitApprovedBy = "Helsinki"))
}


# Extrapolate
## Prepare data frame
pred_js <- data.frame(Year=rep(year_min1:year_max, 4),  # 4 hospitals
                      Hakija=rep("Pred", (year_max-year_min1+1)*4),  # 4 hospitals
                      PermitApprovedBy=c(rep("Helsinki", (year_max-year_min1+1)), rep("Tampere", (year_max-year_min1+1)),
                                 rep("Oulu", (year_max-year_min1+1)), rep("Kuopio", (year_max-year_min1+1))))
## Predicted values for observed data (ignore Turku and Findata)
pred_counts = NULL
df_rek1 = df_rek %>%
  dplyr::filter(!PermitApprovedBy %in% c("FINDATA", "Turku"))
for (i in unique(df_rek[!df_rek$PermitApprovedBy %in% c("Turku", "FINDATA"),]$PermitApprovedBy)) {
  df_rek2 = df_rek1 %>%
    dplyr::filter(Year >= year_min1)
  if (i %in% c("Helsinki")) {
    df_rek2 = df_rek1 %>%
      dplyr::filter(Year >= year_min2)
  }
  pred_counts1 = df_rek2[df_rek2$PermitApprovedBy == i,]
  pred_counts1$extrapolated_preds <- predict(lm(StudyPermitCount ~ Year, data=df_rek2[df_rek2$PermitApprovedBy == i & df_rek2$Year<secondary_use_act_year,]), newdata=pred_counts1)
  pred_counts = rbind(pred_counts, pred_counts1)
}



# Calculate the distance between the regression curve and the dashed line at year 2023
pred_counts$distance = abs(as.integer(pred_counts$StudyPermitCount) - as.integer(pred_counts$extrapolated_preds))
pred_counts$percentage = round(100*pred_counts$distance/pred_counts$extrapolated_preds, 0)
pred_counts = pred_counts %>%
  dplyr::filter(!PermitApprovedBy == "Turku")


# Sum in 2023
## Predicted study permit counts in 2023  (Helsinki + Tampere + Kuopio + Oulu) 
pred_study_counts_2023 = pred_counts %>% 
  dplyr::filter(Year == 2023) %>%
  summarise(extrapolated_preds = sum(extrapolated_preds))
## Actual study permit counts in 2023  (Helsinki + Tampere + Kuopio + Oulu) 
study_counts_2023 = pred_counts %>% 
  dplyr::filter(Year == 2023) %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))
## Actual study permit counts in 2023  (Findata) 
findata_study_counts_2023 = df_rek %>% 
  dplyr::filter(Year == 2023 & PermitApprovedBy == "FINDATA") %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))
## Proportion of Turku
### Study counts in 2023 (Helsinki + Tampere + Kuopio + Oulu + Turku)
counts_in_2023_with_turku = df %>%
  dplyr::filter(Year == 2023 & ResearchType == "Registry" & PermitApprovedBy != "FINDATA") %>%
  ungroup() %>%
  group_by(PermitApprovedBy) %>%
  summarise(StudyPermitCount = sum(StudyPermitCount)); counts_in_2023_with_turku
### Proportion of Turku
counts_in_2023_only_turku = df %>% dplyr::filter(Year == 2023 & ResearchType == "Registry" & PermitApprovedBy == "Turku") %>%
  ungroup() %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))
turku_prop = counts_in_2023_only_turku$StudyPermitCount / sum(counts_in_2023_with_turku$StudyPermitCount)

## Estimated reduction in study permit counts (absolute)
abs_reduction = pred_study_counts_2023$extrapolated_preds - study_counts_2023$StudyPermitCount - (findata_study_counts_2023 * (1-turku_prop))
## Estimated reduction in study permit counts (relative)
100 * abs_reduction / pred_study_counts_2023$extrapolated_preds




# Plot
png("../results/StudyPermitCount3.png", width = 10, height = 6, res = 300, units = "in")
p_js2 +
  geom_line(data = pred_counts %>%
              dplyr::filter(Year >= year_min1) %>%
              dplyr::filter(!PermitApprovedBy %in% c("FINDATA", "Turku")) %>%
              dplyr::mutate(PermitApprovedBy = 
                              ifelse(PermitApprovedBy == "Helsinki", "Helsinki",
                                     ifelse(PermitApprovedBy == "Tampere", "Tampere",
                                            # ifelse(PermitApprovedBy == "Turku", "Turku",
                                            ifelse(PermitApprovedBy == "Kuopio", "Kuopio",
                                                   ifelse(PermitApprovedBy == "Oulu", "Oulu", PermitApprovedBy))))),
            aes(x = Year, y = extrapolated_preds), color = "black", size = 1.5, linetype = "dashed") +
  geom_segment(aes(x = gdpr_year,
                   y = max(df_rek2[df_rek2$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.1,
                   xend = gdpr_year,
                   yend = max(df_rek2[df_rek2$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.05),
               arrow = arrow(length = unit(0.2, "inches"), type = "closed"), size = 1.5, color = "black") +
  geom_segment(aes(x = secondary_use_act_year,
                   y = max(df_rek2[df_rek2$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.1,
                   xend = secondary_use_act_year,
                   yend = max(df_rek2[df_rek2$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.05),
               arrow = arrow(length = unit(0.2, "inches"), type = "closed"), size = 1.5, color = "black") +
  annotate("label", x=gdpr_year, y=max(df_rek2[df_rek2$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.2, label= "GDPR", hjust=0.75, vjust=0) +
  annotate("label", x=secondary_use_act_year, y=max(df_rek2[df_rek2$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.2, label= "Secondary Use Act", hjust=0.25, vjust=0) +
  xlab("Year") +
  ylab("Data permit count") +
  guides(color = guide_legend(title = "Data source ")) +
  geom_line(size = 3) +
  geom_point(size = 2, color="black") +
  ylim(0, max(pred_counts[pred_counts$Year >= year_min1,]$StudyPermitCount, na.rm = TRUE)*1.25) +
  theme_bw() +
  scale_color_brewer(palette="Set1") +
  scale_x_continuous(breaks = seq(year_min1, year_max, by = 1)) +
  facet_wrap(facets = "PermitApprovedBy", scales = "free_y") + 
  theme(text = element_text(face = "bold"),
        axis.text.x = element_text(size=10, colour = "black", face="plain"),
        axis.text.y = element_text(size=12, colour = "black", face="plain"),
        axis.title=element_text(size=14, face="bold", colour = "black"),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        legend.position = "none")
dev.off()


################################# LINE PLOTS OF OTHER THAN REGISTRY-BASED STUDIES ###################################################


# Subset data for other than registry-based studies
df_nonrek <- df %>%
  dplyr::filter(ResearchType == "NonRegistry") %>%
  dplyr::filter(!PermitApprovedBy == "Turku") # Remove Turku
tmp1 = df_nonrek %>%
  dplyr::filter(Year >= year_min2) %>%
  group_by(Year) %>%
  summarise(StudyPermitCount = sum(StudyPermitCount, na.rm = TRUE))

# Plot
png("../results/muut_StudyPermitCount.png", width = 6, height = 5, res = 300, units = "in")
p_js_rek = ggplot(df_nonrek %>%
                    dplyr::filter(Year >= year_min2) %>%
                    dplyr::mutate(PermitApprovedBy = ifelse(PermitApprovedBy == "Helsinki", "Helsinki",
                                                    ifelse(PermitApprovedBy == "Tampere", "Tampere",
                                                           ifelse(PermitApprovedBy == "Turku", "Turku",
                                                                  ifelse(PermitApprovedBy == "Kuopio", "Kuopio",
                                                                         ifelse(PermitApprovedBy == "Oulu", "Oulu", PermitApprovedBy)))))), aes(x = Year, y=StudyPermitCount, color=PermitApprovedBy)) +
  geom_bar(data = tmp1,
           stat="identity", color="black", fill="grey") +
  geom_segment(data = tmp1,
               aes(x = gdpr_year,
                   y = max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount, na.rm = TRUE)*1.08,
                   xend = gdpr_year,
                   yend = max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount, na.rm = TRUE)*1.02),
               arrow = arrow(length = unit(0.2, "inches"), type = "closed"), size = 1.5, color = "black") +
  geom_segment(data = tmp1,
               aes(x = secondary_use_act_year,
                   y = max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount, na.rm = TRUE)*1.08,
                   xend = secondary_use_act_year,
                   yend = max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount, na.rm = TRUE)*1.02),
               arrow = arrow(length = unit(0.2, "inches"), type = "closed"), size = 1.5, color = "black") +
  annotate("label", x=gdpr_year, y=max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount, na.rm = TRUE)*1.095, label= "GDPR", hjust=0.5, vjust=0) +
  annotate("label", x=secondary_use_act_year, y=max(tmp1[tmp1$Year >= year_min2,]$StudyPermitCount, na.rm = TRUE)*1.095, label= "Secondary Use Act", hjust=0.5, vjust=0) +
  geom_line(size = 3) +
  geom_point(size = 2, color="black") +
  ylim(0, sum(df_nonrek[df_nonrek$Year == 2017,]$StudyPermitCount, na.rm = TRUE)*1.12) +
  # scale_y_continuous(name = "Study permit count",
  #                    sec.axis = sec_axis(~., name = "Study permit count (cumulative)")) +
  xlab("Year") +
  ylab("Study permit count") +
  guides(color = guide_legend(title = "Data source", nrow = 2)) +
  theme_bw() + 
  scale_color_brewer(palette="Set1") +
  theme(text =  element_text(face = "bold"),
        axis.text.x = element_text(size=12, colour = "black", face = "plain"),
        axis.text.y = element_text(size=12, colour = "black", face = "plain"),
        axis.title = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, face="bold", colour = "black"),
        legend.position = "bottom") +
  scale_x_continuous(breaks = seq(year_min2, year_max, by = 1))
p_js_rek
dev.off()


# Number of new non-registry research study permits per year
df_nonrek %>%
  group_by(Year) %>%
  summarise(StudyPermitCount = sum(StudyPermitCount, na.rm = TRUE))


############################### STATISTICS ############################################################################


# Study counts at various years

## Study counts in 2016-2017 (Helsinki + Tampere + Kuopio + Oulu), Median
df %>%
  dplyr::filter(Year %in% c(2016:2017) & ResearchType == "Registry" & PermitApprovedBy != "FINDATA") %>%
  ungroup() %>%
  summarise(StudyPermitCount = median(StudyPermitCount))

## Study counts in 2018-2019 (Helsinki + Tampere + Kuopio + Oulu), Median
df %>%
  dplyr::filter(Year %in% c(2018:2019) & ResearchType == "Registry" & PermitApprovedBy != "FINDATA") %>%
  ungroup() %>%
  summarise(StudyPermitCount = median(StudyPermitCount))

## Study counts in 2018 (Helsinki + Tampere + Kuopio + Oulu), Median
df %>%
  dplyr::filter(Year == 2018 & ResearchType == "Registry" & PermitApprovedBy != "FINDATA") %>%
  ungroup() %>%
  summarise(StudyPermitCount = median(StudyPermitCount))

## Study counts in 2019 (Helsinki + Tampere + Kuopio + Oulu), Median
df %>%
  dplyr::filter(Year == 2019 & ResearchType == "Registry" & PermitApprovedBy != "FINDATA") %>%
  ungroup() %>%
  summarise(StudyPermitCount = median(StudyPermitCount))

## Study counts in 2019 (Helsinki + Tampere + Kuopio + Oulu), Max
df %>%
  dplyr::filter(Year == 2019 & ResearchType == "Registry" & PermitApprovedBy != "FINDATA") %>%
  ungroup() %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))

## Study counts after 2020 (Helsinki + Tampere + Kuopio + Oulu + Turku), Sum
counts_since_2020 = df %>%
  dplyr::filter(Year > 2019 & ResearchType == "Registry" & PermitApprovedBy != "FINDATA") %>%
  ungroup() %>%
  group_by(PermitApprovedBy) %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))

## Study permit count in 2023, Sum
df %>%
  dplyr::filter(Year == 2023 & ResearchType == "Registry" & PermitApprovedBy != "FINDATA") %>%
  ungroup() %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))

## Study counts in 2023 (Helsinki + Tampere + Kuopio + Oulu), Sum
df %>%
  dplyr::filter(Year == 2019 & ResearchType == "Registry" & !PermitApprovedBy %in% c("FINDATA", "Turku")) %>%
  ungroup() %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))





# Linearity in study counts with the Kendall Rank test

## Non-registry-based research
### 2016-2023
df_nonrek_sr = df_nonrek %>%
  dplyr::filter(Year >= year_min2 & Year <= year_max) %>%
  dplyr::group_by(Year) %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))
cor.test(df_nonrek_sr$Year, df_nonrek_sr$StudyPermitCount, method = "kendall", alternative = "two.sided")
### Separately for each hospital
for (i in unique(df_nonrek$PermitApprovedBy)) {
  df_nonrek_sr = df_nonrek %>%
    dplyr::filter(Year >= year_min2 & Year <= year_max & PermitApprovedBy == !!i) %>%
    dplyr::group_by(Year) %>%
    summarise(StudyPermitCount = sum(StudyPermitCount))
  print(i)
  print(cor.test(df_nonrek_sr$Year, df_nonrek_sr$StudyPermitCount, method = "kendall", alternative = "two.sided"))
}
## Registry-based research
### 2016-2019
df_rek_sr = df_rek %>%
  dplyr::filter(Year >= year_min2 & Year < secondary_use_act_year) %>%
  dplyr::group_by(Year) %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))
cor.test(df_rek_sr$Year, df_rek_sr$StudyPermitCount, method = "kendall", alternative = "two.sided")
df_rek_sr %>%
  summarise(
    StudyPermitCount_med = median(StudyPermitCount),
    StudyPermitCount_min = min(StudyPermitCount),
    StudyPermitCount_max = max(StudyPermitCount))
# StudyPermitCount_min = quantile(StudyPermitCount, probs = 0.25, na.rm = TRUE),
# StudyPermitCount_max = quantile(StudyPermitCount, probs = 0.75, na.rm = TRUE))
### 2020-2023
df_rek_sr = df_rek %>%
  dplyr::filter(Year >= secondary_use_act_year & Year <= year_max) %>%
  dplyr::group_by(Year) %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))
cor.test(df_rek_sr$Year, df_rek_sr$StudyPermitCount, method = "kendall", alternative = "less")
## Registry-based research (FINDATA only)
### 2020-2023
df_rek_sr = df_rek %>%
  dplyr::filter(Year >= secondary_use_act_year & Year <= year_max & PermitApprovedBy == "FINDATA") %>%
  dplyr::group_by(Year) %>%
  summarise(StudyPermitCount = sum(StudyPermitCount))
cor.test(df_rek_sr$Year, df_rek_sr$StudyPermitCount, method = "kendall", alternative = "greater")
sum(df_rek_sr$StudyPermitCount)
lm(StudyPermitCount ~ Year, data = df_rek_sr)
19.9/52


# Linear regression 2020-2023 + calculate spline

## Prepare dataframe
mod_results = data.frame("Registry" = NA,
                         "Intercept" = NA,
                         "Slope_absolute" = NA,
                         "Slope_relative" = NA)
## Loop
# for (i in unique(df_rek[!df_rek$PermitApprovedBy=="Turku",]$PermitApprovedBy)) {
for (i in unique(df_rek[!df_rek$PermitApprovedBy=="FINDATA",]$PermitApprovedBy)) {
# for (i in unique(df_rek[df_rek$PermitApprovedBy=="FINDATA",]$PermitApprovedBy)) {
# for (i in unique(df_rek$PermitApprovedBy)) {
  # print(shapiro.test(df_rek[df_rek$PermitApprovedBy==i & df_rek$Year>=secondary_use_act_year,]$StudyPermitCount))
  mod = lm(StudyPermitCount ~ Year, data = df_rek[df_rek$PermitApprovedBy==i & df_rek$Year>=secondary_use_act_year,])
  mod_results = rbind(mod_results, data.frame("Registry" = i,
                                              "Intercept" = coef(mod)[1],
                                              "Slope_absolute" = round(coef(mod)[2], 2),
                                              "Slope_relative" = round(100 * coef(mod)[2] / sum(df_rek[df_rek$PermitApprovedBy==i & df_rek$Year==secondary_use_act_year,]$StudyPermitCount, na.rm = TRUE) / (max(df_rek$Year, na.rm=TRUE) - secondary_use_act_year), 2)
  ))
}
mod_results
mod_results = mod_results %>%
  dplyr::filter(!is.na(Registry))
row.names(mod_results) = NULL
## Median spline (absolute values)
median(mod_results$Slope_absolute)
## Min spline (absolute values)
mod_results %>% arrange(Slope_absolute) %>% slice(1)
## Max spline (absolute values)
mod_results %>% arrange(desc(Slope_absolute)) %>% slice(1)
## Median spline (relative values)
median(mod_results$Slope_relative)
## Min spline (relative values)
mod_results %>% arrange(Slope_relative) %>% slice(1)
## Max spline (relative values)
mod_results %>% arrange(desc(Slope_relative)) %>% slice(1)





# Linear regression 2015-2019 + calculate spline

## Prepare dataframe
mod_results1 = data.frame("Registry" = NA,
                         "Intercept" = NA,
                         "Slope_absolute" = NA,
                         "Slope_relative" = NA)
## Loop
for (i in unique(df_rek[!df_rek$PermitApprovedBy %in% c("Turku", "FINDATA"),]$PermitApprovedBy)) {
  mod = lm(StudyPermitCount ~ Year, data = df_rek[df_rek$PermitApprovedBy==i & df_rek$Year<secondary_use_act_year & df_rek$Year>=year_min1,])
  mod_results1 = rbind(mod_results1, data.frame("Registry" = i,
                                              "Intercept" = coef(mod)[1],
                                              "Slope_absolute" = round(coef(mod)[2], 2),
                                              # "Slope_relative" = round(100 * coef(mod)[2] / sum(df_rek[df_rek$PermitApprovedBy==i & df_rek$Year<secondary_use_act_year & df_rek$Year>=year_min1,]$StudyPermitCount, na.rm = TRUE), 2)
                                              "Slope_relative" = round(100 * coef(mod)[2] / sum(df_rek[df_rek$PermitApprovedBy==i & df_rek$Year==year_min1,]$StudyPermitCount, na.rm = TRUE) / ((secondary_use_act_year-1) - year_min1), 2)
  ))
}
mod_results1 = mod_results1 %>%
  dplyr::filter(!is.na(Registry))
row.names(mod_results1) = NULL
mod_results1
## Median spline (absolute values)
median(mod_results1$Slope_absolute)
## Min spline (absolute values)
mod_results1 %>% arrange(Slope_absolute) %>% slice(1)
## Max spline (absolute values)
mod_results1 %>% arrange(desc(Slope_absolute)) %>% slice(1)
## Median spline (relative values)
median(mod_results1$Slope_relative)
## Min spline (absolute values)
mod_results1 %>% arrange(Slope_relative) %>% slice(1)
## Max spline (absolute values)
mod_results1 %>% arrange(desc(Slope_relative)) %>% slice(1)

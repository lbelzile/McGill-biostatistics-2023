library(tidyverse)
setwd("/home/lbelzile/Documents/Dropbox/Rpackage/longevity/inst/rawdata")
idl23 <- readxl::read_xlsx("idl_complete_2023-02.xlsx")
idl_sub <- idl23 |>
  filter(DEATH_COUNTRY %in% c("FRA","EAW","USA","JPN")) |>
  group_by(DEATH_YEAR) |>
  summarize(MRAD = sort(AGEDAYS/365.25, decreasing = TRUE)[1:5]) |>
  mutate(rank = factor(1:5))

idl_sub2 <- idl23 |>
  filter(DEATH_COUNTRY %in% c("FRA","EAW","USA","JPN")) |>
  filter(AGEYEARS >= 110) |>
  group_by(DEATH_YEAR) |>
  summarize(average = mean(AGEDAYS)/365.25,
            nobs = n())

g1 <- ggplot(data = idl_sub |> filter(rank == "1"),
             mapping = aes(x = DEATH_YEAR,
                           y = MRAD)) +
  geom_point() +
  labs(x = "year",
       subtitle = "yearly maximum reported age at death",
       y = "") +
  theme_classic()

g2 <- ggplot(data = idl_sub,
             mapping = aes(x = DEATH_YEAR,
                           y = MRAD,
                           color = rank,
                           group = rank,
                           fill = rank)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ splines::bs(x, df = 5),
              alpha = 0.5) +
  labs(x = "year",
       subtitle = "largest five yearly maximum reported age at death",
       y = "",
       color = "",
       fill = "") +
  theme_classic() +
  theme(legend.position = "none")


# Load 2021 IDL data
load("/home/lbelzile/Documents/Dropbox/Rpackage/longevity/data/idl.rda")


supercent <- idl |> filter(ageyear >= 110)
agerange <- 1980:2017
meansup <- nobs <- vector("numeric", length = length(agerange))
for(i in seq_along(agerange)){
  survf <- with(supercent |> filter(year(ddate) == agerange[i]),
                longevity::np_elife(time = ndays, ltrunc = ifelse(is.na(ltrunc2), ltrunc1, ltrunc2), rtrunc = ifelse(is.na(ltrunc2), rtrunc1, rtrunc2)))
  meansup[i] <- summary(survf$survfun)['Mean']
  nobs[i] <- nrow(supercent |> filter(year(ddate) == agerange[i]))
}


g3 <- ggplot(data = idl_sub2,
             mapping = aes(x = DEATH_YEAR,
                           y = average,
                           weight = sqrt(nobs))) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ splines::bs(x, df = 5),
              alpha = 0.5,
              se = FALSE,
              color = "black") +
  geom_point(data = data.frame(x = agerange,
                               y = meansup/365.25),
             mapping = aes(x = x, y = y),
             shape = 3) +
  geom_smooth(data = data.frame(x = agerange,
                               y = meansup/365.25,
                               w = sqrt(nobs)),
             mapping = aes(x = x, y = y, weight = w),
             linetype = 2,
             method = "lm",
             formula = y ~ splines::bs(x, df = 5),
             alpha = 0.5,
             se = FALSE,
             color = "black") +
  labs(x = "year",
       subtitle = "average reported age at death (110+ years)",
       y = "") +
  theme_classic()


library(patchwork)

g1 + g2 + g3


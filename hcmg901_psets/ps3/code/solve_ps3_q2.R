################################################################################
# AUTHORS: Pedro H. C. Sant’Anna and Brantly Callaway (light edits by Andres Rovira)
# CREATED: 2023-03-19 copied from source: https://psantanna.com/posts/twfe
# PURPOSE: Solve 2nd part of HCMG 901 Problem Set 3
################################################################################


# Load libraries and set baseline parameters
library(tidyverse)
library(lfe)
library(fastDummies)
library(ggthemes)
library(did)
library(fs)
theme_set(theme_clean() + theme(plot.background = element_blank()))

setwd(path_expand("~/Dropbox (Penn)/Classes/2_health_applied_metrics/ps3/code"))
tex_path <- path_expand("~/Dropbox (Penn)/Apps/Overleaf/hcmg901_ps3")

iseed = 20201221
nrep <- 100  
true_mu <- 1
set.seed(iseed)
colors <- c("True Effect" = "red", "Estimated Effect" = "blue")


################################################################################
# 2.1
################################################################################

## Generate data - treated cohorts consist of 250 obs each, with the treatment effect still = true_mu on average
make_data3 <- function(nobs = 1000, 
                       nstates = 40) {
  
  # unit fixed effects (unobservd heterogeneity)
  unit <- tibble(
    unit = 1:nobs,
    # generate state
    state = sample(1:nstates, nobs, replace = TRUE),
    unit_fe = rnorm(nobs, state/5, 1),
    # generate instantaneous treatment effect
    #mu = rnorm(nobs, true_mu, 0.2)
    mu = true_mu
  )
  
  # year fixed effects (first part)
  year <- tibble(
    year = 1980:2010,
    year_fe = rnorm(length(year), 0, 1)
  )
  
  # Put the states into treatment groups
  treat_taus <- tibble(
    # sample the states randomly
    state = sample(1:nstates, nstates, replace = FALSE),
    # place the randomly sampled states into 1\{t \ge g \}G_g
    cohort_year = sort(rep(c(1986, 1992, 1998, 2004), 10))
  )
  
  # make main dataset
  # full interaction of unit X year 
  expand_grid(unit = 1:nobs, year = 1980:2010) %>% 
    left_join(., unit) %>% 
    left_join(., year) %>% 
    left_join(., treat_taus) %>% 
    # make error term and get treatment indicators and treatment effects
    # Also get cohort specific trends (modify time FE)
    mutate(error = rnorm(nobs*31, 0, 1),
           treat = ifelse((year >= cohort_year)* (cohort_year != 2004), 1, 0),
           mu = ifelse(cohort_year==1992, 2, ifelse(cohort_year==1998, 1, 3)),
           tau = ifelse(treat == 1, mu, 0),
           year_fe = year_fe + 0.1*(year - cohort_year)
    ) %>% 
    # calculate cumulative treatment effects
    group_by(unit) %>% 
    mutate(tau_cum = cumsum(tau)) %>% 
    ungroup() %>% 
    # calculate the dep variable
    mutate(dep_var = (2010 - cohort_year) + unit_fe + year_fe + tau_cum + error) %>%
    # Relabel 2004 cohort as never-treated
    mutate(cohort_year = ifelse(cohort_year == 2004, Inf, cohort_year))
  
}
#----------------------------------------------------------------------------
# make data
data <- make_data3()

# plot
plot3 <- data %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) + 
  geom_line(alpha = 1/8, color = "grey") + 
  geom_line(data = data %>% 
              group_by(cohort_year, year) %>% 
              summarize(dep_var = mean(dep_var)),
            aes(x = year, y = dep_var, group = factor(cohort_year),
                color = factor(cohort_year)),
            size = 2) + 
  labs(x = "", y = "Value",  color = "Treatment group   ") + 
  geom_vline(xintercept = 1986, color = '#E41A1C', size = 2) + 
  geom_vline(xintercept = 1992, color = '#377EB8', size = 2) + 
  geom_vline(xintercept = 1998, color = '#4DAF4A', size = 2) + 
  #geom_vline(xintercept = 2004, color = '#984EA3', size = 2) + 
  scale_color_brewer(palette = 'Set1') + 
  theme(legend.position = 'bottom',
        #legend.title = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("1986", "1992", "1998", "Never-treated"),
                     values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
  ggtitle("One draw of the DGP with heterogeneous treatment effect dynamics across cohorts \n and with a never-treated group")+
  theme(plot.title = element_text(hjust = 0.5, size=12))

plot3
ggsave("fig_2-1.png", path = tex_path)




################################################################################
# 2.2
################################################################################

# function to run ES DID
run_ES_DiD_sat_never_het <- function(...) {
  
  # resimulate the data
  data <- make_data3()
  
  # make dummy columns
  data <- data %>% 
    # make relative year indicator
    mutate(rel_year = year - cohort_year)
  
  # get the minimum relative year - we need this to reindex
  min_year <- min(data$rel_year * (data$rel_year != -Inf), na.rm = T)
  
  # reindex the relative years
  data <- data %>% 
    mutate(rel_year2 = rel_year) %>% 
    mutate(rel_year = rel_year - min_year) %>% 
    dummy_cols(select_columns = "rel_year") %>% 
    select(-("rel_year_-Inf"))
  
  
  # make regression formula 
  indics <- paste("rel_year", (1:max(data$rel_year))[-(-1 - min_year)], sep = "_", collapse = " + ")
  keepvars <- paste("rel_year", c(-5:-2, 0:5) - min_year, sep = "_")  
  formula <- as.formula(paste("dep_var ~", indics, "| unit + year | 0 | state"))
  
  # run mod
  mod <- felm(formula, data = data, exactDOF = TRUE)
  
  # grab the obs we need
  # grab the obs we need
  mod2 <- tibble(
    estimate = mod$coefficients,
    term1 = rownames(mod$coefficients)
  )
  
  es <-
    mod2 %>% 
    filter(term1 %in% keepvars) %>% 
    mutate(t = c(-5:-2, 0:5)) %>% 
    select(t, estimate)
  es
}

data_sat_never_het <- map_dfr(1:nrep, run_ES_DiD_sat_never_het)

ES_plot_sat_never_het <- data_sat_never_het %>% 
  group_by(t) %>% 
  summarize(avg = mean(estimate),
            sd = sd(estimate),
            lower.ci = avg - 1.96*sd,
            upper.ci = avg + 1.96*sd) %>% 
  bind_rows(tibble(t = -1, avg = 0, sd = 0, lower.ci = 0, upper.ci = 0)) %>% 
  mutate(true_tau = ifelse(t >= 0, (t + 1)* 2, 0)) %>% 
  ggplot(aes(x = t, y = avg)) + 
  #geom_linerange(aes(ymin = lower.ci, ymax = upper.ci), color = 'darkgrey', size = 2) + 
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), color = "lightgrey", alpha = 0.2) +
  geom_point(color = 'blue', size = 3) + 
  geom_line(aes(color = 'Estimated Effect'), size = 1) + 
  geom_line(aes(x = t, y = true_tau, color = 'True Effect'), linetype = "dashed", size = 2) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Relative Time", y = "Estimate") + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))+
  ggtitle("TWFE event-study regression with 'all' leads and lags")+
  scale_color_manual(values = colors) + 
  theme(plot.title = element_text(hjust = 0.5, size=12),
        legend.position = "bottom", 
        legend.title = element_blank())

ES_plot_sat_never_het
ggsave("fig_2-2.png", path = tex_path)



################################################################################
# 2.3
################################################################################
# function to run ES DID
run_CS_never_het <- function(...) {
  
  # resimulate the data
  data <- make_data3()
  data$cohort_year[data$cohort_year==Inf] <- 0
  
  mod <- did::att_gt(yname = "dep_var", 
                     tname = "year",
                     idname = "unit",
                     gname = "cohort_year",
                     control_group= "nevertreated", # AR changed this from "never_treated"
                     bstrap = FALSE,
                     data = data,
                     print_details = FALSE)
  event_std <- did::aggte(mod, type = "dynamic")
  
  att.egt <- event_std$att.egt
  names(att.egt) <- event_std$egt
  
  # grab the obs we need
  broom::tidy(att.egt) %>% 
    filter(names %in% -5:5) %>% 
    mutate(t = -5:5, estimate = x) %>% 
    select(t, estimate)
}

data_CS_never_het <- map_dfr(1:nrep, run_CS_never_het)

ES_plot_CS_never_het <- data_CS_never_het %>% 
  group_by(t) %>% 
  summarize(avg = mean(estimate),
            sd = sd(estimate),
            lower.ci = avg - 1.96*sd,
            upper.ci = avg + 1.96*sd) %>% 
  mutate(true_tau = ifelse(t >= 0, (t + 1)* 2, 0)) %>% 
  ggplot(aes(x = t, y = avg)) + 
  #geom_linerange(aes(ymin = lower.ci, ymax = upper.ci), color = 'darkgrey', size = 2) + 
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), color = "lightgrey", alpha = 0.2) +
  geom_point(color = 'blue', size = 3) + 
  geom_line(aes(color = 'Estimated Effect'), size = 1) + 
  geom_line(aes(x = t, y = true_tau, color = 'True Effect'), linetype = "dashed", size = 2) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Relative Time", y = "Estimate") + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))+
  ggtitle("Event-study-parameters estimated using Callaway and Sant'Anna (2020)\nComparison group: Never-treated units")+
  scale_color_manual(values = colors) + 
  theme(plot.title = element_text(hjust = 0.5, size=12),
        legend.position = "bottom", 
        legend.title = element_blank())

ES_plot_CS_never_het
ggsave("fig_2-3.png", path = tex_path)



################################################################################
# 2.4
################################################################################


## Generate data - treated cohorts consist of 250 obs each, with the treatment effect still = true_mu on average
make_data4 <- function(nobs = 1000, 
                       nstates = 40) {
  
  # unit fixed effects (unobservd heterogeneity)
  unit <- tibble(
    unit = 1:nobs,
    # generate state
    state = sample(1:nstates, nobs, replace = TRUE),
    unit_fe = rnorm(nobs, state/5, 1),
    # generate instantaneous treatment effect
    #mu = rnorm(nobs, true_mu, 0.2)
    mu = true_mu
  )
  
  # year fixed effects (first part)
  year <- tibble(
    year = 1980:2010,
    year_fe = rnorm(length(year), 0, 1)
  )
  
  # Put the states into treatment groups
  treat_taus <- tibble(
    # sample the states randomly
    state = sample(1:nstates, nstates, replace = FALSE),
    # place the randomly sampled states into 1\{t \ge g \}G_g
    cohort_year = sort(rep(c(1986, 1992, 1998, 2004), 10))
  )
  
  # make main dataset
  # full interaction of unit X year 
  expand_grid(unit = 1:nobs, year = 1980:2010) %>% 
    left_join(., unit) %>% 
    left_join(., year) %>% 
    left_join(., treat_taus) %>% 
    # make error term and get treatment indicators and treatment effects
    # Also get cohort specific trends (modify time FE)
    mutate(error = rnorm(nobs*31, 0, 1),
           treat = ifelse((year >= cohort_year)* (cohort_year != 2004), 1, 0),
           mu = 10, #ifelse(cohort_year==1992, 2, ifelse(cohort_year==1998, 1, 3)),
           tau = ifelse(treat == 1, mu, 0),
           year_fe = year_fe + 0.1*(year - cohort_year)
    ) %>% 
    # calculate cumulative treatment effects
    group_by(unit) %>% 
    mutate(tau_cum = cummax(tau)) %>% 
    ungroup() %>% 
    # calculate the dep variable
    mutate(dep_var = (2010 - cohort_year) + unit_fe + year_fe + tau_cum + error) %>%
    # Relabel 2004 cohort as never-treated
    mutate(cohort_year = ifelse(cohort_year == 2004, Inf, cohort_year))
  
}
#----------------------------------------------------------------------------
# make data
data <- make_data4()

# plot
plot4 <- data %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) + 
  geom_line(alpha = 1/8, color = "grey") + 
  geom_line(data = data %>% 
              group_by(cohort_year, year) %>% 
              summarize(dep_var = mean(dep_var)),
            aes(x = year, y = dep_var, group = factor(cohort_year),
                color = factor(cohort_year)),
            size = 2) + 
  labs(x = "", y = "Value",  color = "Treatment group   ") + 
  geom_vline(xintercept = 1986, color = '#E41A1C', size = 2) + 
  geom_vline(xintercept = 1992, color = '#377EB8', size = 2) + 
  geom_vline(xintercept = 1998, color = '#4DAF4A', size = 2) + 
  #geom_vline(xintercept = 2004, color = '#984EA3', size = 2) + 
  scale_color_brewer(palette = 'Set1') + 
  theme(legend.position = 'bottom',
        #legend.title = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("1986", "1992", "1998", "Never-treated"),
                     values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
  ggtitle("One draw of the DGP with heterogeneous treatment effect dynamics across cohorts \n and with a never-treated group")+
  theme(plot.title = element_text(hjust = 0.5, size=12))

plot4



# function to run ES DID
run_ES_DiD_sat_never_het_4 <- function(...) {
  
  # resimulate the data
  data <- make_data4()
  
  # make dummy columns
  data <- data %>% 
    # make relative year indicator
    mutate(rel_year = year - cohort_year)
  
  # get the minimum relative year - we need this to reindex
  min_year <- min(data$rel_year * (data$rel_year != -Inf), na.rm = T)
  
  # reindex the relative years
  data <- data %>% 
    mutate(rel_year2 = rel_year) %>% 
    mutate(rel_year = rel_year - min_year) %>% 
    dummy_cols(select_columns = "rel_year") %>% 
    select(-("rel_year_-Inf"))
  
  
  # make regression formula 
  indics <- paste("rel_year", (1:max(data$rel_year))[-(-1 - min_year)], sep = "_", collapse = " + ")
  keepvars <- paste("rel_year", c(-5:-2, 0:5) - min_year, sep = "_")  
  formula <- as.formula(paste("dep_var ~", indics, "| unit + year | 0 | state"))
  
  # run mod
  mod <- felm(formula, data = data, exactDOF = TRUE)
  
  # grab the obs we need
  # grab the obs we need
  mod2 <- tibble(
    estimate = mod$coefficients,
    term1 = rownames(mod$coefficients)
  )
  
  es <-
    mod2 %>% 
    filter(term1 %in% keepvars) %>% 
    mutate(t = c(-5:-2, 0:5)) %>% 
    select(t, estimate)
  es
}

data_sat_never_het_4 <- map_dfr(1:nrep, run_ES_DiD_sat_never_het_4)

ES_plot_sat_never_het_4 <- data_sat_never_het_4 %>% 
  group_by(t) %>% 
  summarize(avg = mean(estimate),
            sd = sd(estimate),
            lower.ci = avg - 1.96*sd,
            upper.ci = avg + 1.96*sd) %>% 
  bind_rows(tibble(t = -1, avg = 0, sd = 0, lower.ci = 0, upper.ci = 0)) %>% 
  mutate(true_tau = ifelse(t >= 0, 10, 0)) %>% 
  ggplot(aes(x = t, y = avg)) + 
  #geom_linerange(aes(ymin = lower.ci, ymax = upper.ci), color = 'darkgrey', size = 2) + 
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci), color = "lightgrey", alpha = 0.2) +
  geom_point(color = 'blue', size = 3) + 
  geom_line(aes(color = 'Estimated Effect'), size = 1) + 
  geom_line(aes(x = t, y = true_tau, color = 'True Effect'), linetype = "dashed", size = 2) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Relative Time", y = "Estimate") + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))+
  ggtitle("TWFE event-study regression with homogenous and constant treatment effects")+
  scale_color_manual(values = colors) + 
  theme(plot.title = element_text(hjust = 0.5, size=12),
        legend.position = "bottom", 
        legend.title = element_blank())

ES_plot_sat_never_het_4
ggsave("fig_2-4.png", path = tex_path)



















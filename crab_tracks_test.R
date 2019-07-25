
library(dplyr)
library(plotly)
library(TMB)
compile("randomeffect_hmm.cpp")
dyn.load(dynlib("randomeffect_hmm"))

## Source functions for converting coordinates
source("R/convert_coords.R")

## LOAD DATA -----------------------------------------------------------

tracks <- read.csv("data/SC_HMMdata_final.csv")[, -1]
tracks <- tracks %>%
    group_by(ID) %>%
    mutate(dist = distance(x, y), heading = heading(x, y)) %>%
    ungroup() %>%
    mutate(id = as.numeric(factor(ID)))

hover_text <- paste("Distance:", round(tracks$dist, 2), "<br>",
                    "Heading:", round(rad2deg(tracks$heading)))

plot_ly(data = tracks, x = ~x, y = ~y, color = ~factor(ID), colors = viridis::viridis(100)) %>%
    add_trace(mode = "lines+markers", hoverinfo = "x+y+text",
              text = hover_text) %>%
    layout(yaxis = list(scaleanchor = "x"))

# FIT MODEL WITH NO RANDOM EFFECTS  ---------------------------------------

# data list passed to TMB
ind <- !is.na(tracks$angle)
tmb_dat <- list(step = tracks$step[ind],
                turn = tracks$angle[ind], # difference in heading from one point to next
                id = tracks$id[ind],
                n_states = 2)

# parameters to pass to TMB, must match this order
# Notice, values are specified for random effect parameters but these
# are ignored later.
tmb_par <- list(log_step_mu = rep(log(mean(tmb_dat$step)), tmb_dat$n_states),
                log_step_sd = rep(log(sd(tmb_dat$step)), tmb_dat$n_states),
                logit_turn_rho = rep(0, tmb_dat$n_states),
                ltpm = rep(0, tmb_dat$n_states ^ 2 - tmb_dat$n_states),
                step_r = rep(0, length(unique(tmb_dat$id))),
                log_step_ranef = 0)

# Create model object and specify using map that we want to fix the random effects
# to be their initial values (which are zero - so no random effects). This is specified
# by setting these valueus to factor NA in map.
obj <- MakeADFun(data = tmb_dat,
                 parameters = tmb_par,
                 map = list(step_r = as.factor(rep(NA, length(tmb_par$step_r))),
                            log_step_ranef = as.factor(NA)))

# fit model
fit <- nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr)

# compute variance components for parameter estimators
res <- sdreport(obj)

# format estimates
ests <- data.frame(est = res$value, sd = res$sd)
ests[1:4,] <- exp(ests[1:4,])
ests <- ests[-nrow(ests),]
ests <- round(ests, 2)
rownames(ests) <- c("stepmu1", "stepmu2", "stepsd1", "stepsd2", "rho1", "rho2", "tpm1-1", "tpm2-1", "tpm1-2", "tpm2-2")

ests
res

# FIT WITH RANDOM EFFECTS  ------------------------------------------------

# set starting values based on non-ranef model
tmb_par2 <- list(log_step_mu = res$par.fixed[1:2],
                 log_step_sd = res$par.fixed[3:4],
                 logit_turn_rho = res$par.fixed[5:6],
                 ltpm = res$par.fixed[7:8],
                 step_r = rep(0, length(unique(tmb_dat$id))),
                 log_step_ranef = 0)

# create ranef object, specify that step_r is the individual random effects. This
# tells TMB to integrate them out using Laplace approximation.
rf_obj <- MakeADFun(data = tmb_dat,
                    parameters = tmb_par2,
                    random = "step_r")

# fit model
rf_fit <- nlminb(start = rf_obj$par, objective = rf_obj$fn, gradient = rf_obj$gr)
# get variance components and random effect best predictions
rf_res <- sdreport(rf_obj)

# format fixed effect estimates
rf_ests <- data.frame(est = rf_res$value, sd = rf_res$sd)
rf_ests[1:4,] <- exp(rf_ests[1:4,])
rf_ests <- round(rf_ests, 2)
rownames(rf_ests) <- c("stepmu1", "stepmu2", "stepsd1", "stepsd2", "rho1", "rho2", "tpm1-1", "tpm2-1", "tpm1-2", "tpm2-2", "log_step_ranef")

rf_ests
rf_res





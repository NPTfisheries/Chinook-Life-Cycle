# Purpose: Fit stock-recruit models to juvenile emigrant and smolt data.
# Author: Ryan N. Kinzer
# Created: 2 April 2024


# load pkgs and functions
library(tidyverse)
source(here::here("R","predict_BH.R"))
source(here::here("R","predict_Ricker.R"))
source(here::here("R","fit_BH.R"))
source(here::here("R","fit_Ricker.R"))

# load data
stream <- 'Lostine River'
dat <- readxl::read_excel(here::here("data", "trib_data", "lostine.xlsx")) %>%
  mutate(across(.cols = everything(), as.numeric))

# fit emigration models ----

bh_emig_mod <- fit_bh(stock = dat$nat_spawners + dat$hat_spawners, recruits = dat$nat_emig, inits = list(350, .0005))
summary(bh_emig_mod)

r_emig_mod <- fit_ricker(stock = dat$nat_spawners + dat$hat_spawners, recruits = dat$nat_emig, inits = list(300, .0018))
summary(r_emig_mod)

#bquote(paste('Beverton-Holt: ', alpha == .(a), ", ", beta == .(b), ", ", capacity == .(k))

k_bh = coef(bh_emig_mod)[1]/coef(bh_emig_mod)[2]
max_stock <- 1/coef(r_emig_mod)[2]
k_r <- predict_Ricker(stock = max_stock, alpha = coef(r_emig_mod)[1], beta = coef(r_emig_mod)[2])

x_max <- 2500 #max(dat$nat_spawners+dat$hat_spawners)

png(paste0('./figures/trib_figures/',stream,'_SR_emigrant.png'),
    width = 500, height = 480, units = "px", pointsize = 12)
plot(dat$nat_spawners + dat$hat_spawners, dat$nat_emig, pch = 19, cex = 1,
     main = paste0(stream, ' Emigrant Stock-Recruit Curves'),
     ylab = 'Emigrants',
     xlab = 'Spawners',
     xlim = c(0, x_max),
     ylim = c(0, k_bh))
lines(0:x_max,
      predict_BH(stock = 0:x_max, alpha = coef(bh_emig_mod)[1], beta = coef(bh_emig_mod)[2]),
      col = 'blue')
lines(0:x_max,
      predict_Ricker(stock = 0:x_max, alpha = coef(r_emig_mod)[1], beta = coef(r_emig_mod)[2]),
      col = 'green')
abline(h = k_bh, lty = 2, col = 'blue')
abline(h = k_r, lty = 2, col = 'green')
segments(x0 = max_stock, y0 = 0, x1 = max_stock, y1 = k_r, lty = 3, col = 'green')
legend('bottomright', legend = c('Beverton-Holt', 'Ricker'), col = c('blue', 'green'), lty = c(2,2))
dev.off()

# fit smolt models ----

bh_smolt_mod <- fit_bh(stock = dat$nat_spawners + dat$hat_spawners, recruits = dat$nat_smolts, inits = list(150, .0002))
summary(bh_emig_mod)

r_smolt_mod <- fit_ricker(stock = dat$nat_spawners + dat$hat_spawners, recruits = dat$nat_smolts, inits = list(150, .00007))
summary(r_emig_mod)

#bquote(paste('Beverton-Holt: ', alpha == .(a), ", ", beta == .(b), ", ", capacity == .(k))

k_bh = coef(bh_smolt_mod)[1]/coef(bh_smolt_mod)[2]
max_stock <- 1/coef(r_smolt_mod)[2]
k_r <- predict_Ricker(stock = max_stock, alpha = coef(r_smolt_mod)[1], beta = coef(r_smolt_mod)[2])

x_max <- 2500 #max(dat$nat_spawners+dat$hat_spawners)

png(paste0('./figures/trib_figures/',stream,'_SR_smolt.png'),
    width = 500, height = 480, units = "px", pointsize = 12)
plot(dat$nat_spawners + dat$hat_spawners, dat$nat_smolts, pch = 19, cex = 1,
     main = paste0(stream, ' Smolt Stock-Recruit Curves'),
     ylab = 'Smolts',
     xlab = 'Spawners',
     #xlim = c(0, x_max),
     ylim = c(0, k_bh))
lines(0:x_max,
      predict_BH(stock = 0:x_max, alpha = coef(bh_smolt_mod)[1], beta = coef(bh_smolt_mod)[2]),
      col = 'blue')
lines(0:x_max,
      predict_Ricker(stock = 0:x_max, alpha = coef(r_smolt_mod)[1], beta = coef(r_smolt_mod)[2]),
      col = 'green')
abline(h = k_bh, lty = 2, col = 'blue')
abline(h = k_r, lty = 2, col = 'green')
segments(x0 = max_stock, y0 = 0, x1 = max_stock, y1 = k_r, lty = 3, col = 'green')
legend('bottomright', legend = c('Beverton-Holt', 'Ricker'), col = c('blue', 'green'), lty = c(2,2))
dev.off()

saveRDS(bh_smolt_mod, file = paste0('./data/sr_mod_fits/',stream,'_smolt_mod.rds'))

# keeping in case we want to combine across programs/trib
# dat_long <- dat %>%
#   pivot_longer(cols = c(contains('nat_'), contains('hat_'), contains('tot_'))) %>%
#   separate(name, into = c('origin', 'metric'), sep = '_', extra = 'merge') %>%
#   group_by(origin) %>%
#   nest() %>%
#   mutate(data = map(data, ~pivot_wider(.x, names_from = metric, values_from = value))) %>%
#   mutate(sr_smolt_mod = map(data, ~fit_bh(stock = tot_esc, recruits = nat_smolts, inits = list(150, .002))))

# fit SARs models ---- Needs work

# model as function of nat SAR?  Or binomial model?

dat$nat_sar <- dat$nat_by_return/dat$nat_smolts
dat$hat_sar <- dat$hat_by_return/dat$hat_smolts
dat$tau <- dat$nat_sar/dat$hat_sar
mu_tau <- mean(dat$tau[dat$tau<10], na.rm = TRUE) # truncate to 10......unless a higher value is real???
sd_tau <- sd(dat$tau[dat$tau<10], na.rm = TRUE)

hist(rlnorm(1000, log(mu_tau), log(sd_tau)))
hist(dat$tau[dat$tau<10])

plot(dat$brood_year, dat$nat_sar, type = 'l')
lines(dat$brood_year, dat$hat_sar, lty = 2)

sar_mod <- lm(dat$hat_sar ~ dat$nat_sar)
summary(sar_mod)

png(paste0('./figures/trib_figures/',stream,'_sar.png'),
    width = 500, height = 480, units = "px", pointsize = 12)
plot(dat$nat_sar, dat$hat_sar, pch = 19,
     main = paste("Adj R2 = ",signif(summary(sar_mod)$adj.r.squared, 5),
                          "Intercept =",signif(sar_mod$coef[[1]],5 ),
                          "\n Slope =",signif(sar_mod$coef[[2]], 5),
                          " P =",signif(summary(sar_mod)$coef[2,4], 5)),
     xlim = c(0, .2),
     ylim = c(0, .2))
abline(sar_mod, col = 'blue')
abline(a = 0, b = 1, lty = 2)
dev.off()

sar_dat <- na.omit(cbind(dat$nat_sar, dat$hat_sar))
R <- cor(sar_dat)
cor.test(sar_dat[,1], sar_dat[,2])
mu <- colMeans(log(sar_dat))
p <- length(mu)
sigma <- cov(log(sar_dat))


tmp <- MASS::mvrnorm(1000, mu, sigma) # tails are too heavy, need a truncated function

hist(exp(tmp[,1]))
hist(exp(tmp[,2]))

plot(1:1000, exp(tmp[,1]), type = 'l')
lines(1:1000, exp(tmp[,2]), lty = 2)


tmp_mvrnom <- function (n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE, EISPACK = FALSE) 
{
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) 
    stop("incompatible arguments")
  if (EISPACK) 
    stop("'EISPACK' is no longer supported by R", domain = NA)
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) 
    stop("'Sigma' is not positive definite")
  X <- matrix(rnorm(p * n), n)
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- X %*% svd(X, nu = 0)$v
    X <- scale(X, FALSE, TRUE)
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% 
    t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
    nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) 
    drop(X)
  else t(X)
}


# calc mean and std. from independent vectors
# draw standard normals

mvrlnormt <- function(n, range, mu, Sigma) {
  r <- log(range)
  p <- length(mu)
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  # range is a vector of two values
  
  F.a <- pnorm(min(r), mean = mu[1], sd = sqrt(Sigma[1,1]))
  F.b <- pnorm(max(r), mean = mu[1], sd = sqrt(Sigma[1,1]))
  
  u <- matrix(runif((p*n), min = F.a, max = F.b),n)
  X <- qnorm(u) # new X
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% 
    t(X)
  return(t(X))
}

tmp <- mvrlnormt(10, c(0,.10), mu, sigma)

hist(exp(tmp[,1]))
hist(exp(tmp[,2]))

plot(1:10, exp(tmp[,1]), type = 'l')
lines(1:10, exp(tmp[,2]), lty = 2)


# fit Age models --- Needs work

# use logistic regression
age_dat <- dat %>%
  select(brood_year, matches("[0-9]+"), matches("[0-9]+")) %>%
  pivot_longer(cols = -brood_year, names_to = 'key', values_to = 'value') %>%
  separate(key, into = c('origin', 'age')) %>%
  filter(value != 0,
         !is.na(value)) %>%
  filter(age != '6',
         age != '2') %>%
  group_by(brood_year, origin) %>%
  mutate(total = sum(value),
         failure = total - value,
         p = value/total) %>%
  mutate(brood_year = as.numeric(brood_year),
         age = factor(age, levels = c(3,4,5)),
         origin = as.factor(origin))

# success/failure
response <- cbind(age_dat$value, age_dat$failure)

age_full <- glm(response ~ age + origin + age:origin + brood_year,
                    data = age_dat,
                    family = binomial(link = 'logit'))

age_no_by <- glm(response ~ age + origin + age:origin,
                data = age_dat,
                family = binomial(link = 'logit'))

anova(age_no_by, age_full, test = 'Chi')

# brood year is not significant
age_no_int <- glm(response ~ age + origin,
                data = age_dat,
                family = binomial(link = 'logit'))

# interaction is significant
anova(age_no_int, age_no_by, test = 'Chi')

summary(age_no_by)

new_dat <- tibble(origin = rep(c('nat', 'hat'), each = 3),
                 age = rep(c('3', '4', '5'), 2))

preds <- predict(age_no_by, newdata=new_dat, type="response", se.fit=TRUE)
alpha <- .05  ## confidence level
cc <- -qt(alpha/2, df=Inf)*preds$se.fit

new_dat$p <- predict(age_no_by, newdat = new_dat, type = 'response')

fig_binom <- ggplot(data = age_dat, aes(x = age, y = p)) +
  geom_boxplot(aes(fill = origin), position = position_dodge(width = 0.75)) +
  geom_point(aes(shape = origin), position = position_dodge(width = 0.75)) +
  geom_point(data = new_dat, aes(x = age, y = p, shape = origin), position = position_dodge(width = 0.75), size = 4) +
  theme_minimal()

ggsave(paste0('./figures/trib_figures/',stream,'_binomial_age.png'), fig_binom)

# use multinomial regression
library(nnet)
library(ggeffects)

age_long <- age_dat %>%
  select(-(total:p)) %>%
  slice(rep(row_number(), value)) %>%
  select(-value) %>%
  mutate(age = as.factor(age))

age_mod <- multinom(age ~ origin + brood_year + origin:brood_year, data = age_long,
                    Hess = TRUE,
                    trace = FALSE)

age_no_int <- multinom(age ~ origin + brood_year, data = age_long,
                      Hess = TRUE,
                      trace = FALSE)

anova(age_no_int, age_mod) # interaction is not sign.

age_no_by <- multinom(age ~ origin, data = age_long,
                       Hess = TRUE,
                       trace = FALSE)

anova(age_no_by, age_no_int) # brood year is important, but equal for both

preds <- ggeffect(age_no_int, terms = c('origin', 'brood_year[1998:2023, by = 1]')) %>%
  as_tibble() %>%
  rename(origin = x, brood_year = group) %>%
  mutate(age = str_remove(response.level, 'X'),
         grp = paste(origin,age))

fig_multinomial <- ggplot(preds, aes(x = brood_year, y = predicted, group = grp)) +
  geom_line(aes(color = age, linetype = origin), size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .25) +
  geom_point(data = age_dat, aes(x = brood_year, y = p, colour = age, shape = origin, group = paste(age_dat$origin, age_dat$age))) +
  scale_color_brewer(palette = 'Dark2') +
  labs(title = 'Johnson Creek',
       subtitle = 'Modeled Age Proportions',
       x = 'Brood Year',
       y = 'Proportion') +
  ylim(c(0, 1)) +
  facet_wrap(~age) +
  theme_minimal()

ggsave(paste0('./figures/trib_figures/',stream,'_multinomial_age.png'), fig_multinomial)

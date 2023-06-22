# -------------------------------------------------------------------------- ###
# Soru 1a ----
# -------------------------------------------------------------------------- ###
https://github.com/slymanb/butunleme1.git


# -------------------------------------------------------------------------- ###
# Soru 2a ----
# -------------------------------------------------------------------------- ###
library(dplyr)

titanic %>%
  group_by(sex) %>%
  summarise(mean_fare = mean(fare))


# -------------------------------------------------------------------------- ###
# Soru 2b ----
# -------------------------------------------------------------------------- ###
library(ggplot2)

ggplot(data = na.omit(titanic), aes(x = sex, y = age)) +
  geom_boxplot()


# -------------------------------------------------------------------------- ###
# Soru 2c ----
# -------------------------------------------------------------------------- ###
titanic %>%
  ggplot(aes(x = age)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white") +
  geom_density(alpha = 0.5,
               fill = "red")

# -------------------------------------------------------------------------- ###
# Soru 3a ----
# -------------------------------------------------------------------------- ###
[10, 13, 14]


# -------------------------------------------------------------------------- ###
# Soru 3b ----
# -------------------------------------------------------------------------- ###
dat3 <- merge(dat1, dat2, all = TRUE)


# -------------------------------------------------------------------------- ###
# Soru 3c ----
# -------------------------------------------------------------------------- ###
ggplot(), geom_point(), aes(), geom_smooth() ve hrbrthemes::theme_ipsum() komutları kullanılmalıdır.


# -------------------------------------------------------------------------- ###
# Soru 3d ----
# -------------------------------------------------------------------------- ###
[1] 2.0 4.0


# -------------------------------------------------------------------------- ###
# Soru 3e ----
# -------------------------------------------------------------------------- ###
pnorm(1, mean = μ, sd = σ/sqrt(25))


# -------------------------------------------------------------------------- ###
# Soru 3f ----
# -------------------------------------------------------------------------- ###
tavla_simulasyonum <- function(n) {
  zar1 <- sample(1:6, n, replace = TRUE)
  zar2 <- sample(1:6, n, replace = TRUE)

  for (i in 1:n) {
    print("Atış", i, ": Zar 1 -", zar1[i], ", Zar 2 -", zar2[i], "\n")
  }
}


# -------------------------------------------------------------------------- ###
# Soru 3g ----
# -------------------------------------------------------------------------- ###
# Kurtulan yolcuların yaşları
age_survived <- titanic$titanic %>%
  filter(survived == 1) %>%
  pull(age)

# Kurtulamayan yolcuların yaşları
age_not_survived <- titanic$titanic %>%
  filter(survived == 0) %>%
  pull(age)

# Bağımsız iki örneklem t-testi
result <- t.test(age_survived, age_not_survived, var.equal = TRUE)

# Test istatistiği ve p-değeri
test_statistic <- result$statistic
p_value <- result$p.value

# Sonuçları ekrana yazdırma
print("Test İstatistiği:", test_statistic, "\n")
print ("p-değeri:", p_value, "\n")


# -------------------------------------------------------------------------- ###
# Soru 4a ----
# -------------------------------------------------------------------------- ###
#
library(tidyverse)

dat <- tibble(
  country = c("Ingiltere", "Almanya"),
  '2018' = c(8000, 10000),
  '2019' = c(8100, 11000),
  '2020' = c(8500, 10200)
)

dat2 <- dat %>%
  pivot_longer(cols = starts_with('20'), names_to = 'year', values_to = 'gdp')


# -------------------------------------------------------------------------- ###
# Soru 5a ----
# -------------------------------------------------------------------------- ###
ggplot(), aes(), geom_histogram(), facet_wrap(), labs() ve ggthemes::theme_stata()komutları kullanılmalıdır.



library(ggplot2)

dat <- tibble(
  price = c(326, 326, 327, 334, 335),
  cut = c("Ideal", "Premium", "Good", "Premium", "Good")
  depth = c(61.5, 59.8, 56.9, 62.4, 63.3)
  color = c("E", "E", "E", "I", "J")
)

ggplot(dat, aes(x = price, y = cut, fill = color)) +
  geom_histogram() +
  facet_wrap(~cut) +
  labs(y = "count", x = "price") +
  ggthemes::theme_stata() +
  theme(legend.position = "none")



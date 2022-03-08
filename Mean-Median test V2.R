library(tidyverse)
X1976_2020_house <- read_csv("1976-2020-house.csv")

house_2012 <- 
  X1976_2020_house %>% 
  filter(year==2012) %>% 
  mutate(dist_id = paste(year, state_po, district), .after = state_po)

house_2012_dem <- 
  house_2012 %>%
  filter(party == "DEMOCRAT") %>% 
  mutate(dem_votes = candidatevotes)
house_2012_dem_reduced <- house_2012_dem %>% select(dist_id,dem_votes)
house_2012_dem_reduced <- 
  house_2012_dem_reduced %>%
  group_by(dist_id) %>%
  summarise(dem_votes = sum(dem_votes, na.rm = T))


house_2012_rep <- 
  house_2012 %>%
  filter(party == "REPUBLICAN") %>% 
  mutate(rep_votes = candidatevotes)
house_2012_rep_reduced <- house_2012_rep %>% select(dist_id,rep_votes)
house_2012_rep_reduced <- 
  house_2012_rep_reduced %>%
  group_by(dist_id) %>%
  summarise(rep_votes = sum(rep_votes, na.rm = T))

house_2012_reduced <- 
  full_join(house_2012_dem_reduced,house_2012_rep_reduced,by = "dist_id") %>% 
  arrange(dist_id)

house_2012_reduced <- 
  house_2012_reduced %>% 
  replace(is.na(.),0)

house_2012_reduced <- 
  house_2012_reduced %>% 
  mutate(total_votes = rowSums(.[,2:3])) %>% 
  mutate(pct_dem_votes = dem_votes / total_votes * 100) %>% 
  mutate(pct_rep_votes = rep_votes / total_votes * 100)

house_2012_PA <- house_2012_reduced %>% 
  filter(grepl("PA", dist_id))

median_dem_pct_PA <- median(house_2012_PA$pct_dem_votes)
mean_dem_pct_PA <- mean(house_2012_PA$pct_dem_votes)

house_2012_PA %>% 
  ggplot(aes(x = pct_dem_votes)) +
  geom_dotplot(binwidth=1, color = "black", fill = "blue") + 
  geom_vline(aes(xintercept = median_dem_pct_PA, color = "Median"), 
             linetype = "dashed", show.legend = T) +
  geom_vline(aes(xintercept = mean_dem_pct_PA, color = "Mean"), 
             linetype = "dotted", show.legend = T) +
  scale_color_manual(name = "Measure",
                     breaks = c("Median", "Mean"),
                     values = c("Median"="red", "Mean"="forestgreen"))


data <- house_2012_reduced
B <- 100000
del_districts <- nrow(house_2012_PA)
samples_diff <- vector("numeric", B)
samples_mean <- vector("numeric", B)
samples_median <- vector("numeric", B)

for(samp in 1:B) {
  sample_delegation <- sample_n(data, del_districts)
  sample_delegation_pct_dem_mean <- weighted.mean(sample_delegation$pct_dem_votes, w = sample_delegation$total_votes)
  sample_delegation_pct_dem_median <- median(sample_delegation$pct_dem_votes)
  if(near(mean_dem_pct_PA, sample_delegation_pct_dem_mean, 0.1)){
    samples_mean[samp] <- sample_delegation_pct_dem_mean
    samples_median[samp] <- sample_delegation_pct_dem_median
    samples_diff[samp] <- (sample_delegation_pct_dem_mean - sample_delegation_pct_dem_median)
  }
}

samples <- data.frame(samples_mean,samples_median,samples_diff)
samples <- filter_all(samples, any_vars(. != 0))
quantile(samples$samples_median, c(0.05,1))


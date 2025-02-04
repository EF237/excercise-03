library(tidyverse)
library(ggplot2)
library(tidylog)
library(dplyr)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/data-wrangling.csv"
d <- read_csv(f, col_names = TRUE)

# Create a new variable named BSD (body size dimorphism) which is the ratio of average male to female body mass.
d <- d %>%
  mutate(BSD = Body_mass_male_mean/Body_mass_female_mean)

# Create a new variable named sex_ratio, which is the ratio of the number of adult females to adult males in a typical group.
d <- d %>%
  mutate(sex_ratio = AdultFemale/AdultMales)

# Create a new variable named DI (for “defensibility index”), which is the ratio of day range length to the diameter of the home range.
d <- d %>%
  mutate(DI = DayLength_km/(2 * sqrt(HomeRange_km2 / pi)))


# Plot the relationship between day range length (y axis) and time spent moving (x axis), 
# for these primate species overall and by family (i.e., a different plot for each family, e.g., by using faceting: + facet_wrap()). 
# Do species that spend more time moving travel farther overall? No
# How about within any particular primate family? No
# Should you transform either of these variables? Yes, HomeRange_km2

p <- ggplot(data = d, aes(x = (Move), y = (HomeRange_km2),
                          color = factor(Species))) +
  xlab("time spent moving") +
  ylab("day range length") +
  geom_point(na.rm = TRUE) +
  theme(legend.position = "none")

print(p)


p <- ggplot(data = d, aes(x = (Move), y = (HomeRange_km2),
                          color = factor(Species))) +
                            xlab("time spent moving") +
                            ylab("day range length") +
                            geom_point(na.rm = TRUE) +
                            facet_wrap(~Family, ncol = 4) +
                            theme(legend.position = "none")

print(p)

# Plot the relationship between day range length (y axis) and group size (x axis), overall and by family. 
# Do species that live in larger groups travel farther overall? 
# How about within any particular primate family? 
# Should you transform either of these variables?

p2 <- ggplot(data = d, aes(x = log(MeanGroupSize), y = (HomeRange_km2),
                           color = factor(Species))) +
                             xlab("log(group size)") +
                             ylab("day range length") +
                             geom_point(na.rm = TRUE) +
                             facet_wrap(~Family, ncol = 4) +
                             theme(legend.position = "none")
 

# Plot the relationship between canine size dimorphism (y axis) and body size dimorphism (x axis) overall and by family. 
# Do taxa with greater size dimorphism also show greater canine dimorphism?

p3 <- ggplot(data = d, aes(x = log(BSD), y = log(Canine_Dimorphism),
               color = factor(Species))) +
  xlab("log(body size dimorphism)") +
  ylab("log(canine size dimorphism)") +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Family, ncol = 4) +
  theme(legend.position = "none")


# Create a new variable named diet_strategy that is “frugivore” if fruits make up >50% of the diet, “folivore” if leaves make up >50% of the diet, and “omnivore” if diet data are available, but neither of these is true (i.e., these values are not NA). 
# Then, do boxplots of group size for species with different dietary strategies, omitting the category NA from your plot. 
# Do frugivores live in larger groups than folivores?
  
# HINT: To create this new variable, try combining mutate() with ifelse() or case_when() statements… check out the notes on “Conditional Expressions” in Module 11. Take a peak at the code snippet below if you get stuck!
d <- d %>%
  mutate(diet_strategy = case_when(
    Fruit >= 50 ~ "frugivore", 
    Leaves >= 50 ~ "folivore",
    Fruit < 50 & Leaves < 50 ~ "omnivore",  
    TRUE ~ NA
    ))

d2 <- d %>%
  drop_na(diet_strategy)

p4 <- ggplot(data = d2, aes(x = diet_strategy, y = log(MeanGroupSize))) +
               geom_boxplot(na.rm = TRUE) +
               theme(axis.text.x = element_text(angle = 90)) +
               xlab("Diet Strategy") +
               ylab("Group Size")

# In one line of code, using {dplyr} verbs and the forward pipe (|> or %>%) operator, do the following:
#   Add a variable, Binomial to the data frame d, which is a concatenation of the Genus and Species variables…
# Trim the data frame to only include the variables Binomial, Family, Brain_size_species_mean, and Body_mass_male_mean…
# Group these variables by Family…
# Calculate the average value for Brain_Size_Species_Mean and Body_mass_male_mean per Family (remember, you may need to specify na.rm = TRUE)…
# Arrange by increasing average brain size…
# And print the output to the console

d3 <- d %>%
  mutate(Binomial = paste(Genus, Species, sep = " ")) %>%
  select(Binomial, Family, Brain_Size_Species_Mean, Body_mass_male_mean) %>%
  group_by(Family) %>%
  summarise(
    Average_Brain_Size = mean(Brain_Size_Species_Mean, na.rm = TRUE),
    Average_Body_Mass = mean(Body_mass_male_mean, na.rm = TRUE)
  ) %>%
  arrange(Average_Brain_Size) 


  
                      
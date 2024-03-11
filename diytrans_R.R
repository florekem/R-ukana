library(tidyverse)

my_table <- read_delim("~/Pobrane/diy_r_assignment_data_file/lemis_cleaned.tsv")

glimpse(my_table)
unique(my_table$description)

# Q1
# Identify the most common (by ‘quantity’) 
# live mammal taken from the wild for import into the US.
# Q1.1. check how mammals are identified:
unique(my_table$class)
# A1.1. "Mammalia"
my_table_mammals <- my_table |> 
  filter(class == "Mammalia") |> 
  group_by(generic_name) |> 
  summarize(n = n())
# A1: BEAR
# Q2
# Building on your analysis above, produce a plot showing live mammals 
# (use ‘generic_name’) imported for the purposes of science/research. 
# (tip: use geom_col() in ggplot for this). Feel free to play around 
# with different themes to make your plot more exciting.
unique(my_table$purpose)
# "Scientific" 
my_table_mammals <- my_table |> 
  filter(class == "Mammalia" & purpose == "Scientific") |>   
  group_by(generic_name) |> 
  summarize(n = n()) |> 
  arrange(desc(n))

ggplot(my_table_mammals[1:10,], aes(x = generic_name, y = n)) +
  geom_col()
# Q3
# Identify the countries from which we import the most macaques 
# (again, a simple plot will suffice).
grep("^M", my_table$generic_name, value = TRUE)

my_table_mammals <- my_table |> 
  filter(generic_name == "MACAQUE") |>   
  group_by(country_origin) |> 
  summarize(n = n()) |> 
  arrange(desc(n))
  
ggplot(my_table_mammals[1:10,], aes(x = country_origin, y = n)) +
  geom_col() 

# Q4
# Using the same approach as above, create a plot 
# showing the countries from which we import live bats.
my_table_mammals <- my_table |> 
  filter(generic_name == "BAT") |>   
  group_by(country_origin) |> 
  summarize(n = n()) |> 
  arrange(desc(n))
 
ggplot(my_table_mammals[1:10,], aes(x = country_origin, y = n)) +
  geom_col()

# Q5
# For what purposes do we import bats?
glimpse(my_table)
my_table_mammals <- my_table |> 
  filter(generic_name == "BAT") |>   
  group_by(purpose) |> 
  summarize(n = n()) |> 
  arrange(desc(n))

# Q6
# How does the type of bat (use ‘specific_name’) 
# imported differ between countries 
# (hint: use facet_wrap in your ggplot code)?
my_table_mammals <- my_table |> 
  filter(generic_name == "BAT") |>   
  group_by(country_origin, specific_name) |> 
  summarize(n = n()) |> 
  arrange(desc(n))

ggplot(my_table_mammals[1:50,], aes(country_origin, n)) +
  geom_col() +
  facet_wrap(vars(specific_name), ncol = 5)


unique(my_table$country_imp_exp)














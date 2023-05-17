

nums <- sort(sample.int(nrow(ic_long), 5000, replace = FALSE))

ic_lite <- ic_long[nums, ] 

# Using dplyr
library(dplyr)
x <-
  %>% 
  merge(x=ihme_stack, y=ic_wide,
        by.x = c('ihme_Date', 'ihme_country_code'),
        by.y = c('ic_date', 
                          'ihme_country_code' = 'iso3c'
                 )) %>% View()

inner_join(x=ihme_stack, y=ic_long,
           by = c('ihme_Date' = 'ic_date',
                  'ihme_country_code' = 'ic_iso3c'
           )) %>% View()


df2 <- emp_df %>% inner_join( dept_df, 
                              by=c('emp_dept_id'='dept_id', 
                                   'emp_dept_branch_id'='dept_branch_id'))
df2

# Using merge
df2 <- merge(x=emp_df,y=dept_df, 
             by.x=c("emp_dept_id","emp_dept_branch_id"), 
             by.y=c("dept_id","dept_branch_id"))
df2



ic_long %>%
  pivot_wider(id_cols = c("ic_date", "ic_country", "ic_iso3c"),
              names_from = c("ic_compartment", "ic_fit_type"),
              values_from = c("ic_y_mean", "ic_y_025", "ic_y_975")
              ) %>% View()
a <- ihme_stack[["ihme_country_code"]] %>% sort() %>% unique() 
b <- ic_long[["ic_iso3c"]] %>% sort() %>% unique()
c <- intersect(a, b); c
length(a); length(b); length(c)
(c %in% b) %>% table()

ic_long %>% spread(c(ic_date, ic_iso3c, ic_compartment, ic_fit_type), ic_y_mean ) %>% View()
ic_deaths <- 
  ic_long %>% 
  filter(ic_compartment == "deaths") %>%
  rename("ic_y_mean_deaths" = "ic_y_mean",
         "ic_y_025_deaths" = "ic_y_025",
         "ic_y_975_deaths" = "ic_y_975") %>%
  select(-ic_compartment)

ic_infections <- 
  ic_long %>% 
  filter(ic_compartment == "infections") %>%
  rename("ic_y_mean_inf" = "ic_y_mean",
         "ic_y_025_inf" = "ic_y_025",
         "ic_y_975_inf" = "ic_y_975") %>%
  select(-ic_compartment)


common_cols <- intersect(names(ic_infections), 
                         names(ic_deaths))

merge(ic_infections, ic_deaths, 
      by = common_cols,
      all.x = TRUE, all.y = TRUE) %>%
  View()


  pivot_wider(names_from = c("ic_compartment"),
            values_from = c("ic_y_mean")) %>% 
  View()

library(dplyr)
library(tidyr)
library(stringr)
df %>%
  mutate(time = str_c("time", time)) %>%
  pivot_wider(names_from = time, values_from = c("x", "y"), names_sep="")


[1] "ic_date"             "ic_compartment"      "ic_y_mean"           "ic_fit_type"        
[5] "ic_death_calibrated" "ic_country"          "ic_iso3c" 

stocks <- tibble(
  time = as.Date("2009-01-01") + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocks

stocksm <- stocks %>% gather(stock, price, -time)
stocksm

stocksm %>% spread(stock, price)

stocksm %>% spread(time, price)

# Spread and gather are complements
df <- tibble(x = c("a", "b"), y = c(3, 4), z = c(5, 6))
df

df %>%
  spread(x, y) %>%
  gather("x", "y", a:b, na.rm = TRUE)

# Use 'convert = TRUE' to produce variables of mixed type
df <- tibble(
  row = rep(c(1, 51), each = 3),
  var = rep(c("Sepal.Length", "Species", "Species_num"), 2),
  value = c(5.1, "setosa", 1, 7.0, "versicolor", 2)
)
df %>% spread(var, value) %>% str()
df %>% spread(var, value, convert = TRUE) %>% str()
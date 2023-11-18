library(tidyverse)
library(highcharter)

house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-14/diwali_sales_data.csv')

house <- house %>% janitor::clean_names()

house %>% glimpse()

# User_ID	double	User identification number
# Cust_name	character	Customer name
# Product_ID	character	Product identification number
# Gender	character	Gender of the customer (e.g. Male, Female)
# Age Group	character	Age group of the customer
# Age	double	Age of the customer
# Marital_Status	double	Marital status of the customer (e.g. Married, Single)
# State	character	State of the customer
# Zone	character	Geographic zone of the customer
# Occupation	character	Occupation of the customer
# Product_Category	character	Category of the product
# Orders	double	Number of orders made by the customer
# Amount	double	Amount in Indian rupees spent by the customer


## Total amount by state - on a map
amount_by_state <-
  house %>%
  count(
    state,
    wt = sum(amount, na.rm = TRUE),
    sort = TRUE,
    name = "total_amount"
  ) 

hcmap("countries/in/custom/in-all-disputed", showInLegend = FALSE)

in_map_data <- get_data_from_map(download_map_data("countries/in/custom/in-all-disputed"))

glimpse(in_map_data)

## Check if all state is present
amount_by_state %>% 
  filter(!state %in% in_map_data$name)

in_map_data %>% 
  filter(str_detect(name, "^D")) %>% 
  pull(name)

## Uttarakhand and Telangana can't be found
## Let's do some resarch

amount_by_state <-
  amount_by_state %>%
  mutate(state = ifelse(state == "Delhi", "NCT of Delhi", state))

## We've made sure state is present
hcmap(
  "countries/in/custom/in-all-disputed",
  data = amount_by_state,
  value = "total_amount",
  joinBy = c("name", "state"),
  name = "Total sales",
  dataLabels = list(enabled = TRUE, format = "{point.name}"),
  borderColor = "#FAFAFA",
  borderWidth = 0.1,
  tooltip = list(
    valueDecimals = 2,
    valuePrefix = "₹",
    valueSuffix = "INR"
  )
) %>%
  hc_title(
    text = "Diwali Delights: A Journey Through India's Festive Sales Landscape",
    align = "left",
    style = list(
      color = "#000000",
      fontWeight = "bold",
      fontSize = 20
    )
  ) %>%
  hc_subtitle(
    text = "<i>Total amount by state.</i>",
    align = "left",
    useHTML = TRUE,
    style = list(color = "#000000",
                 fontSize = 15)
  ) %>%
  hc_caption(
    text = "Data: Tidytuesday || X: @emm_aguila",
    align = "left",
    style = list(color = "#000000",
                 fontSize = 15)
  ) %>%
  hc_legend(align = "left",
            verticalAlign = "top",
            title = "Total Sales")



## Total amount by age group - a bar chart
amount_by_age <-
  house %>%
  count(
    age_group,
    wt = sum(amount, na.rm = TRUE),
    name = "total_amount"
  )
 # mutate(age_group = fct_inorder(age_group))

highchart(type = "chart") |>
  hc_add_series(
    type = "column",
    data = amount_by_age,
    hcaes(x = "age_group",
          y = "total_amount"),
    name = "Total Sales"
  ) |>
  hc_plotOptions(series = list(showInLegend = FALSE)) %>% 
  hc_xAxis(categories = amount_by_age$age_group) %>% 
  hc_title(
    text = "Diwali Delights: A Journey Through India's Festive Sales Landscape",
    align = "left",
    style = list(
      color = "#000000",
      fontWeight = "bold",
      fontSize = 20
    )
  ) %>%
  hc_subtitle(
    text = "<i>Total amount by age group.</i>",
    align = "left",
    useHTML = TRUE,
    style = list(color = "#000000",
                 fontSize = 15)
  ) %>%
  hc_caption(
    text = "Data: <b>Tidytuesday</b> || X: <b>@emm_aguila</b>",
    align = "left",
    style = list(color = "#000000",
                 fontSize = 10)
  ) %>%
  hc_legend(align = "left",
            verticalAlign = "top",
            title = "Total Sales") %>% 
hc_tooltip(
    valueDecimals = 2,
    valuePrefix = "₹",
    valueSuffix = "INR",
    shape = "callout"
  
)


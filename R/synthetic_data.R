library(dplyr)

# Generate synthetic example data -----------------------------------------

set.seed(123)

ethnicity_detail <- c("Asian/Asian Brit. - Bangladeshi",
                      "Asian/Asian Brit. - Indian",
                      "Asian/Asian Brit. - Other",
                      "Asian/Asian Brit. - Pakistani",
                      "Black/Black Brit. - African",
                      "Black/Black Brit. - Caribbean",
                      "Black/Black Brit. - Other",
                      "Chinese",
                      "Mixed - Other",
                      "Mixed - White & Asian",
                      "Mixed - White & Black African",
                      "Mixed - White & Black Caribbean",
                      "Not Known",
                      "Not Stated",
                      "Other Ethnic Group",
                      "White - British",
                      "White - Irish",
                      "White - Other")

# Proportions generated from previously synthesised data

frequency_ethnicity <- c(0.001594896,
                         0.005980861,
                         0.005582137,
                         0.016746411,
                         0.00877193,
                         0.004385965,
                         0.000797,
                         0.00199362,
                         0.009968102,
                         0.003987241,
                         0.002392344,
                         0.009569378,
                         0.087719298,
                         0.138755981,
                         0.025119617,
                         0.657097289,
                         0.003189793,
                         0.016347687)

local_authority_name <- c("Ashfield",
                          "Bassetlaw",
                          "Broxtowe",
                          "Gedling",
                          "Mansfield",
                          "Newark and Sherwood",
                          "Nottingham",
                          "Rushcliffe")

fin_year_name <- c("2017/18",
                   "2018/19",
                   "2019/20"
                   )

# Ethnicity groupings

ethnicity_lookup <- tibble::tribble(
                                         ~ethnicity_detail,       ~ethnicity_category,
                        "Asian/Asian Brit. - Bangladeshi", "Asian or Asian British",
                             "Asian/Asian Brit. - Indian", "Asian or Asian British",
                              "Asian/Asian Brit. - Other", "Asian or Asian British",
                          "Asian/Asian Brit. - Pakistani", "Asian or Asian British",
                            "Black/Black Brit. - African", "Black or Black British",
                          "Black/Black Brit. - Caribbean", "Black or Black British",
                              "Black/Black Brit. - Other", "Black or Black British",
                                                "Chinese",    "Other Ethnic Groups",
                                          "Mixed - Other",                  "Mixed",
                                  "Mixed - White & Asian",                  "Mixed",
                          "Mixed - White & Black African",                  "Mixed",
                        "Mixed - White & Black Caribbean",                  "Mixed",
                                              "Not Known",              "Not known",
                                             "Not Stated",             "Not stated",
                                     "Other Ethnic Group",    "Other Ethnic Groups",
                                        "White - British",                  "White",
                                          "White - Irish",                  "White",
                                          "White - Other",                  "White"
                        )



# Generate random data ----------------------------------------------------

df_synth <- data.frame(
    local_authority_name = sample(local_authority_name, 2508, replace = TRUE),
    gender = sample(c("male", "female"), 2508, replace = TRUE),
    fin_year_name = sample(fin_year_name, 2508, replace = TRUE),
    age_at_referral = sample(seq(from = 17, to = 53), size = 2508, replace = TRUE),
    ethnicity_detail = sample(ethnicity_detail, 2508, replace = TRUE, prob = frequency_ethnicity),
    imd_decile = sample(seq(from = 1, to = 10), size = 2508, replace = TRUE)) %>% 
  left_join(ethnicity_lookup, by = "ethnicity_detail") %>% 
  mutate(imd_quintile = floor((imd_decile-1)/2) + 1,
         row_id = row_number())

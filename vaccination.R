library(tidymodels)
library(tidyverse)
library(skimr)

measles <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv")


measles_df <- measles %>% 
              filter(mmr > 0) %>% 
              transmute(state,
                        mmr_threshold = case_when(mmr > 95 ~ "Above",
                                                  TRUE ~ "Below")) %>% 
              mutate_if(is.character, factor)

skim(measles_df)


measles_df %>% 
      group_by(state) %>% 
      summarise(mmr = mean(mmr_threshold == "Above")) %>%
      mutate(state = fct_reorder(state, mmr)) %>% 
      ggplot(mapping = aes(x = state, y = mmr, fill = state)) +
      geom_col(show.legend = FALSE) + 
      scale_y_continuous(labels = scales::percent_format()) +
      coord_flip()

#how certain are we about differences between states? 
#classification model 

glm_fit <- logistic_reg() %>% 
           set_engine("glm") %>% 
           fit(mmr_threshold ~ state, data = measles_df)

tidy(glm_fit) %>% filter(p.value < 0.05)

#getting predictions out 
new_school <- tibble(state = unique(measles_df$state))

#predict the mean and confidence intervals 
mean_pred <- predict(glm_fit, 
                     new_data = new_school,
                     type = "prob")

conf_int <- predict(glm_fit,
                    new_data = new_school,
                    type = "conf_int")

schools_result <- new_school %>% 
                  bind_cols(mean_pred) %>% 
                  bind_cols(conf_int)

schools_result %>% 
    mutate(state = fct_reorder(state, .pred_Above)) %>% 
      ggplot(mapping = aes(x = state, y = .pred_Above, fill = state)) +
      geom_col(show.legend = FALSE) + 
      geom_errorbar(aes(ymin = .pred_lower_Above,
                        ymax = .pred_upper_Above),
                    color = "gray30") +
      scale_y_continuous(labels = scales::percent_format()) +
      coord_flip() + theme_minimal()





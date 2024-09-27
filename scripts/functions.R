nice_table <- function(table){
  # Function to make a nicely formatted html table 
  table %>% 
    kable() %>%
    kable_styling(full_width = F, 
                  font_size = 11) %>% 
    kable_paper(html_font = "Arial") 
}

theme_clean <- function(){
  # Theme without grid and transparent background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(colour = "black", fill='transparent'),
        legend.key = element_rect(fill = "transparent",
                                  color = 'transparent'))
}

theme_tern_clean <- function(){
  # Similar theme for ggtern plots
  theme(panel.grid.major = element_line(colour = "grey80", linewidth = .2),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(colour = "black", fill='transparent'),
        tern.axis.arrow.show = T)
}

transformD <- function(vals, c = 3){
  # Function to transform data to replace 0 and 1 as described in Douma and Weedon (2019)
  n <- length(vals)
  (vals*(n-1)+(1/c))/n
}


compare_0 <- function(data, variable,  ...){
  # Function to compare groups (three dots ...) of a posterior distribution (stored in variable of data) to zero. 
  # For each group, the fraction of the distribution above or below 0 is extracted. Only the smaller fraction is shown.
  
  # Get a list of quoted dots
  group_vars <- enquos(...)

  # check if variable is quoted
  variable <- enquo(variable)
  
  # get total number of draws in posterior distribution
  n_draws <- data %>% 
    group_by(!!!group_vars) %>% 
    summarise(n = n()) %>% 
    pull(n) %>% 
    unique()
  
  # check if same number of draws per group
  if(length(n_draws)>n_draws) {
    stop("Number of draws differs per groups")
  }
  
  # summarise data, extract fraction above and below 0, and filter for smaller fraction
  data %>% 
    group_by(!!!group_vars) %>% 
    count(diff > 0) %>% 
    clean_names() %>% 
    mutate(diff_0 = ifelse(diff_0, "above", "below"),
           frac = round(n/n_draws, 4)) %>% 
    select(-n) %>% 
    pivot_wider(names_from = diff_0, values_from = frac)%>% 
    replace(is.na(.), 0) %>% 
    nice_table()
}




dirichlet_r2 <- function(model, plot = T){
  
  # Extract posterior samples of the fitted values
  fitted_values <- posterior_epred(model)
  
  # Summarize the fitted values by taking the median across the draws
  fitted_values_median <- apply(fitted_values, c(2, 3), median)
  
  # get observed data from model
  observed_data <- model$data[,1]
  
  #get category names
  category <- colnames(observed_data)
  
  # Initialize a vector to store R² values for each response variable
  R2_dat <- data.frame(category = category,
                   R2 = NA)
  
  # Calculate the squared correlation for each response variable
  for (i in 1:ncol(observed_data)) {
    correlation <- cor(observed_data[, i], fitted_values_median[, i])
    R2_dat$R2[i] <- correlation^2

  }
  
  #make a plot of observed vs predicted values
  if(plot){
    plot_pred_obs <- bind_rows(
      data.frame(observed_data) %>% 
        mutate(type = "observed",
               row = 1:n()),
      
      data.frame(fitted_values_median) %>% 
        mutate(type = "predicted",
               row = 1:n())) %>% 
      
      pivot_longer(first(category):last(category), names_to = "category",values_to = "value") %>% 
      pivot_wider(names_from = type, values_from = value) %>%
      ggplot(aes(x = predicted, y = observed, col = category))+
      geom_point()+
      geom_smooth(method = "lm")+
      facet_grid(~category)+
      theme_clean()
    
    print(plot_pred_obs)
  }
  
  # Print the R² values
  return(R2_dat)
  
}

# colours
cols_sharks <- c("absent_Small" = "lightpink3",
                 "present_Small" = "lightpink4",
                 "absent_Medium" = "lemonchiffon3",
                 "present_Medium" = "lemonchiffon4")

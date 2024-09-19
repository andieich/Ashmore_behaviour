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
        panel.background = element_rect(colour = "black", fill='transparent'))
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
  
  # summariese data, extract fraction above and below 0, and filter for smaller fraction
  data %>% 
    filter(!!variable > 0) %>% 
    group_by(!!!group_vars) %>% 
    summarize(above = round(n()/n_draws, 4),
              below = round((n_draws - n())/n_draws, 4)) %>% 
    pivot_longer(above:below, names_to = "direction", values_to = "fraction") %>% 
    filter(fraction < .5) %>% 
    nice_table()
}

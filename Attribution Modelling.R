#Imports
library("dplyr")
library("purrr")
library("gt")
library("plotly")
library("stringr")
library("ChannelAttribution")
library("tidyr")


#Theoretical Transition Matrix
M = t(matrix(c(1,0,0,0,0,
               200,0,300,350,10,
               160,300,0,350,8,
               150,200,200,0,20,
               0,0,0,0,1), nrow = 5))

M = M/rowSums(M)  

colnames(M) = c("Non-Conversion","Display",
                "TV","Search", "Conversion")

row.names(M) = c("Non-Conversion","Display",
                 "TV","Search","Conversion")
rowSums(M)

#Chains to Paths (Simulation)
# 1. Randomly select a starting point by sampling from the set of media touchpoints
# 2. For each step: 
#     pick a row of probabilities from M corresponding to the current state/touchpoints
#     Take one sample from a Multinomial distribution while probabilities set to the row
#     Set the sample as the new current state and repeat
# 3. If we hit conversion or non-conversion we end the path

simulate_path = function(M, num_sim){
  num_sim = num_sim   
  path = vector(mode = "integer", length = num_sim)   
  path[1] = sample(2:(nrow(M)-1), 1) 
  
  for(i in 1:(num_sim-1)){
    p = M[path[i],]
    sn = which(rmultinom(1, 1, p) == 1)
    path[i+1] = sn     
    if(sn == 1 | sn == 5){
      break     
    }   
  }   
  return(path[path > 0]) 
}

#Simulating one path - Non conversion
set.seed(124) 
num_sim = 100 
path = simulate_path(M, num_sim) 
plot(seq_along(path),path ,type = "l",
     axes=FALSE, 
     ylim = c(1,5),
     main = "Non-Converter",
     ylab = "Channel Touchpoint",
     xlab = "Touchpoint Number") 
points(seq_along(path), path) 
axis(side = 2, at = 1:5) 
axis(side = 1, at = seq_along(path))

#Another one path simulation - Conversion
set.seed(007) 
num_sim = 100 
path = simulate_path(M, num_sim) 
plot(seq_along(path),path ,type = "l",
     axes=FALSE, 
     ylim = c(1,5),
     main = "Converter",
     ylab = "Channel Touchpoint",
     xlab = "Touchpoint Number") 
points(seq_along(path), path) 
axis(side = 2, at = 1:5) 
axis(side = 1, at = seq_along(path))

#Simulating a Full Data-set of Journeys
#Generating 10,000 paths
num_sim = 100 
num_paths = 10000  
paths = purrr::map(1:num_paths, ~simulate_path(M, num_sim))  
conversion = purrr::map(paths, ~ data.frame(conversion = ifelse(.x[length(.x)] == 5,"converter", "non-converter"))) %>% bind_rows(.id = "path_num")

pathdf = purrr::map(paths, ~data.frame(touchpoint = 1:length(.x), channel = .x)) %>% bind_rows(.id = "path_num") %>% left_join(conversion) %>% left_join(data.frame(channel_name = colnames(M), channel = 1:5))

head(pathdf,10) %>% gt::gt() %>% gt::tab_header(title = "Simmulated Paths")


#Plotting of paths to see how Markov Chain behaves
plotly::ggplotly(
  pathdf %>% 
    ggplot(aes(touchpoint, channel, color = conversion, 
               group = path_num)) + geom_line() +   
    labs(x = "Touchpoint Number", 
         y = "Channel Touchpoint") +  
    theme_minimal() 
)

table(conversion$conversion)/nrow(conversion)


#From Paths to Chains

#Data Preparation
named_paths =
  pathdf %>% group_by(path_num) %>% 
  group_split() %>% 
  purrr::map(~pull(.x,channel_name)) 

path_trim = 
  purrr::map(named_paths, ~.x[.x != "Non-Conversion" & .x != "Conversion"])  

journeydf = as_tibble(
  cbind(
    as.data.frame(do.call(rbind,
                          purrr::map(path_trim, ~str_c(.x, collapse = " > "))
    )
    ),conversion$conversion)
)

names(journeydf) = c("path","conversion")
journeydf = 
journeydf %>% 
group_by(path) %>% 
summarise(converters = 
              sum(if_else(conversion == "converter",1, 0)),             
          non_converters = 
              sum(if_else(conversion == "non-converter", 1, 0)
            )
) %>% 
arrange(-converters, -non_converters)  

head(journeydf, 15) %>% gt::gt() %>% 
gt::tab_header(
title = "Simmulated Journey Data (Top 15 Converters)"
)


#Quick Summary Stats
data.frame(
  metric = c("Converters",
             "Non Converters",
             "Number of Paths",
             "Conversion Rate %",
             "Unique Journeys"),
  value = c(sum(journeydf$converters),
            sum(journeydf$non_converters),
            sum(journeydf$non_converters) + sum(journeydf$converters),
            100*sum(journeydf$converters) / num_paths,
            nrow(journeydf))) %>%
  gt::gt() %>%
gt::tab_header(title = "Summary Statistics")

path_lengths = map_int(path_trim, ~length(.x))
summary(path_lengths)

hist(path_lengths, main = "Path Length Distribution")


#Transition Matrix Estimation

#We have our simulated data-set with a known transition matrix
#We can now see how we can go from data back to a transition matrix

tM = transition_matrix(journeydf,
                       var_path = "path",
                       var_conv = "converters",
                       var_null = "non_converters")

tM$transition_matrix %>% gt::gt() %>%
gt::tab_header(title = "Transition Probabilities")


#Running Attribution
mm_res = markov_model(journeydf,
                      var_path = "path",
                      var_conv = "converters",
                      var_null = "non_converters",
                      out_more = TRUE)

mm_res$result %>% gt::gt()


#Results Calculations from the removal effects
total_conversions = sum(journeydf$converters)
removal_effects = mm_res$removal_effects$removal_effects
relative_removal_effects = removal_effects/sum(removal_effects)
attributed_conversions = total_conversions * relative_removal_effects

attributed_conversions

removal_effects


#Comparing with Rule-Based Models (FirstTouch, LastTouch, LinearTouch)
#heuristic_models(journeydf,
#                 var_path = "path",
#                 var_conv = "converters") %>%
#  left_join(mm_res$result) %>%
#  gt::gt()

abc = heuristic_models(journeydf,
                       var_path = "path",
                       var_conv = "converters") %>% left_join(mm_res$result)

abc

#We can clearly see the difference between Last-Touch and the Markov Chain model
#results as the importance of Display and especially TV are well underestimated.
#Last-Touch attributed 50 fewer conversions to TV than the Markov Chain model.
#This example makes it clear how Markov Chain attribution can correct for the inherent bias of the Last-Touch attribution. 
#Linear Touch results are similar here, again because the transition probabilities are quite between the channels are not too far apart.


#Multi-Channel Attribution Result Visualizaton

#mta_res$results %>%
#  pivot_longer(mm_res,-mm_res$channel_name, names_to = "MTA_Model", values_to = "Conversions")%>%
#  ggplot(aes(MTA_Model, Conversions, fill = channel_namne)) +
#  geom_col(position = "dodge") +
#  labs(title = "MTA Model Result Comparision")
#theme_minimal()

abc %>%
  pivot_longer(-channel_name, names_to = "MTA_Model", values_to = "Conversions") %>%
  ggplot(aes(MTA_Model, Conversions, fill = channel_name)) +
  geom_col(position = "dodge") +
  labs(title = "MTA Model Result Comparision")



###############################################################################
# load dependencies
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

# format labels to avoid commas
format_number_for_plot_label <- function(vector) {
  # take a vector and do calculations in a DF
  df <- data.frame(input=vector, abs_num=abs(vector)) %>%
    mutate(out = vector) %>%
    mutate(out = ifelse(abs_num >= 1000000, paste0(round(input/1000000,1),'m'),out)) %>%    
    mutate(out = ifelse(abs_num >= 1000 & abs_num < 1000000, paste0(round(input/1000,1),'k'),out)) %>%
    mutate(out = ifelse(abs_num < 10, paste0(round(input,2)),out)) %>%
    mutate(out = ifelse(abs_num < 1000 & abs_num > 10, paste0(round(input)),out))    
  return(df$out)
}

# set method factor for plot
format_method_factor <- function(vector, reverse=FALSE) {
  if(reverse) {
    vector <- factor(vector, levels=c("Human","Beam Search","Sampling","Temperature","Top-K","Top-P"))
  } else {
    vector <- factor(vector, levels=c("Top-P","Top-K","Temperature","Sampling","Beam Search","Human"))    
  }
  return(vector)
}  
  
###############################################################################
# load our data into memory
our_data <- read.csv("https://raw.githubusercontent.com/DataJenius/GPT2_perplexity_repetition/main/data_evaluation_results.csv")


################################################################################################
# interaction of methods & prompts
summary <- our_data %>%
  group_by(method, prompt_source) %>%
  summarize(avg_ppl=mean(perplexity),
            avg_rep=mean(total_repetitions))


summary$method <- format_method_factor(summary$method)
# plot Avg PPL matrix
#plot <- ggplot(summary, aes(x=prompt_source, y=method, fill=avg_ppl, label=format_number_for_plot_label(avg_ppl))) +
plot2 <- ggplot(summary, aes(x=prompt_source, y=method, fill=avg_rep, label=format_number_for_plot_label(avg_rep))) +
  geom_tile(show.legend = TRUE, colour = "#000000", size=0.5) +
  geom_text(size=3) +
  theme_bw() +
  labs(x="Prompt", y="Method") +
  ggtitle("Average Total Repetitions, n=10") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  #scale_fill_gradient(low = "#eeeeee",high = "#f59542")
  scale_fill_gradient(low = "#eeeeee",high = "#8767c2")
grid.arrange(plot2, plot, ncol=2)



################################################################################################
# Why no ANOVA?
# ANOVA would be the easiest way to see effect of methods & prompts (interaction)
# However: we found pretty severe heterosexuality given the weird and chaotic distributions here
our_data$method <- format_method_factor(our_data$method, TRUE)
plot2 <- ggplot(our_data, aes(x=perplexity, fill=method)) +
#plot1 <- ggplot(our_data, aes(x=total_repetitions, fill=method)) +
  geom_density() +
  theme_bw() +
  #ggtitle("Raw Distributions") +
  ggtitle("Bootstrap Mean") +  
  labs(x="Total Repetitions", y="Density") +
  theme(legend.position = "none") +
  facet_grid(vars(method))
plot2


################################################################################################
# Bootstrap, baby! 
# If we don't have a normal, make one via the CLT.

# set seed for reproducibility
set.seed(42)

# cycle through all methods
data_bootstrap_results <- data.frame()
for (i in unique(our_data$method)) {
  
  # filter data to current method
  print(i)
  sub_data <- our_data %>% filter(method==i)
  
  # randomly sample with replacement, 1,000 iterations per method
  # take the means
  for(b in 1:1000) {
    tmp <- sample_n(sub_data, size=nrow(sub_data), replace=TRUE) 
    b_ppl <- mean(tmp$perplexity)
    b_rep <- mean(tmp$total_repetitions)  
    data_bootstrap_results <- rbind(data_bootstrap_results, data.frame(method=i, b=b, b_ppl=b_ppl, b_rep=b_rep))
    print(b)
  }
}

# save/load results
saveRDS(data_bootstrap_results, "data_bootstrap_results.Rds")
data_bootstrap_results <- readRDS("data_bootstrap_results.Rds")


################################################################################################
# Compare distributions
# repetitions is the better visual example to demonstrate this
data_bootstrap_results$method <- format_method_factor(data_bootstrap_results$method, TRUE)
#plot <- ggplot(our_data, aes(x=perplexity, fill=method)) +
#plot <- ggplot(data_bootstrap_results, aes(x=b_ppl, fill=method)) +
plot1 <- ggplot(our_data, aes(x=total_repetitions, fill=method)) +
#plot2 <- ggplot(data_bootstrap_results, aes(x=b_rep, fill=method)) +
  geom_density() +
  facet_grid(vars(method)) +
  theme_bw() +
  ggtitle("Raw Distributions") +
  #ggtitle("Bootstrap Mean") +  
  labs(x="Total Repetitions", y="Density") +
  theme(legend.position = "none") 
plot1
grid.arrange(plot1, plot2, ncol=2)


################################################################################################
# Get 95% confidence intervals
# Top-P has less perplexity than Sampling, more than Top-K
# Repeitions - difference between Top-P and Top-K is NOT significant here
bootstrap_intervals <- data.frame()
for (i in unique(our_data$method)) {
  tmp <- data_bootstrap_results %>% filter(method==i) %>% arrange(b_ppl)
  min_ppl <- tmp[25,"b_ppl"]
  max_ppl <- tmp[975,"b_ppl"] 
  tmp <- data_bootstrap_results %>% filter(method==i) %>% arrange(b_rep)  
  min_rep <- tmp[25,"b_rep"]
  max_rep <- tmp[975,"b_rep"]    
  bootstrap_intervals <- rbind(bootstrap_intervals,data.frame(method=i, 
                                                              min_ppl=min_ppl, 
                                                              b_ppl=mean(c(min_ppl,max_ppl)),
                                                              max_ppl=max_ppl,
                                                              min_rep=min_rep, 
                                                              b_rep=mean(c(min_rep,max_rep)),
                                                              max_rep=max_rep))
  print(i)
}  

# plot 95% confidence of bootstrap estimates
bootstrap_intervals$method <- format_method_factor(bootstrap_intervals$method, TRUE)
plot <- ggplot(bootstrap_intervals, aes(y=reorder(method,-b_ppl), x=b_ppl,xmin=min_ppl, xmax=max_ppl,color=method)) +
#plot <- ggplot(bootstrap_intervals, aes(y=reorder(method,-b_rep), x=b_rep,xmin=min_rep, xmax=max_rep,color=method)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y="Method", x="Perplexity") +
  ggtitle("Perplexity, 95% Confidence")
plot


################################################################################################
# Negative Correlation
# as perplexity goes up, repetitions goes down - makes sense
cor(our_data$perplexity,our_data$total_repetitions)
plot <- ggplot(our_data, aes(x=total_repetitions, y=perplexity, color=method)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ method, ncol = 3) +
  ggtitle("Negative Correlation")
plot



###############################################################################
### how closely do they resemble the human input? 

# min/max normalize data
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

# compare each prompt separately
# normalize to each prompt separately

# visualize what we are trying to do
tmp <- our_data %>%
  filter(prompt_source=="Douglas Adams") %>%
  mutate(total_repetitions_norm=normalize(total_repetitions),
         perplexity_norm=normalize(perplexity)) %>%
  mutate(my_alpha=0.75) %>%
  mutate(my_size=1) %>%  
  mutate(my_size=ifelse(method=="Top-P" & trial==5,3,my_size)) %>%
  mutate(my_size=ifelse(method=="Human",3,my_size)) %>%
  filter(total_repetitions_norm<=0.085) %>%
  filter(perplexity_norm<=0.6)
plot3 <- ggplot(tmp, aes(x=total_repetitions_norm, y=perplexity_norm, color=method, size=my_size, alpha=my_alpha)) +
  geom_point() +
  annotate("segment", x = 0, xend = 0.05333333, y = 4.302086e-01, yend = 2.032344e-01, colour = "red", size=1, alpha=1, arrow=arrow()) +
  annotate("segment", x = 0, xend = 0, y = 4.302086e-01, yend = 2.032344e-01, colour = "red", size=0.5, lty=2,alpha=1) +
  annotate("segment", x = 0, xend = 0.05333333, y = 2.032344e-01, yend = 2.032344e-01, colour = "red", size=0.5, lty=2,alpha=1) +
  geom_text(x=0.015, y=4.302086e-01+0.01, label="Generated Text", color="black", alpha=1, size=4) +  
  geom_text(x=0.015, y=4.302086e-01-0.02, label="Top-P, trial #5", color="black", alpha=1, size=3) +    
  geom_text(x=0.055, y=2.032344e-01-0.03, label="Human Text", color="black", alpha=1, size=4) +    
  geom_text(x=0.055, y=2.032344e-01-0.06, label="Douglas Adams", color="black", alpha=1, size=3) +      
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Calculating Distance-to-Human (DTH)") +
  labs(x="minmax_norm(Total Repetitions)", y="minmax_norm(Perplexity)")
plot1
plot2
plot3
grid.arrange(plot1, plot3, ncol=2)

#--------------------------------------------------------------------
# calculate DTH by each prompt
dth_scores <- data.frame()
for(i in unique(our_data$prompt_source)) {
  
  # normalize to data within each prompt only
  tmp <- our_data %>%
    filter(prompt_source==i) %>%
    mutate(total_repetitions_norm=normalize(total_repetitions),
           perplexity_norm=normalize(perplexity))
  
  # get the human
  human <- tmp %>% filter(method=="Human")
  tmp$human_total_repetitions_norm <- human$total_repetitions_norm
  tmp$human_perplexity_norm <- human$perplexity_norm  
  
  # calculate distance
  tmp$distance <- sqrt((tmp$human_total_repetitions_norm-tmp$total_repetitions_norm)^2 + (tmp$human_perplexity_norm-tmp$perplexity_norm)^2)
  dth_scores <- rbind(dth_scores,tmp)
  print(i)
}


#--------------------------------------------------------------------
# who is more human?
# if we look at average...
# Top-P is "more human" than Top-K in 4 out of 6 prompts, loses in Tale of Two Cities, The Bible
summary <- dth_scores %>%
  group_by(method, prompt_source) %>%
  summarize(avg_dth=mean(distance))


# plot Avg DTH matrix
summary$method <- format_method_factor(summary$method)
plot <- ggplot(summary, aes(x=prompt_source, y=method, fill=avg_dth, label=format_number_for_plot_label(avg_dth))) +
  geom_tile(show.legend = TRUE, colour = "#000000", size=0.5) +
  geom_text(size=3) +
  theme_bw() +
  labs(x="Prompt", y="Method") +
  ggtitle("Average Distance-to-Human, n=10") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_fill_gradient(low = "#eeeeee",high = "#ff0000")
plot

# sum method adds no new insight


#------------------------------------------------------------------------------
# bootstrap instead so we can make confidence intervals

# set seed for reproducibility
set.seed(42)

# cycle through all methods
data_bootstrap_results2 <- data.frame()
for (i in unique(our_data$method)) {
  
  # filter data to current method
  print(i)
  sub_data <- dth_scores %>% filter(method==i)
  
  # randomly sample with replacement, 1,000 iterations per method
  # take the means
  for(b in 1:1000) {
    tmp <- sample_n(sub_data, size=nrow(sub_data), replace=TRUE) 
    b_dist <- mean(tmp$distance)
    data_bootstrap_results2 <- rbind(data_bootstrap_results2, data.frame(method=i, b=b, b_dist=b_dist))
    print(b)
  }
}

# save/load results
saveRDS(data_bootstrap_results2, "data_bootstrap_results2.Rds")
data_bootstrap_results2 <- readRDS("data_bootstrap_results2.Rds")


#------------------------------------------------------------------------------
# Get 95% confidence intervals
# Top-P more human than Top-K, but not significantly so
# Both Top-P and Top-K are significantly closer to human than Temp, Samp, Beam
bootstrap_intervals2 <- data.frame()
for (i in unique(our_data$method)) {
  tmp <- data_bootstrap_results2 %>% filter(method==i) %>% arrange(b_dist)
  min_b_dist <- tmp[25,"b_dist"]
  max_b_dist <- tmp[975,"b_dist"] 
  bootstrap_intervals2 <- rbind(bootstrap_intervals2,data.frame(method=i, 
                                                                min_b_dist=min_b_dist, 
                                                                mean_b_dist=mean(c(min_b_dist,max_b_dist)),
                                                                max_b_dist=max_b_dist))
  print(i)
}  

# plot 95% confidence of bootstrap estimates
bootstrap_intervals2$method <- format_method_factor(bootstrap_intervals2$method, TRUE)
plot <- ggplot(bootstrap_intervals2, aes(y=reorder(method,-mean_b_dist), x=mean_b_dist,xmin=min_b_dist, xmax=max_b_dist,color=method)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y="Method", x="Distance-to-Human") +
  ggtitle("Distance-to-Human, 95% Confidence")
plot


###############################################################################
# what happens if we ignore the other prompts?
# set seed for reproducibility
set.seed(42)

# cycle through all methods
data_bootstrap_results3 <- data.frame()
for (i in unique(our_data$method)) {
  
  # filter data to current method
  print(i)
  #sub_data <- dth_scores %>% filter(method==i) %>% filter(!(prompt_source %in% c("Tale of Two Cities","The Bible")))
  sub_data <- dth_scores %>% filter(method==i) %>% filter(!(prompt_source %in% c("The Bible")))
  
  # randomly sample with replacement, 1,000 iterations per method
  # take the means
  for(b in 1:1000) {
    tmp <- sample_n(sub_data, size=nrow(sub_data), replace=TRUE) 
    b_dist <- mean(tmp$distance)
    data_bootstrap_results3 <- rbind(data_bootstrap_results3, data.frame(method=i, b=b, b_dist=b_dist))
    print(b)
  }
}

# save/load results
saveRDS(data_bootstrap_results3, "data_bootstrap_results3.Rds")
data_bootstrap_results3 <- readRDS("data_bootstrap_results3.Rds")

#------------------------------------------------------------------------------
# Get 95% confidence intervals
bootstrap_intervals3 <- data.frame()
for (i in unique(our_data$method)) {
  tmp <- data_bootstrap_results3 %>% filter(method==i) %>% arrange(b_dist)
  min_b_dist <- tmp[25,"b_dist"]
  max_b_dist <- tmp[975,"b_dist"] 
  bootstrap_intervals3 <- rbind(bootstrap_intervals3,data.frame(method=i, 
                                                                min_b_dist=min_b_dist, 
                                                                mean_b_dist=mean(c(min_b_dist,max_b_dist)),
                                                                max_b_dist=max_b_dist))
  print(i)
}  

# plot 95% confidence of bootstrap estimates
bootstrap_intervals3$method <- format_method_factor(bootstrap_intervals3$method, TRUE)
plot <- ggplot(bootstrap_intervals3, aes(y=reorder(method,-mean_b_dist), x=mean_b_dist,xmin=min_b_dist, xmax=max_b_dist,color=method)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y="Method", x="Distance-to-Human", subtitle="Omiting prompts from \"The Bible\" and \"Tale of Two Cities\"") +
  ggtitle("Distance-to-Human, 95% Confidence")
plot



###############################################################################
# bootstrap, but look by prompt

# set seed for reproducibility
set.seed(42)

# cycle through all methods
data_bootstrap_results4 <- data.frame()
for (i in unique(our_data$prompt_source)) {
  
  # filter data to current method
  print(i)
  sub_data <- our_data %>% filter(prompt_source==i) %>% filter(method!="Human")
  
  # randomly sample with replacement, 1,000 iterations per method
  # take the means
  for(b in 1:1000) {
    tmp <- sample_n(sub_data, size=nrow(sub_data), replace=TRUE) 
    b_ppl <- mean(tmp$perplexity)
    b_rep <- mean(tmp$total_repetitions)  
    data_bootstrap_results4 <- rbind(data_bootstrap_results4, data.frame(prompt=i, b=b, b_ppl=b_ppl, b_rep=b_rep))
    print(b)
  }
}

# save/load results
saveRDS(data_bootstrap_results4, "data_bootstrap_results4.Rds")
data_bootstrap_results4 <- readRDS("data_bootstrap_results4.Rds")


#------------------------------------------------------------------------------
# Get 95% confidence intervals
bootstrap_intervals4 <- data.frame()
for (i in unique(our_data$prompt_source)) {
  tmp <- data_bootstrap_results4 %>% filter(prompt==i) %>% arrange(b_ppl)
  min_ppl <- tmp[25,"b_ppl"]
  max_ppl <- tmp[975,"b_ppl"] 
  tmp <- data_bootstrap_results4 %>% filter(prompt==i) %>% arrange(b_rep)  
  min_rep <- tmp[25,"b_rep"]
  max_rep <- tmp[975,"b_rep"]    
  bootstrap_intervals4 <- rbind(bootstrap_intervals4,data.frame(prompt=i, 
                                                              min_ppl=min_ppl, 
                                                              b_ppl=mean(c(min_ppl,max_ppl)),
                                                              max_ppl=max_ppl,
                                                              min_rep=min_rep, 
                                                              b_rep=mean(c(min_rep,max_rep)),
                                                              max_rep=max_rep))
  print(i)
}  

# plot 95% confidence of bootstrap estimates
#bootstrap_intervals4$method <- format_method_factor(bootstrap_intervals3$method, TRUE)
#plot2 <- ggplot(bootstrap_intervals4, aes(y=reorder(prompt,b_ppl), x=b_ppl,xmin=min_ppl, xmax=max_ppl)) +
plot2 <- ggplot(bootstrap_intervals4, aes(y=reorder(prompt,b_rep), x=b_rep,xmin=min_rep, xmax=max_rep)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  #labs(y="Prompt", x="Perplexity", subtitle="All methods, by prompt") +
  labs(y="Prompt", x="Total Repetitions", subtitle="All methods, by prompt") +
  #ggtitle("Perplexity, 95% Confidence") +
  ggtitle("Total Repetitions, 95% Confidence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip()
plot2
grid.arrange(plot1, plot2, ncol=2)


###############################################################################
## extra plots for article

# sample probability distribution 
ggdata <- data.frame(token=c('jelly','chocolate','allergies','oatmeal','applesauce','...','galvanize','acumen','aardvark','xylophone'),
                     p=c(0.4,0.35,0.2,0.02,0.012,0.00525,0.004,0.003,0.002,0.0001),
                     my_color=c("blue","blue","blue","red","red","red","red","red","red","red"))
plot <- ggplot(ggdata, aes(x=reorder(token,-p), y=p, color=my_color, fill=my_color)) +
  geom_bar(stat="identity") +
  #geom_bar(stat="identity", fill="#6d8dbf",color="black") +  
  ggtitle("p(token|peanut butter and)") +
  labs(x="Token",y="Sample Probability Distribution") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none") +
  geom_vline(xintercept = "allergies", color="red", lty=3) +
  scale_fill_manual(name=" ",
                    values = c("blue"="#6d8dbf",
                               "red"="#f74848")) +
  scale_colour_manual(name=" ",
                    values = c("blue"="#000000",
                               "red"="#f74848"))  
plot


jojo <- our_data %>% 
  filter(trial==1) %>%
  filter(method=="Human")
ggplot(jojo,aes(x=total_repetitions, y=perplexity, label=prompt_source, color=prompt_source)) +
  geom_point() +
  geom_text(nudge_y=1) +
  ggtitle("Scores of Human Text") +
  labs(x="Total Repetitions", y="Perplexity") +
  theme_bw() +
  theme(legend.position = "none")


jojo <- our_data %>% 
  filter(trial==1) %>%
  filter(prompt_source=="Reddit") %>%
  filter(method=="Top-P")
jojo$generated_text

unique(our_data$prompt_source)



###############################################################################
## how similar are the texts?
# use  Levenshtein edit distance 
#library(RecordLinkage)

# no, try other instead
# https://www.rdocumentation.org/packages/stringdist/versions/0.9.8/topics/stringdist-metrics
library("stringdist")

# cycle through all prompts and methods
levenshtein_df <- data.frame()
for (my_prompt in unique(our_data$prompt_source)) {
  for (my_method in unique((our_data %>% filter(method!="Human"))$method)) {
    print(my_method)
    print(my_prompt)

    # get all text generated by this prompt/method combo
    tmp = our_data %>% filter(prompt_source==my_prompt) %>% filter(method==my_method)
    # cycle through each generated text
    for(i in 1:nrow(tmp)) {
      # compare each generated text to the other 9
      my_row <- tmp[i,]
      levenshtein_df <- rbind(levenshtein_df, data.frame(method=my_row$method,
                                                         prompt_source=my_row$prompt_source,
                                                         trial=my_row$trial,
                                                         lcs=stringsim(tmp[i,"generated_text"], tmp[-i,"generated_text"],method="lv")))
                                                         #levenshtein=levenshteinSim(tmp[i,"generated_text"], tmp[-i,"generated_text"])))
    }
  } 
}  



# bootstrap on prompt
data_bootstrap_prompt <- data.frame()
for (i in unique(levenshtein_df$prompt_source)) {
  
  # filter data to current method
  print(i)
  sub_data <- levenshtein_df %>% filter(prompt_source==i)
  
  # randomly sample with replacement, 1,000 iterations per method
  # take the means
  for(b in 1:1000) {
    tmp <- sample_n(sub_data, size=nrow(sub_data), replace=TRUE) 
    b_lcs <- mean(tmp$lcs)
    data_bootstrap_prompt <- rbind(data_bootstrap_prompt, data.frame(prompt_source=i, b=b, b_lcs=b_lcs))
    print(b)
  }
}



# bootstrap on method
data_bootstrap_method <- data.frame()
for (i in unique(levenshtein_df$method)) {
  
  # filter data to current method
  print(i)
  sub_data <- levenshtein_df %>% filter(method==i)
  
  # randomly sample with replacement, 1,000 iterations per method
  # take the means
  for(b in 1:1000) {
    tmp <- sample_n(sub_data, size=nrow(sub_data), replace=TRUE) 
    b_lcs <- mean(tmp$lcs)
    data_bootstrap_method <- rbind(data_bootstrap_method, data.frame(method=i, b=b, b_lcs=b_lcs))
    print(b)
  }
}

#------------------------------------------------------------------------------
# Get 95% confidence intervals - prompt
bootstrap_intervals5 <- data.frame()
for (i in unique(data_bootstrap_prompt$prompt_source)) {
  tmp <- data_bootstrap_prompt %>% filter(prompt_source==i) %>% arrange(b_lcs)
  min_lcs <- tmp[25,"b_lcs"]
  max_lcs <- tmp[975,"b_lcs"] 
  bootstrap_intervals5 <- rbind(bootstrap_intervals5,data.frame(prompt=i, 
                                                                min_lcs=min_lcs, 
                                                                b_lcs=mean(c(min_lcs,max_lcs)),
                                                                max_lcs=max_lcs))
  print(i)
}  

#------------------------------------------------------------------------------
# Get 95% confidence intervals - method
bootstrap_intervals6 <- data.frame()
for (i in unique(data_bootstrap_method$method)) {
  tmp <- data_bootstrap_method %>% filter(method==i) %>% arrange(b_lcs)
  min_lcs <- tmp[25,"b_lcs"]
  max_lcs <- tmp[975,"b_lcs"] 
  bootstrap_intervals6 <- rbind(bootstrap_intervals6,data.frame(method=i, 
                                                                min_lcs=min_lcs, 
                                                                b_lcs=mean(c(min_lcs,max_lcs)),
                                                                max_lcs=max_lcs))
  print(i)
}  

# plot 95% confidence of bootstrap estimates
#bootstrap_intervals4$method <- format_method_factor(bootstrap_intervals3$method, TRUE)
plot1 <- ggplot(bootstrap_intervals5, aes(y=reorder(prompt,b_lcs), x=b_lcs,xmin=min_lcs, xmax=max_lcs)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y="Prompt", x="Similarity of Output", subtitle="All methods, by prompt") +
  #ggtitle("Perplexity, 95% Confidence") +
  ggtitle("Levenshtein Distance, 95% Confidence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip()
plot1


# plot 95% confidence of bootstrap estimates
#bootstrap_intervals4$method <- format_method_factor(bootstrap_intervals3$method, TRUE)
bootstrap_intervals6$method <- format_method_factor(bootstrap_intervals6$method)
plot2 <- ggplot(bootstrap_intervals6, aes(y=reorder(method,b_lcs), x=b_lcs,xmin=min_lcs, xmax=max_lcs, color=method)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y="Prompt", x="Similarity of Output", subtitle="All prompts, by method") +
  #ggtitle("Perplexity, 95% Confidence") +
  ggtitle("Levenshtein Distance, 95% Confidence") +
  theme(legend.position="none")
plot2
grid.arrange(plot2, plot1, ncol=2)



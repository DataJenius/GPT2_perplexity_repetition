###############################################################################
# load dependencies
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

###############################################################################
# format labels to avoid commas
# added in v1.7 
format_number_for_plot_label <- function(vector) {
  
  # take a vector and do calculations in DF
  df <- data.frame(input=vector, abs_num=abs(vector)) %>%
    mutate(out = vector) %>%
    mutate(out = ifelse(abs_num >= 1000000, paste0(round(input/1000000,1),'m'),out)) %>%    
    mutate(out = ifelse(abs_num >= 1000 & abs_num < 1000000, paste0(round(input/1000,1),'k'),out)) %>%
    mutate(out = ifelse(abs_num < 10, paste0(round(input,2)),out)) %>%
    mutate(out = ifelse(abs_num < 1000 & abs_num > 10, paste0(round(input)),out))    
  return(df$out)
}


###############################################################################
# load our results into memory
#setwd("/Users/joshpause/Desktop/Experiments/GPT2")
my_data <- read.csv("data_evaluation_results.csv")

###############################################################################
# matrix view
matrix_data <- my_data %>%
  group_by(method, prompt_source) %>%
  summarise(avg_ppl=mean(perplexity),
            avg_rep=mean(total_repetitions))

# design matrix
plot <- ggplot(matrix_data, aes(x=prompt_source, y=method, fill=avg_ppl, label=format_number_for_plot_label(avg_ppl))) +
  geom_tile(show.legend = TRUE, colour = "#000000", size=0.5) +
  geom_text(size=3) +
  theme_bw() +
  labs(x="Prompt", y="Method") +
  ggtitle("Average Perplexity, 10 Samples") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_fill_gradient(low = "#eeeeee",high = "#f59542")
plot

# design matrix
plot <- ggplot(matrix_data, aes(x=prompt_source, y=method, fill=avg_rep, label=format_number_for_plot_label(avg_rep))) +
  geom_tile(show.legend = TRUE, colour = "#000000", size=0.5) +
  geom_text(size=3) +
  theme_bw() +
  labs(x="Prompt", y="Method") +
  ggtitle("Average Repetitions, 10 Samples") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_fill_gradient(low = "#eeeeee",high = "#f59542")
plot


###############################################################################
# stats by method
ggdata <- my_data %>%
  group_by(method) %>%
  summarise(avg_ppl=mean(perplexity),
            avg_rep=mean(total_repetitions))

plot <- ggplot(ggdata, aes(x=reorder(method,avg_ppl), y=avg_ppl, fill=avg_ppl, label=format_number_for_plot_label(avg_ppl))) +
  geom_bar(stat="identity", color="black") +
  geom_text(size=3) +
  theme_bw() +
  labs(x="Method", y="Perplexity") +
  ggtitle("Average Perplexity, 60 Samples") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_fill_gradient(low = "#eeeeee",high = "#f59542") +
  coord_flip()
plot

plot <- ggplot(ggdata, aes(x=reorder(method,avg_rep), y=avg_rep, fill=avg_rep, label=format_number_for_plot_label(avg_rep))) +
  geom_bar(stat="identity", color="black") +
  geom_text(size=3) +
  theme_bw() +
  labs(x="Method", y="Repetitions") +
  ggtitle("Average Repetitions, 60 Samples") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_fill_gradient(low = "#eeeeee",high = "#f59542") +
  coord_flip()
plot


###############################################################################
# scatterplot
ggplot_data <- my_data %>%
  group_by(method) %>%
  summarise(avg_ppl=mean(perplexity),
            avg_rep=mean(total_repetitions))

#plot <- ggplot(ggplot_data,aes(x=avg_rep, y=avg_ppl, color=method, label=method)) +
#plot <- ggplot(my_data,aes(x=log10(total_repetitions), y=log10(perplexity), color=method, shape=prompt_source, label=method)) +
#plot <- ggplot(my_data,aes(x=log10(total_repetitions), y=log10(perplexity), shape=method, color=prompt_source, label=method)) +
plot <- ggplot(my_data,aes(x=(total_repetitions), y=(perplexity), shape=method, color=prompt_source, label=method)) +
  geom_point() +
  facet_grid(vars(method), vars(prompt_source))
plot




###############################################################################
# variance matrix
matrix_data <- my_data %>%
  group_by(method, prompt_source) %>%
  summarise(avg_ppl=mean(perplexity),
            avg_rep=mean(total_repetitions),
            var_ppl=var(log10(perplexity)),
            var_rep=var(log10(total_repetitions)))

# design matrix
plot <- ggplot(matrix_data, aes(x=prompt_source, y=method, fill=var_ppl, label=format_number_for_plot_label(var_ppl))) +
  geom_tile(show.legend = TRUE, colour = "#000000", size=0.5) +
  geom_text(size=3) +
  theme_bw() +
  labs(x="Prompt", y="Method") +
  ggtitle("Perplexity Variance, 10 Samples") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_fill_gradient(low = "#eeeeee",high = "#f59542")
plot






############################################
# Anova
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(stringr)
#install.packages('terra', repos='https://rspatial.r-universe.dev')



#https://www.scribbr.com/statistics/anova-in-r/
one.way.method <- aov(perplexity ~ method, data = my_data)
one.way.prompt <- aov(perplexity ~ prompt_source, data = my_data)
two.way <- aov(perplexity ~ method+prompt_source, data = my_data)
interaction <- aov(perplexity ~ method*prompt_source, data = my_data)

# pick best model
model.set <- list(one.way.method, one.way.prompt, two.way, interaction)
model.names <- c("one.way.method", "one.way.prompt", "two.way", "interaction")
aictab(model.set, modnames = model.names)

# best model
summary(interaction)

par(mfrow=c(2,2))
plot(interaction)
par(mfrow=c(1,1))


# tukey
tukey.interaction<-TukeyHSD(interaction)
tukey.interaction


tukey.plot.aov<-aov(perplexity ~ method:prompt_source, data=my_data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


# simplified
tukey.plot.aov<-aov(perplexity ~ method, data=my_data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

tukey_df = data.frame(tukey.plot.test$method)
tukey_df$rn = row.names(tukey_df)
tukey_df <- tukey_df %>%
  mutate(rn=str_replace(rn,"-P","_P"),
         rn=str_replace(rn,"-K","_K")) %>%
  separate(rn, c("Method B","Method A"), sep = "-") %>%
  mutate(`Method A`=str_replace(`Method A`,"_P","-P"),
         `Method A`=str_replace(`Method A`,"_K","-K"),
         `Method B`=str_replace(`Method B`,"_P","-P"),
         `Method B`=str_replace(`Method B`,"_K","-K")) %>%
  mutate(fill_group=ifelse(diff > 0,"green","red"),
         fill_group=ifelse(p.adj > 0.05,"blue",fill_group))



# design matrix
plot <- ggplot(tukey_df, aes(x=`Method B`, y=`Method A`, fill=fill_group, label=format_number_for_plot_label(diff))) +
  geom_tile(show.legend = TRUE, colour = "#000000", size=0.5) +
  geom_text(size=3) +
  theme_bw() +
  labs(x="Method", y="Compare To") +
  ggtitle("Difference in Perplexity, 10 Samples") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position="none") +
  coord_flip() +
  scale_fill_manual(name=" ",
                    values = c("blue"="#eeeeee",
                               "green"="#5cd668",
                               "red"="#d6745c"))
plot




# does this Anova make sense?
# do we have homoskedacity? -- I think no
ggplot(my_data, aes(x=method, y=perplexity, color=method)) +
  geom_point() +
  ggtitle("Heteroskedacitiy") +
  coord_flip()

ggplot(my_data, aes(x=method, y=total_repetitions, color=method)) +
  geom_point() +
  ggtitle("Heteroskedacitiy") +
  coord_flip()

ggplot(my_data, aes(x=prompt_source, y=perplexity, color=method)) +
  geom_point() +
  ggtitle("Heteroskedacitiy") +
  coord_flip()

ggplot(my_data, aes(x=prompt_source, y=total_repetitions, color=method)) +
  geom_point() +
  ggtitle("Heteroskedacitiy") +
  coord_flip()



# bootstrap because we see evidence of heteroskedacity

# plot confidence intervals and distributions



#importing the pre-processing r script fie
source("Survey_Preprocessing.R")

## Compute a correlation matrix
corrplot(cor(CompleteResponses), method = "square")
corr <- round(cor(CompleteResponses), 1)
head(corr[, 1:7])
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(CompleteResponses)
head(p.mat[, 1:7])
# Visualize the correlation matrix
ggcorrplot(corr,lab = TRUE)
# method = "circle"
ggcorrplot(corr, method = "circle")
# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(corr, hc.order = TRUE,
           type = "lower", p.mat = p.mat)

#EDA
#converting the factor variables from double to factor 
CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)
CompleteResponses$brand<-as.factor(CompleteResponses$brand)
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
summary(CompleteResponses)
levels(CompleteResponses$brand) <- c("Acer", "Sony")


ggplot(data = CompleteResponses) +
  geom_bar(mapping = aes(x = brand))
#from the above graph brand1 has more customers than the other
ggplot(data = CompleteResponses)+
  stat_count(mapping = aes(x= brand))

ggplot(data = CompleteResponses)+
  geom_bar(
    mapping = aes(x=brand, y= ..prop.., group=1))

ggplot(data = CompleteResponses)+
  stat_summary(mapping = aes(x=brand, y = age),
               fun.min = min,
               fun.max = max,
               fun = median)
#salary & brand 
ggplot(data = CompleteResponses,
       mapping = aes(x=salary,fill = brand))+
  geom_histogram(bins = 4, stat = 'bin',
                 alpha = .5, position = "identity")

ggplot(data = CompleteResponses,
       mapping = aes(x=salary,fill = brand))+
  geom_histogram(bins = 4, stat = 'bin',
                 alpha = .5, position = "dodge")
ggplot(data = CompleteResponses,
       mapping = aes(x=salary,fill = brand))+
  geom_density(alpha = .5)
# Add median line to overlaid histograms
compare_mean <- CompleteResponses %>%
  group_by(brand) %>%
  summarise(Mean = mean(salary))
ggplot(data = CompleteResponses,
       mapping = aes(x=salary,fill = brand))+
  geom_histogram(bins = 4, stat = 'bin',
                 alpha = .5, position = "identity")+
  geom_vline(data = compare_mean, aes(xintercept = Mean, color = brand),
             linetype = "dashed", size = 1)

ggplot(CompleteResponses, aes(x = salary)) +
  geom_histogram(binwidth = 2000, color = "grey30", fill = "blue") +
  facet_grid(brand ~ .)

#boxplot
plot(CompleteResponses$brand,CompleteResponses$salary)
boxplot(CompleteResponses$age, data = CompleteResponses,
        xlab="Age")


ggplot(data = CompleteResponses) +
  geom_point(mapping = aes(x = salary, y = elevel, color = brand))

ggplot(data = CompleteResponses) +
  geom_point(mapping = aes(x = brand, y = elevel))

ggplot(data = CompleteResponses) +
  geom_point(mapping = aes(x = brand, y = salary))

#brand preference with other variable
ggplot(CompleteResponses, aes(x = credit)) +
  geom_histogram(binwidth = 20000, color = "grey30", fill = "blue") +
  facet_grid(brand ~ .)+
  scale_x_continuous(labels = comma) #no much pattern difference

ggplot(CompleteResponses, aes(x = zipcode)) +
  geom_bar(color = "grey30", fill = "blue") +
  facet_grid(brand ~ .)
ggplot(CompleteResponses, aes(x = car)) +
  geom_bar(color = "grey30", fill = "blue") +
  facet_grid(brand ~ .)

ggplot(CompleteResponses, aes(x = elevel)) +
  geom_bar(color = "grey30", fill = "blue") +
  facet_grid(brand ~ .)

ggplot(CompleteResponses, aes(x = age)) +
  geom_bar(color = "grey30", fill = "blue") +
  facet_grid(brand ~ .)
ggplot(CompleteResponses, aes(x = age)) +
  geom_histogram(binwidth = 0.5, color = "grey30", fill = "blue") +
  facet_grid(brand ~ .)
#there is no difference in the customer preference 
#based on any other variable other than salary  

#eda on brand1
ggplot(data = CompleteResponses, mapping = aes(x=elevel))+
  geom_bar(data = filter(CompleteResponses, brand == "Sony"),)
ggplot(data = CompleteResponses, mapping = aes(x=salary))+
  geom_histogram(data = filter(CompleteResponses, brand == "Sony"),
                 binwidth = 25000)
ggplot(data = CompleteResponses, mapping = aes(x=credit, fill= elevel))+
  geom_histogram(data = filter(CompleteResponses, brand == "Sony"),
                 bins = 4, stat="bin",)
ggplot(data = CompleteResponses, mapping = aes(x=salary, fill= elevel))+
  geom_histogram(data = filter(CompleteResponses, brand == "Sony"),
                 bins = 4, stat="bin",)
ggplot(data = CompleteResponses, mapping = aes(x=zipcode))+
  geom_bar(data = filter(CompleteResponses, brand == "Sony"),)

#eda on brand0
ggplot(data = CompleteResponses, mapping = aes(x=elevel))+
  geom_bar(data = filter(CompleteResponses, brand == "Acer"))
ggplot(data = CompleteResponses, mapping = aes(x=credit,  fill= elevel))+
  geom_histogram(data = filter(CompleteResponses, brand == "Acer"),
                 bins = 4, stat="bin",)

ggplot(data = CompleteResponses, mapping = aes(x=salary))+
  geom_histogram(data = filter(CompleteResponses, brand == "Acer"),
                 binwidth = 25000) + scale_x_continuous(labels = comma)
ggplot(data = CompleteResponses, mapping = aes(x=salary, fill= elevel))+
  geom_histogram(data = filter(CompleteResponses, brand == "Acer"),
                 bins = 4, stat="bin",)
ggplot(data = CompleteResponses, mapping = aes(x=zipcode))+
  geom_bar(data = filter(CompleteResponses, brand == "Acer"),)

bar <- ggplot(data = filter(CompleteResponses, brand == "Sony")) +
  geom_bar(
    mapping = aes(x = elevel, fill = elevel),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()

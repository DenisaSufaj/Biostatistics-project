
#import the dataset
library(ISLR2)
data("BrainCancer")

# Create the dataset
BrainCancer_df <- BrainCancer
BrainCancer_df <- na.omit(BrainCancer_df)

#define variable 
BrainCancer_df$ki #continuous variable 
BrainCancer$gtv #continuous

# View the first few rows of the created dataframe
head(BrainCancer_df)

#create the table to show the dataset
install.packages("gt")
install.packages("tibble")
library(tibble)
library(gt)
BrainCancer_df<-as_tibble(BrainCancer)
gt_tbl <- 
  gt(BrainCancer_df[1:8,]) |>
  tab_header(
    title = "Primary Brain Tumor Patients",
  ) |>
  tab_source_note(
    source_note = "Source: I. Selingerova, H. Dolezelova, I. Horova, S. Katina, and J. Zelinka. Survival of patients with primary brain tumors: Comparison of two statistical approaches. PLoS One, 11(2):e0148733, 2016. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4749663/"
  ) |>
  tab_source_note(
    source_note = md("Reference: James, G., Witten, D., Hastie, T., and Tibshirani, R. (2021) An Introduction to Statistical Learning with applications in R, Second Edition, https://www.statlearning.com, Springer-Verlag, New York")
  )
gt_tbl

#------------------------------------------------------------------------------
#Descriptive statistics - data distribution

#sex distribution

barplot(table(BrainCancer_df$sex), main = "Sex distribution", col = c("#41B8D5", "#2D8BBA"))
#difference not so significant 
library(ggplot2)
# Create a dataframe with the counts of each sex
sex_counts <- table(BrainCancer_df$sex)
sex_df <- data.frame(sex = names(sex_counts), count = as.numeric(sex_counts))

# Create the bar plot using ggplot
ggplot(sex_df, aes(x = sex, y = count, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(title = "Sex Distribution", x = "Sex", y = "Count") +
  scale_fill_manual(values = c("#41B8D5", "#2D8BBA"))
#difference not so significant 

#diagnosis distribution

colors <- c("#41B8D5", "#2D8BBA", "#2F5F98", "#6CE5E8")

# Create the bar plot
barplot(table(BrainCancer_df$diagnosis), 
        main = "Diagnosis distribution", 
        col = colors,
        legend = rownames(colors),
        args.legend = list(title = "Diagnosis"))

# Create a dataframe with the counts of each diagnosis
diagnosis_counts <- table(BrainCancer_df$diagnosis)
diagnosis_df <- data.frame(diagnosis = names(diagnosis_counts), count = as.numeric(diagnosis_counts))

# Create the bar plot using ggplot
ggplot(diagnosis_df, aes(x = diagnosis, y = count, fill = diagnosis)) +
  geom_bar(stat = "identity") +
  labs(title = "Diagnosis Distribution", x = "Diagnosis", y = "Count") +
  scale_fill_manual(values = colors) +
  theme_minimal()
num.dias <- matrix(c(nrow(BrainCancer_df[BrainCancer_df$diagnosis=='Meningioma',]), 
                     nrow(BrainCancer_df[BrainCancer_df$diagnosis=='HG glioma',]), 
                     nrow(BrainCancer_df[BrainCancer_df$diagnosis=='LG glioma',]), 
                     nrow(BrainCancer_df[BrainCancer_df$diagnosis=='Other',])),
                   ncol=4,nrow=1,byrow = T)

# Creare il grafico a torta con percentuali
diagnosis_df$percent <- diagnosis_df$count / sum(diagnosis_df$count) * 100
ggplot(diagnosis_df, aes(x = "", y = count, fill = diagnosis)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribuzione delle diagnosi") +
  theme_void() +
  scale_fill_manual(values = colors)

#con numero di pazienti
ggplot(diagnosis_df, aes(x = "", y = count, fill = diagnosis)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribuzione delle diagnosi") +
  theme_void() +
  scale_fill_manual(values = colors)
#Meningioma emerges as the most common diagnosis, 
#reflecting its prevalence among primary brain tumors.

#loc distribution

#Create a dataframe with the counts of each loc
loc_counts <- table(BrainCancer_df$loc)
loc_df <- data.frame(loc = names(loc_counts), count = as.numeric(loc_counts))

# Create the bar plot using ggplot
ggplot(loc_df, aes(x = loc, y = count, fill = loc)) +
  geom_bar(stat = "identity") +
  labs(title = "Loc Distribution", x = "loc", y = "Count") +
  scale_fill_manual(values = c("#41B8D5", "#2D8BBA"))
#Supratentorial higher value

#ki distribution

# Create the histogram using ggplot
median_value <- median(BrainCancer_df$ki)
ggplot(BrainCancer_df, aes(x = ki)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  geom_vline(xintercept = median_value, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Column", x = "Values", y = "Frequency") +
  annotate("text", x = median_value, y = 20, label = paste("Median:", median_value), color = "red", size = 4, vjust = -1)

ggplot(BrainCancer_df, aes(x=ki)) + 
  geom_histogram(binwidth = 10, fill="#41B8D5") +
  ylab("Frequency")+
  xlab("Karnofsky Index")+
  ggtitle("Karnofsky Index Values")+
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        plot.background = element_rect(fill = "#F2F4F5"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F2F4F5"),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12))

shapiro_test <- shapiro.test(BrainCancer_df$ki)
print(shapiro_test) # -->not normal

#gtv distribution
BrainCancer_df$gtv #valori continui

# Create the histogram using ggplot
median_value <- median(BrainCancer_df$gtv)
ggplot(BrainCancer_df, aes(x=gtv)) + 
  geom_histogram(binwidth = 5, fill="#41B8D5") +
  ylab("Frequency") +
  xlab("Gross Tumor Volume (cm^3)") +
  ggtitle("Gross Tumor Volume Values") +
  geom_vline(xintercept = median_value, linetype = "dashed", color = "red", size = 1) +  # Add vertical line for median
  annotate("text", x = median_value, y = 20, label = paste("Median:", median_value), color = "red", size = 4, vjust = -1) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        plot.background = element_rect(fill = "#F2F4F5"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F2F4F5"),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12))

# Esegui il test di Shapiro-Wilk
shapiro_test <- shapiro.test(BrainCancer_df$gtv)
print(shapiro_test)

#stereo distribution
barplot(table(BrainCancer_df$stereo), 
        main = "Histogram of Stereo", 
        xlab = "Values",
        ylab = "Frequency",
        col = c("#41B8D5", "#2D8BBA"))

# Creazione del grafico a barre con ggplot2
count_df <- as.data.frame(table(BrainCancer_df$stereo))
colnames(count_df) <- c("Stereo", "Frequency")
ggplot(count_df, aes(x = Stereo, y = Frequency, fill = Stereo)) +
  geom_bar(stat = "identity") +
  labs(title = "Histogram of Stereo", x = "Values", y = "Frequency") +
  scale_fill_manual(values = c("#41B8D5", "#2D8BBA")) + 
  theme_minimal()

#SRTR >>

#status distribtion
barplot(table(BrainCancer_df$status), 
        main = "Histogram of Status", 
        xlab = "Values",
        ylab = "Frequency",
        col = c("#41B8D5", "#2D8BBA"))

# Creazione del grafico a barre con ggplot2
count_df <- as.data.frame(table(BrainCancer_df$status))
colnames(count_df) <- c("Status", "Frequency")
ggplot(count_df, aes(x = Status, y = Frequency, fill = Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Histogram of Status", x = "Values", y = "Frequency") +
  scale_fill_manual(values = c("#41B8D5", "#2D8BBA")) 

#----------------------------------------------------------------------------------------------

#Biological question: Do the patient's characteristic impact the survival time? 
#which circumstances increase the risk of death?

#----Non-parametric methods 
#Is there an association between Diagnosis and GTV? (NOT SIGNIFICANT)
#Il test di Kruskal-Wallis è un test non parametrico utilizzato per confrontare le mediane di tre o 
#più gruppi indipendenti quando la variabile dipendente è continua (volume del tumore) e i gruppi sono definiti da una 
#variabile categorica (diagnosi che indica il tipo di tumore).
#Vuoi determinare se ci sono differenze significative nei volumi dei tumori tra i 
#diversi tipi di tumore. The Kruskal-Wallis test, which is a non-parametric test used to compare the distributions of a numerical variable across 
#multiple groups, is not appropriate for testing the association between two categorical variables.
#The Kruskal-Wallis test is designed to compare the medians of a numerical variable across two or 
#more groups defined by a single categorical variable. It does not assess the association or 
#relationship between two categorical variables directly.

library(stats)
kruskal.test(BrainCancer_df$gtv ~ BrainCancer_df$diagnosis, data = BrainCancer_df)
# il p-value è superiore al livello di significatività prefissato (α = 0,05), you would accept the null hp 
#and conclude that, according to the Kruskal-Wallis test, there are no significant 
#differences in tumor volumes among the different 
#diagnosis groups in the population from which the sample was drawn.
library(ggpubr)
ggplot(BrainCancer_df, aes(x = diagnosis, y = gtv, fill = diagnosis)) +
  geom_violin() +
  labs(x = "Diagnosis", y = "gtv", fill = "Diagnosis") +
  ggtitle("Distribution of Tumor Volumes by Diagnosis")

ggplot(BrainCancer_df, aes(x = diagnosis, y = gtv, color = diagnosis)) +
  geom_jitter() +
  labs(x = "Diagnosis", y = "gtv", color = "Diagnosis") +
  ggtitle("Tumor Volumes by Diagnosis")

ggplot(BrainCancer_df, aes(x = gtv, fill = diagnosis)) +
  geom_density(alpha = 0.5) +
  labs(x = "GTV", y = "Density", fill = "Diagnosis") +
  ggtitle("Density of Tumor Volumes by Diagnosis")

#Is there an association between Diagnosis and KI? (NOT SIGNIFICANT)
kruskal.test(BrainCancer_df$ki ~ BrainCancer_df$diagnosis, data = BrainCancer_df)
ggboxplot(BrainCancer_df, x = "diagnosis", y = "ki", 
          color = "diagnosis",
          order = c("Meningioma", "HG glioma", "LG glioma", "Other"),
          ylab = "ki", xlab = "Diagnosis")
kruskal.test(ki ~ diagnosis, data = BrainCancer)
#not significant 

#Is there an association between Diagnosis and Loc? (SIGNIFICANT)
#set up contingency table
num.s.meningioma <- nrow(BrainCancer_df[BrainCancer_df$loc=="Supratentorial" & BrainCancer_df$diagnosis=="Meningioma",])
num.i.meningioma <- nrow(BrainCancer_df[BrainCancer_df$loc=="Infratentorial" & BrainCancer_df$diagnosis=="Meningioma",])
num.s.hgglioma <- nrow(BrainCancer_df[BrainCancer_df$loc=="Supratentorial" & BrainCancer_df$diagnosis=="HG glioma",])
num.i.hgglioma <- nrow(BrainCancer_df[BrainCancer_df$loc=="Infratentorial" & BrainCancer_df$diagnosis=="HG glioma",])
num.s.lgglioma <- nrow(BrainCancer_df[BrainCancer_df$loc=="Supratentorial" & BrainCancer_df$diagnosis=="LG glioma",])
num.i.lgglioma <- nrow(BrainCancer_df[BrainCancer_df$loc=="Infratentorial" & BrainCancer_df$diagnosis=="LG glioma",])
num.s.other <- nrow(BrainCancer_df[BrainCancer_df$loc=="Supratentorial" & BrainCancer_df$diagnosis=="Other",])
num.i.other <- nrow(BrainCancer_df[BrainCancer_df$loc=="Infratentorial" & BrainCancer_df$diagnosis=="Other",])

dia.loc.contigencytbl <- matrix(c(num.s.meningioma, num.i.meningioma,
                                  num.s.hgglioma, num.i.hgglioma,
                                  num.s.lgglioma, num.i.lgglioma,
                                  num.s.other, num.i.other),ncol=2,nrow=4,byrow = T)
row.names(dia.loc.contigencytbl) <- c("Meningioma","HG glioma","LG glioma", "Other")
colnames(dia.loc.contigencytbl) <- c("Supratentorial", "Infratentorial")
dia.loc.contigencytbl
#perform chi squared test, but it is not valid because less than 80% of expected frequencies are more than 5
chi_test <- chisq.test(dia.loc.contigencytbl, correct = F)
chi_test
chi_test$observed
chi_test$expected
#perform fisher exact test
fisher <- fisher.test(dia.loc.contigencytbl)
fisher
#significant 
dia.loc.obs.df <- data.frame(
  diagnosis=c(rep(c("Meningioma","HG glioma", "LG glioma","Other"), 4)),
  patients=c(35,22,9,6,8,1,1,9,34.02, 18.19, 7.91, 11.87, 8.98, 4.80, 2.09, 3.13),
  loc=c(rep("Supratentorial", 4), rep("Infratentorial", 4), rep("Supratentorial",4), rep("Infratentorial", 4)),
  type=c(rep("Observed", 8), rep("Expected", 8))
)

ggplot(dia.loc.obs.df) +
  geom_bar(aes(x = type, y = patients, fill = loc),
           position = "stack",
           stat = "identity") +
  facet_grid(~ diagnosis, switch = "x") +
  ggtitle("Comparison of Observed and Expected Values for Each Diagnosis") +
  xlab("Diagnosis") +
  ylab("Patients") +
  labs(fill="Tumor Location") +
  scale_fill_manual(values = c("#6CE5E8", "#31356E")) +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = "white"),
        panel.spacing = unit(-.01,"cm"),
        plot.background = element_rect(fill = "#F2F4F5"),
        panel.background = element_rect(fill = "#F2F4F5"),
        legend.background = element_rect(fill = "#F2F4F5"),
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5))

#without difference btw expected and observed
ggplot(dia.loc.obs.df) +
  geom_bar(aes(x = diagnosis, y = patients, fill = loc),
           position = "stack",
           stat = "identity") +
  ggtitle("Comparison of Patients Across Diagnoses and Tumor Locations") +
  xlab("Diagnosis") +
  ylab("Patients") +
  labs(fill = "Tumor Location") +
  scale_fill_manual(values = c("#6CE5E8", "#31356E")) +
  theme(legend.position = "bottom")


#Is there an association between Diagnosis and Sex? (NOT SIGNIFICANT)
#set up contingency table
num.m.meningioma <- nrow(BrainCancer_df[BrainCancer_df$sex=="Male" & BrainCancer_df$diagnosis=="Meningioma",])
num.f.meningioma <- nrow(BrainCancer_df[BrainCancer_df$sex=="Female" & BrainCancer_df$diagnosis=="Meningioma",])
num.m.hgglioma <- nrow(BrainCancer_df[BrainCancer_df$sex=="Male" & BrainCancer_df$diagnosis=="HG glioma",])
num.f.hgglioma <- nrow(BrainCancer_df[BrainCancer_df$sex=="Female" & BrainCancer_df$diagnosis=="HG glioma",])
num.m.lgglioma <- nrow(BrainCancer_df[BrainCancer_df$sex=="Male" & BrainCancer_df$diagnosis=="LG glioma",])
num.f.lgglioma <- nrow(BrainCancer[BrainCancer_df$sex=="Female" & BrainCancer_df$diagnosis=="LG glioma",])
num.m.other <- nrow(BrainCancer_df[BrainCancer_df$sex=="Male" & BrainCancer_df$diagnosis=="Other",])
num.f.other <- nrow(BrainCancer[BrainCancer_df$sex=="Female" & BrainCancer_df$diagnosis=="Other",])

dia.sex.contigencytbl <- matrix(c(num.m.meningioma, num.f.meningioma,
                                  num.m.hgglioma, num.f.hgglioma,
                                  num.m.lgglioma, num.f.lgglioma,
                                  num.m.other, num.f.other),ncol=2,nrow=4,byrow = T)

row.names(dia.sex.contigencytbl) <- c("Meningioma","HG glioma","LG glioma", "Other")
colnames(dia.sex.contigencytbl) <- c("Male", "Female")
dia.sex.contigencytbl

#perform chi squared test, but it is not valid because less than 80% of expected frequencies are more than 5
chi_test <- chisq.test(dia.sex.contigencytbl, correct = F)
chi_test$expected
#perform fisher exact test
fisher <- fisher.test(dia.sex.contigencytbl)

#Convert expected frequencies to a dataframe
expected_df <- as.data.frame(chi_test$expected)
expected_df$Diagnosis <- row.names(expected_df)

# Reshape the dataframe for plotting
library(reshape2)
library(ggplot2)

# Create the barplot
expected_df_melted <- melt(expected_df, id.vars = "Diagnosis")
ggplot(expected_df_melted, aes(x = Diagnosis, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#6CE5E8", "#31356E")) +  
  labs(x = "Diagnosis", y = "Frequency", fill = "Sex") +
  ggtitle("Observed vs. Expected Frequencies by Diagnosis and Sex") +
  theme_minimal()


#Is there an association between Diagnosis and Stereo? (NOT SIGNIFICANT)
#set up contingency table
num.s.meningioma <- nrow(BrainCancer_df[BrainCancer_df$stereo=="SRS" & BrainCancer_df$diagnosis=="Meningioma",])
num.t.meningioma <- nrow(BrainCancer_df[BrainCancer_df$stereo=="SRT" & BrainCancer_df$diagnosis=="Meningioma",])
num.s.hgglioma <- nrow(BrainCancer_df[BrainCancer_df$stereo=="SRS" & BrainCancer_df$diagnosis=="HG glioma",])
num.t.hgglioma <- nrow(BrainCancer_df[BrainCancer_df$stereo=="SRT" & BrainCancer_df$diagnosis=="HG glioma",])
num.s.lgglioma <- nrow(BrainCancer_df[BrainCancer_df$stereo=="SRS" & BrainCancer_df$diagnosis=="LG glioma",])
num.t.lgglioma <- nrow(BrainCancer_df[BrainCancer_df$stereo=="SRT" & BrainCancer_df$diagnosis=="LG glioma",])
num.s.other <- nrow(BrainCancer_df[BrainCancer_df$stereo=="SRS" & BrainCancer_df$diagnosis=="Other",])
num.t.other <- nrow(BrainCancer_df[BrainCancer_df$stereo=="SRT" & BrainCancer_df$diagnosis=="Other",])

dia.stereo.contigencytbl <- matrix(c(num.s.meningioma, num.t.meningioma,
                                     num.s.hgglioma, num.t.hgglioma,
                                     num.s.lgglioma, num.t.lgglioma,
                                     num.s.other, num.t.other),ncol=2,nrow=4,byrow = T)

row.names(dia.stereo.contigencytbl) <- c("Meningioma","HG glioma","LG glioma", "Other")
colnames(dia.stereo.contigencytbl) <- c("SRS", "SRT")
dia.stereo.contigencytbl

#perform chi squared test, but it is not valid because less than 80% of expected frequencies are more than 5
chi_test <- chisq.test(dia.stereo.contigencytbl, correct = F)
chi_test
chi_test$observed
chi_test$expected
#perform fisher exact test
fisher <- fisher.test(dia.stereo.contigencytbl)
fisher
#p val is greater than 0.05, so we can conclude that there is not a significant association

#Is there an association between Sex and Loc? (NOT SIGNIFICANT)
#The chi-squared test for independence is a statistical test used to determine whether there 
#is a significant association between two categorical variables in a contingency table. 
num.m.supra <- nrow(BrainCancer_df[BrainCancer_df$sex=="Male" & BrainCancer_df$loc=="Supratentorial",])
num.f.supra <- nrow(BrainCancer_df[BrainCancer_df$sex=="Female" & BrainCancer_df$loc=="Supratentorial",])
num.m.infra <- nrow(BrainCancer_df[BrainCancer_df$sex=="Male" & BrainCancer_df$loc=="Infratentorial",])
num.f.infra <- nrow(BrainCancer_df[BrainCancer_df$sex=="Female" & BrainCancer_df$loc=="Infratentorial",])

sex.loc.contigencytbl <- matrix(c(num.m.supra, num.f.supra,
                                  num.m.infra, num.f.infra),ncol=2,nrow=2,byrow = T)

rownames(sex.loc.contigencytbl) <- c("Supratentorial","Infratentorial")
colnames(sex.loc.contigencytbl) <- c("Male", "Female")
sex.loc.contigencytbl

#perform chi squared test, with Yates continuity correction
chi_test <- chisq.test(sex.loc.contigencytbl, correct = T)
chi_test
chi_test$observed
chi_test$expected
#p val is greater than 0.05, so we can conclude that there is not a significant association

##Is there an association between Sex and KI? (NOT SIGNIFICANT)
m.ki.vals <- BrainCancer_df$ki[BrainCancer_df$sex=="Male"]
f.ki.vals <- BrainCancer_df$ki[BrainCancer_df$sex=="Female"]
wilcox.test(m.ki.vals, f.ki.vals, paired=F, alternative='two.sided',exact = F,correct = T)
#p val is greater than 0.05, so we can conclude that there is not a significant association

ggboxplot(BrainCancer_df, x = "sex", y = "ki", 
          color = "sex",
          order = c("Male", "Female"),
          ylab = "KI", xlab = "Sex")

#Is there an association between Sex and GTV? (NO SIGNIFICANT) 
#mann whitney U test
m.gtv.vals <- BrainCancer_df$gtv[BrainCancer_df$sex=="Male"]
f.gtv.vals <- BrainCancer_df$gtv[BrainCancer_df$sex=="Female"]

hist(m.gtv.vals, prob=T, main = 'Histogram of GTV values of Males', xlim=c(0,35), ylim=c(0,0.1), breaks=5)
hist(f.gtv.vals, prob=T, main = 'Histogram of GTV values of Females', xlim=c(0,35), ylim=c(0,0.1))
ggplot(BrainCancer_df, aes(x = sex, y = ki, fill = sex)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#6CE5E8", "#31356E")) +  
  labs(x = "Sex", y = "GTV", title = "Boxplot of GTV by Sex") +
  theme_minimal()

wilcox.test(m.gtv.vals, f.gtv.vals, paired=F, alternative='two.sided',exact = F,correct = T)
#p val is greater than 0.05, so we can conclude that there is not a significant association

##Is there an association between Sex and Stereo? (??)
num.m.srs <- nrow(BrainCancer_df[BrainCancer_df$sex=="Male" & BrainCancer_df$stereo=="SRS",])
num.f.srs <- nrow(BrainCancer_df[BrainCancer_df$sex=="Female" & BrainCancer_df$stereo=="SRS",])
num.m.srt <- nrow(BrainCancer_df[BrainCancer_df$sex=="Male" & BrainCancer_df$stereo=="SRT",])
num.f.srt <- nrow(BrainCancer_df[BrainCancer_df$sex=="Female" & BrainCancer_df$stereo=="SRT",])

sex.stereo.contigencytbl <- matrix(c(num.m.srs, num.f.srs,
                                     num.m.srt, num.f.srt),ncol=2,nrow=2,byrow = T)

row.names(sex.stereo.contigencytbl) <- c("SRS","SRT")
colnames(sex.stereo.contigencytbl) <- c("Male", "Female")
sex.stereo.contigencytbl

#perform chi squared test, with Yates continuity correction
chi_test <- chisq.test(sex.stereo.contigencytbl, correct = T)
chi_test
chi_test$expected
#p val is greater than 0.05, so we can conclude that there is not a significant association

###Is there an association between Loc and KI? (NOT SIGNIFICANT)
#mann whitney U test
supra.ki.vals <- BrainCancer_df$ki[BrainCancer_df$loc=="Supratentorial"]
infra.ki.vals <- BrainCancer_df$ki[BrainCancer_df$loc=="Infratentorial"]

hist(supra.ki.vals, prob=T, main = 'Histogram of Karnofsky Index values of Patients with Supratentorial Tumors', xlim=c(0,100), ylim=c(0,0.1), breaks=5)
hist(infra.ki.vals, prob=T, main = 'Histogram of Karnofsky Index values of Patients with Infratentorial Tumors', xlim=c(0,100), ylim=c(0,0.1))

ggplot(BrainCancer_df, aes(x = factor(loc, levels = c("Supratentorial", "Infratentorial")), y = ki, color = loc)) +
  geom_boxplot() +
  labs(x = "Tumor Location", y = "KI", color = "Tumor Location", title = "Boxplot of KI by Tumor Location") +
  theme_minimal()

wilcox.test(supra.ki.vals, infra.ki.vals, paired=F, alternative='two.sided',exact = F,correct = T)
#p val is greater than 0.05, so we can conclude that there is not a significant association

##Is there an association between Loc and GTV?
#mann whitney U test
supra.gtv.vals <- BrainCancer_df$gtv[BrainCancer_df$loc=="Supratentorial"]
infra.gtv.vals <- BrainCancer_df$gtv[BrainCancer_df$loc=="Infratentorial"]

hist(supra.gtv.vals, prob=T, main = 'Histogram of GTV values of Patients with Supratentorial Tumors', xlim=c(0,35), ylim=c(0,0.1), breaks=5)
hist(infra.gtv.vals, prob=T, main = 'Histogram of GTV values of Patients with Infratentorial Tumors', xlim=c(0,35), ylim=c(0,0.1))

ggplot(BrainCancer_df, aes(x = factor(loc, levels = c("Supratentorial", "Infratentorial")), y = gtv, color = loc)) +
  geom_boxplot() +
  labs(x = "Tumor Location", y = "GTV", color = "Tumor Location", title = "Boxplot of GTV by Tumor Location") +
  theme_minimal()

##Is there an association between Loc and Stereo? (SIGNIFICANT)
num.supra.srs <- nrow(BrainCancer_df[BrainCancer_df$loc=="Supratentorial" & BrainCancer_df$stereo=="SRS",])
num.infra.srs <- nrow(BrainCancer_df[BrainCancer_df$loc=="Infratentorial" & BrainCancer_df$stereo=="SRS",])
num.supra.srt <- nrow(BrainCancer_df[BrainCancer_df$loc=="Supratentorial" & BrainCancer_df$stereo=="SRT",])
num.infra.srt <- nrow(BrainCancer_df[BrainCancer_df$loc=="Infratentorial" & BrainCancer_df$stereo=="SRT",])

loc.stereo.contigencytbl <- matrix(c(num.supra.srs, num.infra.srs,
                                     num.supra.srt, num.infra.srt),ncol=2,nrow=2,byrow = T)

row.names(loc.stereo.contigencytbl) <- c("SRS","SRT")
colnames(loc.stereo.contigencytbl) <- c("Male", "Female")
sex.stereo.contigencytbl

#perform chi squared test
chi_test <- chisq.test(loc.stereo.contigencytbl, correct = F)
chi_test
chi_test$observed
chi_test$expected
#perform fisher exact test
fisher <- fisher.test(loc.stereo.contigencytbl)
fisher
#p val is less than 0.05, so we can conclude that there is a significant association
#bar plot
stereo.loc.obs.df <- data.frame(
  stereo=c(rep(c("SRS", "SRT"), 4)),
  patients=c(12,57,11,8,18.03,50.97,4.97,14.03),
  loc=c(rep("Supratentorial", 2), rep("Infratentorial", 2), rep("Supratentorial",2), rep("Infratentorial", 2)),
  type=c(rep("Observed", 4), rep("Expected", 4))
)
ggplot(stereo.loc.obs.df) +
  geom_bar(aes(x = type, y = patients, fill = loc),
           position = "stack",
           stat = "identity") +
  facet_grid(~ stereo, switch = "x") +
  ggtitle("Comparison of Observed and Expected Values for Each Stereotactic Method") +
  xlab("Stereotactic Method") +
  ylab("Patients") +
  labs(fill="Tumor Location") +
  scale_fill_manual(values = c("#6CE5E8", "#31356E")) +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = "white"),
        panel.spacing = unit(-.01,"cm"),
        plot.background = element_rect(fill = "#F2F4F5"),
        panel.background = element_rect(fill = "#F2F4F5"),
        legend.background = element_rect(fill = "#F2F4F5"),
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5))


##Is there an association between KI and GTV? (SIGNIFICANT) 

ggplot(BrainCancer_df, aes(x = ki, y = gtv, fill = factor(ki))) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "#6CE5E8", "#41B8D5", "#2D8BBA", "#2F5F98", "#31356E")) +
  labs(x = "Karnofsky Index", y = "GTV", fill = "Karnofsky Index Score",
       title = "Gross Tumor Volume by Karnofsky Index Score") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#F2F4F5"),
    legend.background = element_rect(fill = "#F2F4F5"),
    plot.background = element_rect(fill = "#F2F4F5"),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5)
  )
cor.test(BrainCancer_df$ki, BrainCancer_df$gtv, method='s', alternative = "less")
cor.test(BrainCancer_df$ki, BrainCancer_df$gtv, method='k', alternative = "less")
#significant negative correlation: as ki increases, gtv decreases
cor.test(BrainCancer_df$ki, BrainCancer_df$gtv, method='s', alternative = "greater")
cor.test(BrainCancer_df$ki, BrainCancer_df$gtv, method='k', alternative = "greater")

cor.test(BrainCancer_df$ki, BrainCancer_df$gtv, method='s', alternative = "g")
cor.test(BrainCancer_df$ki, BrainCancer_df$gtv, method='k', alternative = "g") 
# "g" option for the alternative parameter in the cor.test function corresponds to "two.sided"

#perchè sempre lo stesso pvalue?
#This is because both tests are non-parametric and rank-based, meaning they are 
#testing for the presence of any correlation (positive or negative), not specifically 
#greater or lesser correlation. Therefore, when you specify "greater" or "less", it's 
#essentially the same as testing for a two-sided alternative hypothesis 
#(alternative = "two.sided").

##Is there an association between KI and Stereo? (NOT SIGNIFICANT)
#mann whitney U test
srs.ki.vals <- BrainCancer_df$ki[BrainCancer_df$stereo=="SRS"]
srt.ki.vals <- BrainCancer_df$ki[BrainCancer_df$stereo=="SRT"]

hist(srs.ki.vals, prob=T, main = 'Histogram of Karnofsky Index values of Patients with SRS Treatment', xlim=c(0,100), ylim=c(0,0.1), breaks=5)
hist(srt.ki.vals, prob=T, main = 'Histogram of Karnofsky Index values of Patients with SRT Treatment', xlim=c(0,100), ylim=c(0,0.1))
ggplot(BrainCancer_df, aes(x = factor(stereo, levels = c("SRS", "SRT")), y = ki, color = stereo)) +
  geom_boxplot() +
  labs(x = "Stereo", y = "KI", color = "Stereo", title = "Karnofsky Index by Stereo") +
  theme_minimal()

wilcox.test(srs.ki.vals, srt.ki.vals, paired=F, alternative='two.sided',exact = F,correct = T)
#p val is greater than 0.05, so we can conclude that there is not a significant association

##Is there an association between GTV and Stereo? (SIGNIFICANT)
#mann whitney U test
srs.gtv.vals <- BrainCancer_df$gtv[BrainCancer_df$stereo=="SRS"]
srt.gtv.vals <- BrainCancer_df$gtv[BrainCancer_df$stereo=="SRT"]

hist(srs.gtv.vals, prob=T, main = 'Histogram of GTV values of Patients with SRS Treatment', xlim=c(0,35), ylim=c(0,0.2), breaks=5)
hist(srt.gtv.vals, prob=T, main = 'Histogram of GTV values of Patients with SRT Treatment', xlim=c(0,35), ylim=c(0,0.2))

ggplot(BrainCancer_df, aes(x = factor(stereo, levels = c("SRS", "SRT")), y = gtv, fill = stereo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#6CE5E8", "#31356E")) +
  labs(x = "Stereotactic Method", y = "GTV", fill = "Stereotactic Method",
       title = "Gross Tumor Volume by Stereotactic Method") +
  theme(
    panel.background = element_rect(fill = "#F2F4F5"),
    legend.background = element_rect(fill = "#F2F4F5"),
    plot.background = element_rect(fill = "#F2F4F5"),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5)
  )

wilcox.test(srs.gtv.vals, srt.gtv.vals, paired=F, alternative='less',exact = F,correct = T)
#p val is less than 0.05, so we can conclude that there is a significant association

#-------------------------------------------------------------------------------------------------
#SURVIVAL 
install.packages("survival")
library(survival)
install.packages("survminer")
library(survminer)


BrainCancer_df$status<- as.factor(BrainCancer_df$status)
brain_lollipop<-data.frame(
  Patients=rownames(BrainCancer_df),
  Time=BrainCancer_df$time
)

library(dplyr)
brain_lollipop %>%
  arrange(desc(Time)) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Patients=factor(Patients, levels=Patients)) %>%
  ggplot(aes(x=Patients, y=Time, color=BrainCancer_df$status)) +
  geom_segment( aes(x=Patients, xend=Patients, y=0, yend=Time), color="#41B8D5") +
  geom_point(size=3.5) +
  geom_point(aes(color=BrainCancer_df$status))+
  theme_light() +
  ggtitle("Time to Event for Each Patient") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_text(hjust = 0.5, size = 13),
    legend.text = element_text(size = 13),
    plot.background = element_rect(fill = "#F2F4F5"),
    legend.background = element_rect(fill = "#F2F4F5"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F2F4F5"),
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title.x = element_text(size=13),
    axis.title.y = element_text(size = 13)
  )+
  ylab("Time (in Months)")+
  labs(
    color="Status"
  )+
  scale_color_manual(labels = c("Censored", "Uncensored"), values = c("#6CE5E8", "#31356E"))


#basic KM, with no consideration of factors
install.packages("survival")
install.packages("survminer")
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
survival::myeloma
epitools::ratetable

fit_KM <- survfit(Surv(time, status) ~ 1, data = BrainCancer_df)
fit_KM
ggsurvfit(fit_KM, linewidth = 1) +
  add_confidence_interval() + # add confidence interval
  add_risktable() + # Add risk table
  add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+  # Specify median survival
  labs(title = "Kaplan-Meier Curve for Brain Cancer Survival",
       x="Time (Months)")+
  scale_x_continuous(breaks = seq(0,90,by=10))

#basic KM again, but with Greenwood formula for confidence intervals
fit_KM_gr <- survfit(Surv(time, status)~1, data = BrainCancer_df, conf.type='plain')
fit_KM_gr
ggsurvfit(fit_KM_gr, linewidth = 1) +
  add_confidence_interval() + # add confidence interval
  add_risktable() + # Add risk table
  add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+  # Specify median survival
  labs(title = "Kaplan-Meier Curve for Brain Cancer Survival",
       x="Time (Months)")+
  scale_x_continuous(breaks = seq(0,90,by=10))

#30 months
tail(fit_KM$surv[fit_KM$time<=30],1)
#60 months
tail(fit_KM$surv[fit_KM$time<=60],1)
#median survival time
median_surv<-fit_KM$time[fit_KM$surv<=0.5][1]
median_surv

surv_prob_30 <- tail(fit_KM$surv[fit_KM$time <= 30], 1)
surv_prob_60 <- tail(fit_KM$surv[fit_KM$time <= 60], 1)

# Assuming you have calculated the median survival time
median_surv <- fit_KM$time[fit_KM$surv <= 0.5][1]

# Create a bar plot
barplot(c(surv_prob_30, surv_prob_60),
        names.arg = c("30 Months", "60 Months"),
        ylim = c(0, 1), 
        main = "Survival Probabilities at 30 Months and 60 Months",
        xlab = "Time",
        ylab = "Survival Probability",
        col = c("lightblue", "steelblue"))  

# Add median survival time as a reference line
abline(h = 0.5, col = "red", lty = 2)
text(1.5, 0.5, paste("Median Survival:", median_surv, "months"), pos = 4, col = "red")

#Cumulative Incidence
survfit2(Surv(time, status) ~ 1,data=BrainCancer_df) |>
  ggsurvfit(linewidth = 1,type = "risk") +
  add_confidence_interval() + # add confidence interval
  add_risktable() + # Add risk table
  labs(title = "Cumulative Incidence for Brain Cancer Survival",
       x="Time (months)",
       y="Probability of death")+
  scale_x_continuous(breaks = seq(0,90,by=10))

#Cumulative Hazard
survfit2(Surv(time, status) ~ 1,data=BrainCancer_df) |>
  ggsurvfit(linewidth = 1,type = "cumhaz") +
  add_confidence_interval() + # add confidence interval
  add_risktable() + # Add risk table
  labs(title = "Cumululative Hazard for Brain Cancer Survival",
       x="Time (months)",
       y="Cumulative Hazard")+
  scale_x_continuous(breaks = seq(0,90,by=10))

#Log rank tests and Kaplan Meier Curves for Categorical Covariates
#Stereo
fit_KM_by_stereo <- survfit2(Surv(time, status) ~ stereo, data = BrainCancer_df)
survfit2(Surv(time, status)~stereo,data=BrainCancer_df) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval("lines") + # add confidence interval
  #add_risktable() + # Add risk table
  add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+  # Specify median survival
  labs(title = "Survival Probability by Stereotactic Method",
       x="Time (Months)",
       color="Stereotactic Method")+
  scale_x_continuous(breaks = seq(0,90,by=10))+
  add_pvalue("annotation", x=2, y=0.25) +  
  scale_color_manual(values = c("#6CE5E8", "#31356E"))+
  scale_fill_manual(values = c("#6CE5E8", "#31356E")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        plot.background = element_rect(fill = "#F2F4F5"),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F2F4F5"),
        legend.background = element_rect(fill = "#F2F4F5"),
        legend.title = element_text(hjust = 0.5, size = 13),
        legend.position = "right",
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 13))
fit_log_rank_stereo <- survdiff(Surv(time, status) ~ stereo, data = BrainCancer_df)
fit_log_rank_stereo

#sex (not significant by itsself)
fit_KM_by_sex <- survfit2(Surv(time, status) ~ sex, data = BrainCancer_df)
survfit2(Surv(time, status)~sex,data=BrainCancer_df) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval() + # add confidence interval
  add_risktable() + # Add risk table
  add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+  # Specify median survival
  labs(title = "Kaplan-Meier Curve for Brain Cancer Survival",
       x="Time (Months)")+
  scale_x_continuous(breaks = seq(0,90,by=10))+
  add_pvalue("annotation", x=2, y=0.25)
fit_log_rank_sex <- survdiff(Surv(time, status) ~ sex, data = BrainCancer_df)
fit_log_rank_sex

#diagnosis
fit_KM_by_dia <- survfit2(Surv(time, status) ~ diagnosis, data = BrainCancer_df)
survfit2(Surv(time, status)~diagnosis,data=BrainCancer_df) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval("lines") + # add confidence interval
  #add_risktable() + # Add risk table
  add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+  # Specify median survival
  labs(title = "Survival Probability by Diagnosis",
       x="Time (Months)",
       color="Diagnosis")+
  scale_x_continuous(breaks = seq(0,90,by=10))+
  add_pvalue("annotation", x=2, y=0.25) +  
  scale_color_manual(values = c("#6CE5E8", "#41B8D5", "#2F5F98", "#31356E"))+
  scale_fill_manual(values = c("#6CE5E8", "#41B8D5", "#2F5F98", "#31356E")) +
  theme(text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 18), 
        plot.background = element_rect(fill = "#F2F4F5"),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F2F4F5"),
        legend.background = element_rect(fill = "#F2F4F5"),
        legend.title = element_text(hjust = 0.5, size = 13),
        legend.position = "right",
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 13))
fit_log_rank_dia <- survdiff(Surv(time, status) ~ diagnosis, data = BrainCancer_df)
fit_log_rank_dia #pvalue di 2e-06

#C.I. più piccoli
levels(BrainCancer_df$diagnosis)[levels(BrainCancer_df$diagnosis)=="LG glioma"]="LG-Other"
levels(BrainCancer_df$diagnosis)[levels(BrainCancer_df$diagnosis)=="Other"]="LG-Other"

fit_KM_by_dia <- survfit2(Surv(time, status) ~ diagnosis, data = BrainCancer_df)
survfit2(Surv(time, status)~diagnosis,data=BrainCancer_df) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval("lines") + # add confidence interval
  #add_risktable() + # Add risk table
  add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+  # Specify median survival
  labs(title = "Survival Probability by Diagnosis",
       x="Time (Months)",
       color="Diagnosis")+
  scale_x_continuous(breaks = seq(0,90,by=10))+
  add_pvalue("annotation", x=2, y=0.25) +  
  scale_color_manual(values = c("#6CE5E8", "#41B8D5", "#2F5F98", "#31356E"))+
  scale_fill_manual(values = c("#6CE5E8", "#41B8D5", "#2F5F98", "#31356E")) +
  theme(text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 18), 
        plot.background = element_rect(fill = "#F2F4F5"),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F2F4F5"),
        legend.background = element_rect(fill = "#F2F4F5"),
        legend.title = element_text(hjust = 0.5, size = 13),
        legend.position = "right",
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 13))
fit_log_rank_dia <- survdiff(Surv(time, status) ~ diagnosis, data = BrainCancer_df)
fit_log_rank_dia #pvalue = 4e-07

#CI ancora più piccoli
levels(BrainCancer_df$diagnosis)[levels(BrainCancer_df$diagnosis) %in% c("LG glioma", "HG glioma", "Other")] <- "LG-HG-Other"
#pvalue of 9e-05

#location
fit_KM_by_loc <- survfit2(Surv(time, status) ~ loc, data = BrainCancer_df)
survfit2(Surv(time, status)~loc,data=BrainCancer_df) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval() + # add confidence interval
  add_risktable() + # Add risk table
  add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+  # Specify median survival
  labs(title = "Kaplan-Meier Curve for Brain Cancer Survival",
       x="Time (Months)")+
  scale_x_continuous(breaks = seq(0,90,by=10))+
  add_pvalue("annotation", x=2, y=0.25)
fit_log_rank_loc <- survdiff(Surv(time, status) ~ loc, data = BrainCancer_df)
fit_log_rank_loc

#ki
#finding the mean and distribution of ki in order to separate into categories
mean(BrainCancer_df$ki)
hist(BrainCancer_df$ki)
ggplot(BrainCancer_df, aes(ki)) + geom_histogram(bins=20)

#cutting into categories above and below 80
BrainCancer_df$kicat <- cut(BrainCancer_df$ki, breaks=c(0, 79, Inf), labels=c("low ki", "high ki"))

fit_KM_by_ki <- survfit2(Surv(time, status) ~ kicat, data = BrainCancer_df)
survfit2(Surv(time, status)~kicat,data=BrainCancer_df) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval("lines") + # add confidence interval
  #add_risktable() + # Add risk table
  add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+  # Specify median survival
  labs(title = "Survival Probability by Karnofsky Index",
       x="Time (Months)",
       color="Karnofsky Index")+
  scale_x_continuous(breaks = seq(0,90,by=10))+
  add_pvalue("annotation", x=2, y=0.25) +  
  scale_color_manual(labels = c("KI<80", "KI>=80"), values = c("#6CE5E8", "#31356E"))+
  scale_fill_manual(values = c("#6CE5E8", "#31356E")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        plot.background = element_rect(fill = "#F2F4F5"),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F2F4F5"),
        legend.background = element_rect(fill = "#F2F4F5"),
        legend.title = element_text(hjust = 0.5, size = 13),
        legend.position = "right",
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 13))
fit_log_rank_ki <- survdiff(Surv(time, status) ~ kicat, data = BrainCancer_df)
fit_log_rank_ki

#gtv
#finding the mean and distribution of gtv in order to separate into categories
mean(BrainCancer_df$gtv)
hist(BrainCancer_df$gtv)
ggplot(BrainCancer_df, aes(gtv)) + geom_histogram(bins=20)

#cutting into categories above and below 80
BrainCancer_df$gtvcat <- cut(BrainCancer_df$gtv, breaks=c(0, 15, 30, Inf), labels=c("small gtv", "medium gtv", "large gtv"))

fit_KM_by_gtv <- survfit2(Surv(time, status) ~ gtvcat, data = BrainCancer_df)
survfit2(Surv(time, status)~gtvcat,data=BrainCancer_df) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval("lines") + # add confidence interval
  #add_risktable() + # Add risk table
  add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+  # Specify median survival
  labs(title = "Survival Probability by Gross Tumor Volume",
       x="Time (Months)",
       color="Gross Tumor Volume (cm^3)")+
  scale_x_continuous(breaks = seq(0,90,by=10))+
  add_pvalue("annotation", x=2.3, y=0.25) +  
  scale_color_manual(labels = c("GTV<=15", "15<GTV<=30", "GTV>30"), values = c("#6CE5E8", "#41B8D5", "#31356E"))+
  scale_fill_manual(values = c("#6CE5E8", "#41B8D5", "#31356E")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        plot.background = element_rect(fill = "#F2F4F5"),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F2F4F5"),
        legend.background = element_rect(fill = "#F2F4F5"),
        legend.title = element_text(hjust = 0.5, size = 13),
        legend.position = "right",
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 13))
fit_log_rank_gtv <- survdiff(Surv(time, status) ~ gtvcat, data = BrainCancer_df)
fit_log_rank_gtv #pvalue = 0.04

#cut in a different way

BrainCancer_df$gtvcat <- cut(BrainCancer_df$gtv, breaks=c(0, 30, Inf), labels=c("small-medium gtv", "large gtv"))

fit_KM_by_gtv <- survfit2(Surv(time, status) ~ gtvcat, data = BrainCancer_df)
survfit2(Surv(time, status)~gtvcat,data=BrainCancer_df) |>
  ggsurvfit(linewidth = 1) +
  add_confidence_interval("lines") + # add confidence interval
  #add_risktable() + # Add risk table
  add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+  # Specify median survival
  labs(title = "Survival Probability by Gross Tumor Volume",
       x="Time (Months)",
       color="Gross Tumor Volume (cm^3)")+
  scale_x_continuous(breaks = seq(0,90,by=10))+
  add_pvalue("annotation", x=2.3, y=0.25) +  
  scale_color_manual(labels = c("GTV<=15", "15<GTV<=30", "GTV>30"), values = c("#6CE5E8", "#41B8D5", "#31356E"))+
  scale_fill_manual(values = c("#6CE5E8", "#41B8D5", "#31356E")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        plot.background = element_rect(fill = "#F2F4F5"),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F2F4F5"),
        legend.background = element_rect(fill = "#F2F4F5"),
        legend.title = element_text(hjust = 0.5, size = 13),
        legend.position = "right",
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 13))
fit_log_rank_gtv <- survdiff(Surv(time, status) ~ gtvcat, data = BrainCancer_df)
fit_log_rank_gtv #NO

#Coxph Models
#multivariate cox
fit.cox <- coxph(Surv(time, status) ~ sex+diagnosis+loc+ki+gtv+stereo,
                 data=BrainCancer_df)
summary(fit.cox) #significant all together

###Hazard Ratios Plot
ggforest(fit.cox, data=as.data.frame(BrainCancer_df))

###Goodness of Fit
ggcoxdiagnostics(fit.cox, type = "martingale", linear.predictions = T)
ggcoxdiagnostics(fit.cox, type = "deviance", linear.predictions = T)

###Proportional Hazard Assumption: all the variables can be included
diag.ph <- cox.zph(fit.cox)
diag.ph
ggcoxzph(diag.ph)

##Univariate Cox Models
#cox sex
cox.sex <- coxph(Surv(time, status) ~ sex,
                 data=BrainCancer_df)
summary(cox.sex)
#plot survival
sex_df <- with(lung,
               data.frame(sex = c("Male","Female") )
)
fit.sex <- survfit(cox.sex, newdata = sex_df)
plot(fit.sex, conf.int=F,
     col=c("dodgerblue2","darkmagenta"), lwd=2, lty=1,
     xlab='Time [months]', ylab='Survival Probability',
     main='Adjusted Survival Probability Plot')
grid()
legend('topright', c("Sex = Male", "Sex = Female"),
       lty=c(1,1), lwd=c(2,2), col=c("dodgerblue2","darkmagenta"))

#cox diagnosis
cox.dia <- coxph(Surv(time, status) ~ diagnosis,
                 data=BrainCancer_df)
summary(cox.dia) #significant
#plot survival
dia_df <- with(lung,
               data.frame(diagnosis = c("Meningioma","HG glioma", "LG glioma", "Other") )
)
fit.dia <- survfit(cox.dia, newdata = dia_df)
plot(fit.dia, conf.int=F,
     col=c("red","blue", "green", "orange"), lwd=2, lty=1,
     xlab='Time [months]', ylab='Survival Probability',
     main='Adjusted Survival Probability Plot')
grid()
legend('topright', c("Meningioma", "HG glioma", "LG glioma", "Other"),
       lty=c(1,1,1,1), lwd=c(2,2,2,2), col=c("red", "blue", "green", "orange"))

#cox loc
cox.loc <- coxph(Surv(time, status) ~ loc,
                 data=BrainCancer_df)
summary(cox.loc) #not significant
#plot survival
loc_df <- with(lung,
               data.frame(loc = c("Supratentorial","Infratentorial") )
)
fit.loc <- survfit(cox.loc, newdata = loc_df)
plot(fit.loc, conf.int=F,
     col=c("dodgerblue2","darkmagenta"), lwd=2, lty=1,
     xlab='Time [months]', ylab='Survival Probability',
     main='Adjusted Survival Probability Plot')
grid()
legend('topright', c("Supratentorial", "Infratentorial"),
       lty=c(1,1), lwd=c(2,2), col=c("dodgerblue2","darkmagenta"))

#cox ki
cox.ki <- coxph(Surv(time, status) ~ ki,
                data=BrainCancer_df)
summary(cox.ki) #significant
#plot survival
ki_df <- with(lung,
              data.frame(ki = c(60,90) )
)
fit.ki <- survfit(cox.ki, newdata = ki_df)
plot(fit.ki, conf.int=F,
     col=c("dodgerblue2","darkmagenta"), lwd=2, lty=1,
     xlab='Time [months]', ylab='Survival Probability',
     main='Adjusted Survival Probability Plot')
grid()
legend('topright', c("KI = 60", "KI = 90"),
       lty=c(1,1), lwd=c(2,2), col=c("dodgerblue2","darkmagenta"))

#cox gtv
cox.gtv <- coxph(Surv(time, status) ~ gtv,
                 data=BrainCancer_df)
summary(cox.gtv) #significant
#plot survival
quartiles <- quantile(BrainCancer_df$gtv)
gtv_df <- with(lung,
               data.frame(gtv = c(2.5,12.12) ) #1st and 3rd quantile values
)
fit.gtv <- survfit(cox.gtv, newdata = gtv_df)
plot(fit.gtv, conf.int=F,
     col=c("dodgerblue2","darkmagenta"), lwd=2, lty=1,
     xlab='Time [months]', ylab='Survival Probability',
     main='Adjusted Survival Probability Plot')
grid()
legend('topright', c("GTV = 2.5", "GTV = 12.12"),
       lty=c(1,1), lwd=c(2,2), col=c("dodgerblue2","darkmagenta"))

#cox stereo
cox.stereo <- coxph(Surv(time, status) ~ stereo,
                    data=BrainCancer_df)
summary(cox.stereo) #significant
#plot survival
stereo_df <- with(lung,
                  data.frame(stereo = c("SRT","SRS") )
)
fit.stereo <- survfit(cox.stereo, newdata = stereo_df)
plot(fit.stereo, conf.int=F,
     col=c("dodgerblue2","darkmagenta"), lwd=2, lty=1,
     xlab='Time [months]', ylab='Survival Probability',
     main='Adjusted Survival Probability Plot')
grid()
legend('topright', c("SRT", "SRS"),
       lty=c(1,1), lwd=c(2,2), col=c("dodgerblue2","darkmagenta"))

###Proportional Hazards Assumption for Categorical Covariates
ggsurvplot(fit_KM_by_sex,fun = "cloglog")
ggsurvplot(fit_KM_by_dia,fun = "cloglog")
ggsurvplot(fit_KM_by_loc,fun = "cloglog")
ggsurvplot(fit_KM_by_stereo,fun = "cloglog")

#univariets coxmodel are not so informative--------------------------

#Multivariate Cox
fit.cox <- coxph(Surv(time, status) ~ sex+diagnosis+loc+ki+gtv+stereo,
                 data=BrainCancer_df)
summary(fit.cox) #significant all together
#Hazard Ratios
ggforest(fit.cox, data=as.data.frame(BrainCancer_df))
#goodness of fit
ggcoxdiagnostics(fit.cox, type = "martingale", linear.predictions = T)
ggcoxdiagnostics(fit.cox, type = "deviance", linear.predictions = T)
#PH Assumption
diag.ph <- cox.zph(fit.cox)
diag.ph
ggcoxzph(diag.ph)

#cox model with all covariets and diagnosis groupd 
levels(BrainCancer_df$diagnosis)[levels(BrainCancer_df$diagnosis) %in% c("LG glioma", "HG glioma", "Other")] <- "LG-HG-Other"
fit_cox <- coxph(Surv(time, status) ~ diagnosis+ki+stereo+gtv+sex+loc, 
                 data = BrainCancer_df)
summary(fit_cox)
ggforest(fit_cox, data = as.data.frame(BrainCancer_df))  
ggcoxdiagnostics(fit_cox, type = "martingale", linear.predictions = TRUE)
ggcoxdiagnostics(fit_cox, type = "deviance", linear.predictions = TRUE)
diag_ph <- cox.zph(fit_cox)
diag_ph
ggcoxzph(diag_ph)

#not all variable are significant so we did a cox model just with ki and diagnosis grouped
install.packages("survival")
library(survival)
install.packages("survminer")
library(survminer)
levels(BrainCancer_df$diagnosis)[levels(BrainCancer_df$diagnosis) %in% c("LG glioma", "HG glioma", "Other")] <- "LG-HG-Other"
fit_cox <- coxph(Surv(time, status) ~ diagnosis+ki, 
                 data = BrainCancer_df)
summary(fit_cox)
ggforest(fit_cox, data = as.data.frame(BrainCancer_df))  
ggcoxdiagnostics(fit_cox, type = "martingale", linear.predictions = TRUE)
ggcoxdiagnostics(fit_cox, type = "deviance", linear.predictions = TRUE)
diag_ph <- cox.zph(fit_cox)
diag_ph
ggcoxzph(diag_ph)

#plot the partial cox model based on diagnosis
dia_df <- with(BrainCancer_df,
               data.frame(diagnosis = c("Meningioma","LGG/HGG/Other"),
                          ki = rep(mean(ki), 2))
)
fit.dia <- survfit(fit_cox, newdata = dia_df)
#TODO: make pretty
ggsurvplot(fit.dia, conf.int = TRUE, legend.labs=c("Meningioma", "LGG/HGG/Other"),
           ggtheme = theme_minimal(), data = brain_tibble_nona)

#-----------------------------------------------------
install.packages("randomForest")
install.packages("ISLR2")
library(randomForest)
train_data <- BrainCancer[!is.na(BrainCancer$diagnosis), ]
test_data <- BrainCancer[is.na(BrainCancer$diagnosis), ]
head(train_data)
head(test_data)

model <- randomForest(diagnosis ~ ., data = train_data)
print(model)
prediction <- predict(model, newdata = test_data)
print(prediction)
BrainCancer[is.na(BrainCancer$diagnosis), "diagnosis"] <- prediction
head(BrainCancer)
install.packages("caret")
library(caret)

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

cv_model <- train(diagnosis ~ ., data=train_data, method="rf", trControl=control, metric=metric)
print(cv_model)


addestramento <- BrainCancer_df[!is.na(BrainCancer_df$diagnosis), ]  # Escludi i pazienti senza diagnosi
set.seed(123)  # Imposta un seed per la riproducibilità
modello <- train(diagnosis ~ ., data = addestramento, method = "rf")

# Predizione della classe per i pazienti senza diagnosi
nuovi_dati <- BrainCancer_df[is.na(BrainCancer_df$diagnosis), ]  # Seleziona i pazienti senza diagnosi
predizione <- predict(modello, newdata = nuovi_dati)

# Stampare la predizione
print(predizione) #bias di selezione --> so we do not use it in the further analysis
#The selection bias may refer to the possibility that the trained model is influenced by 
#the composition of the training data, for instance, if the data has a significant number 
#of observations of a certain type of brain tumor compared to others. 
#This could lead the model to be more inclined to predict that type of tumor over others.
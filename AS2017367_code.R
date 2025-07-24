df <- readxl::read_xlsx("E://Uni//2nd sem//STA_471_GLM&NLM//Project//cleaned_data.xlsx")

View(df)


df <- as.data.frame(df)

summary(df)

df[,1:17] <- lapply(df[,1:17],factor)

summary(df)

attach(df)
df_1 <- as.data.frame(xtabs(~Region+Ethnicity+ Religion
                            +`Highest education qualification`+`Frequency of all media combined`
                            +`Working status`+`Welth index`+Y2, data = df))


df_2 <- subset.data.frame(df_1, df_1$Freq != 0)
View(df_2)

summary(df_2)
library(tidyr)

df_3 <- spread(df_2, key = Y2, value = Freq)

df_3[is.na(df_3)] <- 0

df_3$Total <- df_3$Right+df_3$Wrong
attach(df_3)
summary(df_3)

df_3 <- df_3[,-9]

summary(df_3)

full_model <- glm(cbind(Right, Total) ~Region+Ethnicity+Religion+Highest.education.qualification+Frequency.of.all.media.combined
                  +Working.status+Welth.index, family = binomial, data = df_3)

summary(full_model)


best_model <- step(glm(cbind(Right, Total) ~ 1,family = binomial(link = "logit"),data=df_3),
     test="LRT",scope=formula(model),direction = "forward")
summary(best_model)

best_model$anova

write.csv(df_3, "E:\\Uni\\2nd sem\\STA_471_GLM&NLM\\Project\\df.csv")

glm(cbind(Right, Total) ~ Ethnicity+Region+Frequency.of.all.media.combined
    , family = binomial, data = df_3)

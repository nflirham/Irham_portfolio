Data = read.csv('D:/Memento/Project/df_full_premierleague.csv')
library(glmm)

club_name    = NA
H_or_A       = NA
Win          = NA
Win_HT       = NA
Lawan        = NA
Shot_on_T    = NA
Possession   = NA
Passes       = NA
Tackel       = NA
Red_cards    = NA
Yellow_cards = NA


df = data.frame(club_name, H_or_A, Win, Win_HT, Lawan, Shot_on_T,
                Possession, Passes, Tackel, Red_cards, Yellow_cards)

for (i in unique(Data$home_team)){
  for (j in seq(1,length(Data$home_team))){
    if (i == Data$home_team[j]){
      Win          = (Data$goal_home_ft[j] > Data$goal_away_ft[j])*1
      Win_HT       = (Data$goal_home_ht[j] > Data$goal_away_ht[j])*1
      Shot_on_T    = Data$home_shots_on_target[j]
      Possession   = Data$home_possession[j]
      Passes       = Data$home_passes[j]
      Tackel       = Data$home_tackles[j]
      Red_cards    = Data$home_red_cards[j]
      Yellow_cards = Data$home_yellow_cards[j]
      
      new_row      = data.frame(i, "H", Win, Win_HT, Data$away_team[j], Shot_on_T,
                                Possession, Passes, Tackel, Red_cards, Yellow_cards)
      names(new_row) = c("club_name", "H_or_A", "Win", "Win_HT", "Lawan", "Shot_on_T",
                         "Possession", "Passes", "Tackel", "Red_cards", "Yellow_cards")
      df = rbind(df, new_row)
      
    } else if (i == Data$away_team[j]){
      Win          = (Data$goal_home_ft[j] <= Data$goal_away_ft[j])*1
      Win_HT       = (Data$goal_home_ht[j] <= Data$goal_away_ht[j])*1
      Shot_on_T    = Data$away_shots_on_target[j]
      Possession   = Data$away_possession[j]
      Passes       = Data$away_passes[j]
      Tackel       = Data$away_tackles[j]
      Red_cards    = Data$away_red_cards[j]
      Yellow_cards = Data$away_yellow_cards[j]
  
      new_row      = data.frame(i, "A", Win, Win_HT, Data$home_team[j], Shot_on_T,
                                Possession, Passes, Tackel, Red_cards, Yellow_cards)
      names(new_row) = c("club_name", "H_or_A", "Win", "Win_HT", "Lawan", "Shot_on_T",
                         "Possession", "Passes", "Tackel", "Red_cards", "Yellow_cards")
      df = rbind(df, new_row)
    }
  }
}

df=na.omit(df)

str(df)

df$club_name = as.factor(df$club_name)
df$H_or_A    = as.factor(df$H_or_A)
df$Win       = as.factor(df$Win)
df$Win_HT    = as.factor(df$Win_HT)
df$Lawan     = as.factor(df$Lawan)

str(df)
summary(df)

x = df$Win

barplot(prop.table(table(x)))

EPL <- glmm(df$Yellow_cards ~ 0 + df$Possession, 
            random = list(~ 0 + df$club_name), 
            varcomps.names = c("Club_name"), 
            data = df,
            family.glmm = bernoulli.glmm)


library(nlme) 
fm1 <- lme(Win ~ Win_HT+Red_cards, random = ~ 0 + Win_HT | club_name,
           data = df) 

model1=step(glm(Win ~ 
                  H_or_A+
                  Lawan+
                  Possession+
                  Red_cards+
                  Shot_on_T,
                family = binomial(link = "logit")
                ,data=df)
            ,direction = "both"
            ,trace = FALSE)

summary(model1)

confint(model1, level=0.95)

library(dplyr)
library(pROC)
library(caret)
library(Metrics) # RMSE/RMSLE
library(ggplot2)

#membangun ROC curve
pred1 = predict(model1,type="response")
roccurve1 = roc(df$Win~ pred1)

roclist=list("Model 1"=roccurve1)

ggroc(roclist,aes="colour",legacy.axes = TRUE) +
  geom_abline(intercept = 0,slope = 1) +
  labs(x = "1 - Specificity",
       y = "Sensitivity",
       colour = "Model")

auc(roccurve1)

anova(model1)

library(dgof) #ks.test
library(fitdistrplus) #MLE
library(actuar)
library(SuppDists)
library(EnvStats)

# menyusun tabel kontingensi dengan threshold yang menyeimbangkan sensitivitas dan spesifisitas
pred=factor(ifelse(model1$fitted.values < 0.4, 0, 1))
mat1=confusionMatrix(pred,df$Win)
mat1

# Interventions: Experiments, Uplift Models, and Reinforcement Learning
## Uplift (Persuasion) Modeling
### Computing Uplift with Python

library(uplift)
voter.df <- mlba::VoterPersuasion
# transform variable MOVED_AD to numerical
voter.df$MOVED_AD_NUM <- ifelse(voter.df$MOVED_AD == "Y", 1, 0)

set.seed(1)
train.index <- sample(c(1:dim(voter.df)[1]), dim(voter.df)[1]*0.6)
train.df <- voter.df[train.index, ]
valid.df <- voter.df[-train.index, ]

# use upliftRF to apply a Random Forest (alternatively use upliftKNN() to apply kNN).
up.fit <- upliftRF(MOVED_AD_NUM ~ AGE + NH_WHITE + COMM_PT + H_F1 + REG_DAYS+
                   PR_PELIG + E_PELIG + POLITICALC  + trt(MESSAGE_A),
                 data = train.df, mtry = 3, ntree = 100, split_method = "KL",
                 minsplit = 200, verbose = TRUE)
pred <- predict(up.fit, newdata = valid.df)
# first column: p(y | treatment)
# second column: p(y | control)
head(data.frame(pred, "uplift" = pred[,1] - pred[,2]))

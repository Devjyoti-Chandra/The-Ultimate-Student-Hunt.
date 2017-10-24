library(mlr)

# Can see learners using listLearner()
lrn = makeLearner("regr.xgboost")
train1=createDummyFeatures(train[,-c(1,8,10)], target = "Count", method = "1-of-n")
task = makeRegrTask(data = train1, target = "Count")

ps = makeParamSet(makeDiscreteParam("booster", values = c("gbtree" ,"gblinear")),
                  makeDiscreteParam("eta", values = c(0.05, 0.1, 0.3, 0.5)),
                  makeDiscreteParam("max_depth", values = c(6, 8, 10)),
                  makeDiscreteParam("colsample_bytree", values = c(0.6, 0.8, 1.0)),
                  makeDiscreteParam("colsample_bylevel", values = c(0.6, 0.8, 1.0)),
                  makeDiscreteParam("min_child_weight", values = c(0.6, 0.8, 1.0))
                  )
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 3L)
res = tuneParams("regr.xgboost", task = task, resampling = rdesc,
                 par.set = ps, control = ctrl)

param=list(booster="gblinear", eta=0.5, max_depth=8, colsample_bytree=1, colsample_bylevel=0.6, min_child_weight=0.8)

lrn=makeLearner("regr.xgboost", par.vals = param, fix.factors.prediction = TRUE)
mod=train(lrn, task)
model=getLearnerModel(mod)

colnames(test)[3]="Year.2013"
test1=createDummyFeatures(test[,-c(1,2,8)], method = "1-of-n", cols = c("Month", "Day", "Hour", "DayName"))
task.pred = predict(mod, newdata = test1)

test$count = getPredictionResponse(task.pred)





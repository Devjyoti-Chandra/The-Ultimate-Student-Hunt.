library(mlr)

lrn = makeLearner("regr.cvglmnet", par.vals = list(family="poisson", type.measure="mse"))
task = makeRegrTask(data = train, target = "Count")

ps = makeParamSet(makeDiscreteParam("alpha", values = c(0, 0.05 , 0.1, 0.3, 0.5, 0.8, 1.0)),
                 # makeDiscreteParam("s", values = c("lambda.1se", "lambda.min")),
                  makeDiscreteParam("nlambda", values = c(10, 50, 100, 1000, 10000)),
                #  makeDiscreteParam("penalty.factor", values = c(0.6, 0.8, 1.0)),
                  makeDiscreteParam("lambda.min.ratio", values = c(0.6, 0.8))
                  )
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 3L)
res = tuneParams("regr.cvglmnet", task = task, resampling = rdesc,
                 par.set = ps, control = ctrl)

lrn = makeLearner("regr.cvglmnet", par.vals = list(family="poisson", type.measure="mse", alpha=1, nlambda=50, lambda.min.ratio=0.6))
mod=train(lrn, task)
model=getLearnerModel(mod)
task.pred = predict(mod, newdata = test)

count = getPredictionResponse(task.pred)

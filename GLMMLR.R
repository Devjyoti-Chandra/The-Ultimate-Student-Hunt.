library(mlr)

lrn=makeLearner("regr.glm", par.vals = list(family="poisson"))
task = makeRegrTask(data = train[,-c(1,8,10)], target = "Count")

ps = makeParamSet(makeDiscreteParam("poisson.link", values = c("log" ,"identity", "sqrt")),
                  makeDiscreteParam("maxit", values = c(25, 50, 100, 200))
                  )
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 3L)
res = tuneParams("regr.glm", task = task, resampling = rdesc,
                 par.set = ps, control = ctrl)

lrn=makeLearner("regr.glm", par.vals = list(family="poisson", poisson.link="log", maxit=50))
mod=train(lrn, task)

task.pred=predict(mod, newdata = test)
test$Count=round(getPredictionResponse(task.pred))

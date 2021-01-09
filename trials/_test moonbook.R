
source("plugins/labels.R")

system.time(
  data.raw <- readRDS(dataset.label)
) # 0.75s

dataset
ggplot(dataset, aes(x = job_score)) + geom_density(aes(color = passed))
ggplot(dataset, aes(x = job_score)) + geom_density(aes(color = company_name))
ggplot(dataset, aes(x = job_score)) + geom_density(aes(color = job_type))

library(moonBook)

data(acs)
acs
mytable(Dx~.,data=acs)
mytable(passed ~  ., data = dataset) # not interesting
mytable(job_type ~  ., data = dataset) # wrong company_name!!
mytable(company_name ~  ., data = dataset) # publishable
out <- mytable(company_name ~  ., data = dataset)

mylatex(out)

# publishable: job_score CIs
out <- mytable(company_name ~ job_score + job_type + passed, 
               data = dataset, 
               method = 3) %>% print
myhtml(out) %>% cat(., file = "out.html")

# gglm::gglm(fit.glm)
fit.glm <- glm(job_score ~  conscientiousness +
                 agreeableness + extraversion +
                 openness + emotional_stability, data = dataset)

ORplot(fit.glm, type=1)
ORplot(fit.glm, type=2)
ORplot(fit.glm,type=2,show.CI=TRUE,
       main="Factors on Job Score")
# ORplot(fit.glm,type=4,show.CI=TRUE,sig.level=0.05)

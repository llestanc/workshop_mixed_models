
# Number of subjects per groups (2 groups, control/test)
N = 30
# number of trials per subject
ntrials = 20
# reaction time at T0 for both groups (ms)
int.T0 = 950
# Retest effect
slope.retest = 100
# Training effect
slope.training = 150
# variance among subjects at T0
sd = 50
# variance among retest and training effects is sd/2

rt.control.T0 = sort(rnorm(N, int.T0, sd))
rt.test.T0 = sort(rnorm(N, int.T0, sd))

slope.control.T1 = sort(rnorm(N, slope.retest, sd/2), decreasing = TRUE)
slope.test.T1 = sort(rnorm(N, slope.retest, sd/2), decreasing = TRUE)+sort(rnorm(N, slope.training, sd/2), decreasing = TRUE)

rt.control.T1 = rt.control.T0 - slope.control.T1
rt.test.T1 = rt.test.T0 - slope.test.T1

# trials = 10*sort(rpois(ntrials, 4))
trials = sort(rpois(ntrials, 4))

my_f <- function(x) {
  data.frame(trial1 = x + rnorm(1,10,2)*rpois(1, 4),
             trial2 = x + rnorm(1,10,2)*rpois(1, 4),
             trial3 = x + rnorm(1,10,2)*rpois(1, 4),
             trial4 = x + rnorm(1,10,2)*rpois(1, 4),
             trial5 = x + rnorm(1,10,2)*rpois(1, 4),
             trial6 = x + rnorm(1,10,2)*rpois(1, 4),
             trial7 = x + rnorm(1,10,2)*rpois(1, 4),
             trial8 = x + rnorm(1,10,2)*rpois(1, 4),
             trial9 = x + rnorm(1,10,2)*rpois(1, 4),
             trial10 = x + rnorm(1,10,2)*rpois(1, 4),
             trial11 = x + rnorm(1,10,2)*rpois(1, 4),
             trial12 = x + rnorm(1,10,2)*rpois(1, 4),
             trial13 = x + rnorm(1,10,2)*rpois(1, 4),
             trial14 = x + rnorm(1,10,2)*rpois(1, 4),
             trial15 = x + rnorm(1,10,2)*rpois(1, 4),
             trial16 = x + rnorm(1,10,2)*rpois(1, 4),
             trial17 = x + rnorm(1,10,2)*rpois(1, 4),
             trial18 = x + rnorm(1,10,2)*rpois(1, 4),
             trial19 = x + rnorm(1,10,2)*rpois(1, 4),
             trial20 = x + rnorm(1,10,2)*rpois(1, 4))
}


data = data.frame(
  rt.control.T0, 
  rt.control.T1 ,
  rt.test.T0, 
  rt.test.T1)%>%
  mutate(pax = str_pad(row_number(),3,"left","0"))%>%
  pivot_longer(!pax, names_to = "variable", values_to = "rt.mean")%>%
  mutate(my_f(rt.mean))%>%
  pivot_longer(starts_with("trial"), names_to = "trial", values_to = "rt")%>%
  select(-rt.mean)%>%
  separate(variable, into = c("drop", "group", "session"))%>%
  select(-drop)%>%
  mutate(pax = str_c("sub-", group, pax))%>%
  mutate(item = as.numeric(str_sub(trial, 6,-1)))%>%
  mutate(item = case_when(
    item > 15 ~ "item4",
    item > 10 ~ "item3",
    item > 5 ~ "item2",
    TRUE ~ "item1"
  ))%>%
  group_by(pax)%>%
  mutate(age = ifelse(group == "control", rnorm(1,8, 1.5), rnorm(1,15,1.5)))%>%
  ungroup()



write_csv(data, "./data/dataset_groupXsessionXage.csv")


x = 1:10
y = seq(75,145, length.out = 10)

data_ageheight = data.frame(
  age = x,
  height = jitter(y, 10)
)%>%
  mutate(age_group = relevel(as.factor(ifelse(age<5.5, "younger", "older")), ref = "younger"))

write_csv(data_ageheight, "./data/dataset_ageheight.csv")
  


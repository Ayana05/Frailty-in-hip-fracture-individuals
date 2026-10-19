#Missing Value Imputation (for frailty components)
library(mice)

#Generate m = 5 imputed datasets
imp <- mice(
  df4 %>% select(weak.grip, slow.gait, low.energy, low.activity, weight.loss),
  m = 5,
  method = "logreg",
  seed = 123
)

imp_complete1 <- complete(imp,1)

df4_imputed <- df4 %>%
  select(spid, round, row_num) %>%
  bind_cols(imp_complete1) %>%
  mutate(frailty_score_imputed = weak.grip + slow.gait + low.energy + low.activity + weight.loss)

#LCMM modeling
library(broom)
library(lcmm)
#m1
set.seed(123)
m1 <- hlme(
  fixed = frailty_score_imputed ~ row_num,
  subject = "spid",
  ng = 1,
  data = df4_imputed
)

#m2
m2 <- gridsearch(
  rep = 100,
  maxiter = 100,
  minit = m1,
  hlme(
    fixed = frailty_score_imputed ~ row_num,
    mixture = ~ row_num,
    subject = "spid",
    ng = 2,
    data = df4_imputed
  ))

#m3
m3 <- gridsearch(
  rep = 100,
  maxiter = 100,
  minit = m1,
  hlme(
    fixed = frailty_score_imputed ~ row_num,
    mixture = ~ row_num,
    subject = "spid",
    ng = 3,
    data = df4_imputed
  )
)

#m4
set.seed(123)
m4 <- gridsearch(
  rep = 100,
  maxiter = 100,
  minit = m1,
  hlme(
    fixed = frailty_score_imputed ~ row_num,
    mixture = ~ row_num,
    subject = "spid",
    ng = 4,
    data = df4_imputed))

#m5
set.seed(123)
m4 <- gridsearch(
  rep = 100,
  maxiter = 100,
  minit = m1,
  hlme(
    fixed = frailty_score_imputed ~ row_num,
    mixture = ~ row_num,
    subject = "spid",
    ng = 5,
    data = df4_imputed))

summary(m4)

class_data<-as.data.frame(m4$pprob) %>% select(spid,class)
df_class <- df4 %>%
  left_join(class_data, by = "spid") 
df_class %>% select(spid,frailty_score,row_num)
mean_frality<-df_class %>% group_by(class, row_num) %>% summarise(mean_fraility=mean(frailty_score,na.rm=T),.groups="drop")
mean_frality

df_class <- df_class %>%
  arrange(spid, round) %>%              # make sure rows are in chronological order within each person
  group_by(spid) %>%
  mutate(rel_round = row_number()) %>%  # 1 = fracture round, 2/3/4 = subsequent rounds, PER PERSON
  ungroup()

frailty_table <- df_class %>%
  filter(rel_round %in% 1:4) %>%
  group_by(class, rel_round) %>%
  summarise(mean_frailty = mean(frailty_score, na.rm = TRUE), .groups = "drop") %>%
  mutate(mean_frailty = round(mean_frailty, 2)) %>%
  pivot_wider(
    names_from = rel_round,
    values_from = mean_frailty,
    names_prefix = "Round "
  ) %>%
  rename(Group = class) %>%
  arrange(Group)

#Trjectory plot
##################
pred_m4 <- predictY(
  m4,
  newdata = data.frame(
    row_num = seq(
      min(df4$row_num, na.rm = TRUE),
      max(df4$row_num, na.rm = TRUE),
      length.out = 100
    )
  ),
  var.time = "row_num"
)

time_seq <- seq(
  min(df4$row_num, na.rm = TRUE),
  max(df4$row_num, na.rm = TRUE),
  length.out = 100
)

newdata <- data.frame(
  row_num = time_seq
)

pred_m4 <- predictY(
  m4,
  newdata = newdata,
  var.time = "row_num"
)

layout(matrix(c(1, 2), nrow = 2), heights = c(4, 1))

# --- Panel 1: the plot ---
par(mar = c(4, 4, 3, 2))
plot(
  pred_m4,
  legend = NULL,
  xlab = "Post Hip fracture rounds",
  ylab = "Frailty Score (0 - 5)",
  main = "Four-Class LCMM Trajectories",
  lwd = 2,
  ylim = c(0, 5),
  yaxt = "n"
)
axis(2, at = seq(0, 5, by = 1), las = 1)

# --- Panel 2: legend ---
par(mar = c(0, 0, 0, 0))
plot.new()
legend(
  "center",
  legend = c(
    "Class 1: High Frailty",
    "Class 2: Low Frailty",
    "Class 3: Moderate & Increasing Frailty",
    "Class 4: High & Declining Frailty"
  ),
  #legend = c(
  # "Class 1: High Frality",
  #"Class 2: Moderate Frailty",
  #"Class 3: Low Frailty"
  #),
  col = c("#000000","#D9536F","#4DAF4A","lightblue"),
  lty = c(1, 2, 3, 4),
  lwd = 2,
  ncol = 2,
  bty = "n",
  cex = 0.7          # smaller legend text (was 0.9) — try 0.6–0.8 to taste
)

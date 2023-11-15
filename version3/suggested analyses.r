data <- haven::read_dta("version2/gpa_analysis_file-2.dta")

data = data |> filter(samp_overall_gpa_hs_covars == 1)

colnames(data)

data = data %>% 
  select(student_id, remote_spring2021, bl_school_id,  matches("core_gpa_non")) %>% 
  pivot_longer(cols = -c(student_id, remote_spring2021, bl_school_id), names_to = "quarter", values_to = "gpa") %>% 
  separate(quarter, into = c("quarter", "year"), sep = "_") %>%
  mutate(quarter = parse_number(quarter)) %>% 
  mutate(year = parse_number(year)) %>% 
  mutate(period = (year*4 + quarter)-76)

data = data %>% 
  mutate(remote_t = ifelse(period %in% 4:8, 1, 0),
  period_dummies = factor(period))

data = data %>% 
  # make period 4 be the reference category
  mutate(period_dummies = fct_relevel(period_dummies, "4"))

library(plm)
model <- plm(gpa ~ remote_t + remote_t:remote_spring2021 + factor(bl_school_id), data = data, index = c("student_id", "period"), model = "within")
model_event <- plm(gpa ~ remote_spring2021*period_dummies, data = data, index = c("student_id", "period"), model = "within")




model %>% broom::tidy(conf.int = TRUE)
model_event %>% broom::tidy(conf.int = TRUE)
 
model %>% summary()
model_event %>% summary()

stargazer::stargazer(model, model_event, type = "text")

model_event %>%
  broom::tidy(conf.int = TRUE) %>% 
  mutate(
    term_type = ifelse(str_detect(term, "remote_spring2021"), "interaction", "fixed"),
    term = str_remove(term, "remote_spring2021:"),
    period = parse_number(term),
   ) %>% 
  select(period, term = term_type, estimate, conf.high, conf.low) %>% 
  pivot_wider(names_from = term, values_from = c(estimate, conf.low, conf.high)) %>% 
  rowwise() %>%
  mutate(remote = fixed+interaction) %>% 
  # add_row(period = 4, fixed = 0, interaction = 0, remote = 0) %>% 
  ggplot(aes(period, remote)) +
  geom_line() +
  geom_point() + 
  geom_line(aes(period, fixed), color = "red")+
  geom_point(aes(period, fixed), color = "red")+
  scale_x_continuous(breaks = 1:12)+ 
  geom_hline(yintercept = 0, color = "gray", linetype = 2)+ 
  geom_vline(xintercept = c(4.5,8.5), color = "gray", linetype = 2)+ 
  labs(x = "Quarter", y = "GPA (Difference from baseline)")

model_event %>%
  broom::tidy(conf.int = TRUE) %>% 
  mutate(
    term_type = ifelse(str_detect(term, "remote_spring2021"), "interaction", "fixed"),
    term = str_remove(term, "remote_spring2021:"),
    period = parse_number(term),
   ) %>% 
  select(period, term = term_type, estimate, conf.high, conf.low) %>% 
  filter(term == "interaction") %>%
  add_row(period = 4, term = "interaction", estimate = 0, conf.high = 0 , conf.low = 0) %>% 
  ggplot(aes(period, estimate)) +
  geom_ribbon(aes(ymin = conf.high, ymax = conf.high), alpha = 0.42, fill = "red")+
  geom_line() +
  geom_point()+ 
  scale_x_continuous(breaks = 1:12)+ 
  geom_hline(yintercept = 0, color = "gray", linetype = 2)+ 
  geom_vline(xintercept = c(4.5,8.5), color = "gray", linetype = 2)+ 
  labs(x = "Quarter", y = "GPA\n(Difference between remote and\nin-person students)")

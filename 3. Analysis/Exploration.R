library(haven)
library(tidyverse)

df <- read_dta("Google Drive/My Drive/0 | Research/2. Return from Covid/TI_S21_F21_W22_analysis_file_wgt.dta")
variables <- readxl::read_excel("Google Drive/My Drive/0 | Research/2. Return from Covid/TI_main_results_key_variables.xlsx")
variables |> gather() |> pull(value) |> na.omit()
df |> select(variables |> gather() |> pull(value) |> na.omit()) |> colnames()
df |> colnames()

df |> select(in_F21_svy)

df |> select(S21_pref_inperson_all_sp21,
             S21_pref_remote_all_sp21,
             S21_pref_hybrid_all_sp21,
             S21_pref_inperson_inperson_sp21,
             S21_pref_remote_inperson_sp21,
             S21_pref_hybrid_inperson_sp21,
             S21_pref_inperson_remote_sp21,
             S21_pref_remote_remote_sp21,
             S21_pref_hybrid_remote_sp21,
             in_S21_svy,
             remote_spring2021,
             in_person_spring2021)

df |> select(
             remote_spring2021,
             in_person_spring2021)

wellbeing <- df |> 
  select(student_id,
         remote_t0 = remote_spring2021,
         remote_t1 = remote_spring2021,
         remote_t2 = remote_spring2021,
         social_t0 = S21_social_01miss,
         emotional_t0 = S21_emotional_01miss,
         academic_t0 = S21_academic_01miss,
         overallgpa_t0 = SIS_S21_overall_gpa_non,
         coregpa_t0 = SIS_S21_core_gpa_non,
         completed_t1 = in_F21_svy,
         social_t1 = F21_social_01miss,
         emotional_t1 = F21_emotional_01miss,
         academic_t1 = F21_academic_01miss,
         overallgpa_t1 = SIS_F21_overall_gpa_non,
         coregpa_t1 = SIS_F21_core_gpa_non,
         completed_t2 = in_W22_svy,
         social_t2 = W22_social_01miss,
         emotional_t2 = W22_emotional_01miss,
         academic_t2 = W22_academic_01miss,
         overallgpa_t2 = SIS_W22_overall_gpa_non,
         coregpa_t2 = SIS_W22_core_gpa_non) |> 
  pivot_longer(cols = c(everything(), -student_id)) |> 
  separate(name, c('name', 'time'),"_") |> 
  spread(name,value) 


lms = wellbeing |> 
  mutate(remote = ifelse(remote == 0, "Always in person", "Returned to school from virtual learning")) |> 
  select(time, remote, academic, emotional, social,coregpa) |> 
  pivot_longer(academic:coregpa) |> 
  group_by(time,name) |> 
  nest() |> 
  mutate(lm = map(data, ~lm(value~1+remote, data = .)))

lms = wellbeing |> 
  mutate(remote = ifelse(remote == 0, "Always in person", "Returned to school from virtual learning")) |> 
  select(time, remote, academic, emotional, social,coregpa) |> 
  pivot_longer(academic:coregpa) |> 
  filter(time != "t2") |> 
  spread(time,value)
  group_by(time,name) |> 
  nest() |> 
  mutate(lm = map(data, ~lm(value~1+remote, data = .)))

lms |>
  mutate(ems = map(lm,~emmeans::emmeans(.,"remote") |> as_tibble())) |> 
  unnest(ems) |> 
  ungroup() |> 
  mutate(name = case_when(
    name == "academic" ~ "Academic Well-being",
    name == 'coregpa' ~ "Core GPA",
    name == "emotional" ~ "Emotional Well-Being",
    name == "social" ~ "Social Well-Being",
  ),
  name= fct_inorder(name)) |> 
  ggplot(aes(time, emmean, color = name, fill = name, lty = remote, group = paste(name, remote)))+
  geom_line()+
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), color = NA, alpha = .3)+
  egg::theme_article()+
  scale_color_viridis_d(guide = "none")+
  scale_fill_viridis_d(guide = "none")+
  facet_wrap(~name, scales =  "free", nrow = 1)+
  theme(legend.position = "bottom")+
  geom_vline(xintercept = 1.5, lty = 3, size = .5)+
  labs(y = "Mean [95% CI]", x = "Time", lty = "Group")
  
wellbeing |> 
  mutate(remote = ifelse(remote == 0, "Always in person", "Returned to school from virtual learning")) |> 
  select(time, remote, academic, emotional, social,coregpa) |> 
  pivot_longer(academic:coregpa) |> 
  group_by(time,name,remote) |> 
  nest() |> 
  mutate(lm = map(data, ~lm(value~1, data = .)),
         lm = map(lm, broom::tidy, conf.int = T)) |> 
  unnest(lm) |> 
  ungroup() |> 
  mutate(name = case_when(
    name == "academic" ~ "Academic Well-being",
    name == 'coregpa' ~ "Core GPA",
    name == "emotional" ~ "Emotional Well-Being",
    name == "social" ~ "Social Well-Being",
  ),
  name= fct_inorder(name)) |> 
  ggplot(aes(time, estimate, color = name, fill = name, lty = remote, group = paste(name, remote)))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = NA, alpha = .3)+
  egg::theme_article()+
  scale_color_viridis_d(guide = "none")+
  scale_fill_viridis_d(guide = "none")+
  facet_wrap(~name, scales =  "free", nrow = 1)+
  theme(legend.position = "bottom")+
  geom_vline(xintercept = 1.5, lty = 3, size = .5)+
  labs(y = "Mean [95% CI]", x = "Time", lty = "Group")

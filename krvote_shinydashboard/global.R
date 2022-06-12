# library(devtools)
# install_github("ai-carpentry/krvote")

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
# library(krvote)
library(tidyverse)
library(here)

sido_name_v <-
  read_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sido_name_v.rds"))

# 
# sido_name_v <- krvote::election_20220309$투표율 %>%
#   distinct(시도명) %>%
#   pull(시도명)
# 
# sido_name_v %>%
#   write_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sido_name_v.rds"))
# 
# # 1. 구시군별 투표율 ---------------------
# 
# sido_casting <- krvote::election_20220309$투표율  %>%
#   filter(시도명 == "서울특별시") %>%
#   group_by(시도명, 구시군명) %>%
#   summarise(선거인수 = sum(선거인수),
#             투표수   = sum(투표수)) %>%
#   mutate(투표율 = 투표수 / 선거인수) %>%
#   ungroup()
# 
# sido_casting %>%
#   write_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sido_casting.rds"))
# 
# # 2. 정당별 득표수 ---------------------
# sido_party <- krvote::election_20220309$득표율  %>%
#   filter(시도명 == "서울특별시") %>%
#   pivot_longer(cols = 더불어민주당이재명:계) %>%
#   mutate(정당 = case_when(str_detect(name, "더불어민주당") ~"민주당",
#                           str_detect(name, "국민의힘") ~"국민의힘",
#                           str_detect(name, "계") ~"계",
#                           TRUE ~ "그외정당")) %>%
#   group_by(시도명, 구시군명, 정당) %>%
#   summarise(득표 = sum(value)) %>%
#   ungroup() %>%
#   pivot_wider(names_from = 정당, values_from = 득표) %>%
#   select(시도명, 구시군명, 민주당, 국민의힘, 그외정당, 계)
# 
# sido_party %>%
#   write_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sido_party.rds"))
# 
# 
# # 3. 정당별 득표율 ---------------------
# sido_party_rate <- krvote::election_20220309$득표율  %>%
#   filter(시도명 == "서울특별시") %>%
#   pivot_longer(cols = 더불어민주당이재명:계) %>%
#   mutate(정당 = case_when(str_detect(name, "더불어민주당") ~"민주당",
#                         str_detect(name, "국민의힘") ~"국민의힘",
#                         str_detect(name, "계") ~"계",
#                         TRUE ~ "그외정당")) %>%
#   group_by(시도명, 구시군명, 정당) %>%
#   summarise(득표 = sum(value)) %>%
#   ungroup() %>%
#   pivot_wider(names_from = 정당, values_from = 득표) %>%
#   mutate(민주당 = 민주당/계,
#          국민의힘 = 국민의힘/계,
#          그외정당 = 그외정당/계,
#          계 = 민주당 + 국민의힘 + 그외정당) %>%
#   select(시도명, 구시군명, 민주당, 국민의힘, 그외정당, 계)
# 
# sido_party_rate %>%
#   write_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sido_party_rate"))
# 
# # 4. 지도 -----------------------------
# 
# 
# 

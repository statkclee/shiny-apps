# library(devtools)
# install_github("ai-carpentry/krvote")

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
# library(krvote)
library(tidyverse)
library(here)
library(sf)
library(showtext)
font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()

sido_name_v <-
  readRDS(url("https://github.com/statkclee/shiny-apps/blob/main/krvote_shinydashboard/www/sido_name_v.rds?raw=true", "rb"))

sido_casting <- 
  readRDS(url("https://github.com/statkclee/shiny-apps/blob/main/krvote_shinydashboard/www/sido_casting.rds?raw=true", "rb"))
sgg_casting_sf <- 
  readRDS(url("https://github.com/statkclee/shiny-apps/blob/main/krvote_shinydashboard/www/sgg_casting_sf.rds?raw=true", "rb"))

sido_party <-
  readRDS(url("https://github.com/statkclee/shiny-apps/blob/main/krvote_shinydashboard/www/sido_party.rds?raw=true"))
sgg_party_sf <- 
  readRDS(url("https://github.com/statkclee/shiny-apps/blob/main/krvote_shinydashboard/www/sgg_party_sf.rds?raw=true"))

sido_party_rate <- 
  readRDS(url("https://github.com/statkclee/shiny-apps/blob/main/krvote_shinydashboard/www/sido_party_rate.rds?raw=true"))

sgg_party_rate_sf <- 
  readRDS(url("https://github.com/statkclee/shiny-apps/blob/main/krvote_shinydashboard/www/sgg_party_rate_sf.rds?raw=true"))  

# 
# sido_name_v <- krvote::election_20220309$투표율 %>%
#   distinct(시도명) %>%
#   pull(시도명)
# 
# sido_name_v %>%
#   write_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sido_name_v.rds"))
# 
# 1. 구시군별 투표율 ---------------------
# 
# sido_casting <- krvote::election_20220309$투표율  %>%
#   # filter(시도명 == "서울특별시") %>%
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
#   # filter(시도명 == "서울특별시") %>%
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
#   # filter(시도명 == "서울특별시") %>%
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
#   write_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sido_party_rate.rds"))
# 
# 4. 지도 -----------------------------
# 
# adm_map <- st_read("assets/HangJeongDong_ver20220401.geojson")
# 
# sf::sf_use_s2(TRUE)
# 
# adm_sgg_map <- adm_map %>%
#   st_make_valid() %>%
#   group_by(sidonm, sggnm) %>%
#   summarise(geometry = st_union(geometry))
# 
# ## 4.1. 투표율 -----------------------------
# sgg_casting_sf <- adm_sgg_map %>%
#   ungroup()  %>%
#   left_join(sido_casting, by = c("sggnm" = "구시군명",
#                                  "sidonm" = "시도명")) %>%
#   rename(시도명 = sidonm,
#          구시군명 = sggnm)
# 
# sgg_casting_sf %>%
#   write_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sgg_casting_sf.rds"))
# 
# sgg_casting_sf %>%
#   filter(시도명 == "서울특별시") %>%
#   ggplot() +
#     geom_sf(aes(geometry = geometry, fill = 투표율)) +
#     theme_void() +
#     scale_fill_viridis_c(option = "plasma", begin = 0.0) +
#     geom_sf_text(aes(label = glue::glue("{구시군명}\n{scales::percent(투표율, accuracy=0.1)}")),
#                  size = 3)
# 
# 
# ## 4.2. 득표수 -----------------------------
# 
# sgg_party_sf <- adm_sgg_map %>%
#   ungroup()  %>%
#   left_join(sido_party, by = c("sggnm" = "구시군명",
#                                  "sidonm" = "시도명")) %>%
#   rename(시도명 = sidonm,
#          구시군명 = sggnm) %>% 
#   mutate(표차이 = 민주당 - 국민의힘)
# 
# sgg_party_sf %>%
#   write_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sgg_party_sf.rds"))
# 
# sgg_party_sf %>%
#   filter(시도명 == "서울특별시") %>%
#   ggplot() +
#   geom_sf(aes(geometry = geometry, fill = 표차이)) +
#   theme_void() +
#   scale_fill_gradient2(low='red', mid = "white", high='blue') +
#   geom_sf_text(aes(label = glue::glue("{구시군명}\n", 
#                                       "민주당: {scales::comma(민주당, accuracy=1)}\n",
#                                       "국민의힘: {scales::comma(국민의힘, accuracy=1)}\n",
#                                       "그외정당: {scales::comma(그외정당, accuracy=1)}")),
#                size = 3)
# 
## 4.3. 득표율 -----------------------------
# 
# sgg_party_rate_sf <- adm_sgg_map %>%
#   ungroup()  %>%
#   left_join(sido_party_rate, by = c("sggnm" = "구시군명",
#                                "sidonm" = "시도명")) %>%
#   rename(시도명 = sidonm,
#             구시군명 = sggnm) %>%
#   mutate(표차이 = 민주당 - 국민의힘)
# 
# sgg_party_rate_sf %>%
#   write_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sgg_party_rate_sf.rds"))
# 
# sgg_party_rate_sf %>%
#   filter(시도명 == "서울특별시") %>%
#   ggplot() +
#   geom_sf(aes(geometry = geometry, fill = 표차이)) +
#   theme_void() +
#   scale_fill_gradient2(low='red', mid = "white", high='blue',
#                        labels = scales::percent) +
#   geom_sf_text(aes(label = glue::glue("{구시군명} > ",
#                                       "민주당: {scales::percent(민주당, accuracy=0.1)}\n",
#                                       "국민의힘: {scales::percent(국민의힘, accuracy=0.1)}\n",
#                                       "그외정당: {scales::percent(그외정당, accuracy=0.1)}")),
#                size = 4)


# Setup: Trade costs depend only on the interventions of one country. 
# The NTMs implemented by one country only depends on the sqrt(GDP) of the country. 
# There is only a limited amount of information know about that country. 

controls <- readRDS(file = paste0(path.data.out, "Controls cleaned CEPII grid.RData"))
controls <- controls%>%
  filter(year == "2019")%>%
  select(gdp_d)%>%
  unique()
hist(controls$gdp_d, breaks = 100)
hist(log(controls$gdp_d), breaks = 100)



CRI = 0.01156533233
beta <- 0.1
n = 1000
a <- 0.9

data <- data.frame(id = 1:n,
                   GDPs = exp(rnorm(n, 18, 2)))%>% # draw GDPs
  mutate(NTMs = (CRI * sqrt(GDPs))/a)%>%         # Calculate "real" NTMs
  mutate(ti = beta*NTMs + rnorm(n, 200, 5))%>%
  mutate(NTMS_discovered_prop = 0.5 * NTMs)

cutoff <- data$GDPs[order(data$GDP)][n*0.7]

data <- data %>%
  mutate(share_disc = ifelse(GDPs > cutoff, 0.8,0)) %>%
  #mutate(share_disc = GDPs*runif(n)*0.0000000003) %>%
  #mutate(share_disc = ifelse(share_disc > 1, 1, share_disc))%>%
  mutate(NTMS_discovered = NTMs * share_disc)%>%
  mutate(CRI = (NTMS_discovered/sqrt(GDPs)))

#hist(data$share_disc, breaks = 100)
# hist(data$GDPs, breaks = 100)
# hist(data$NTMs, breaks = 100)

#data <- data %>% pivot_longer(cols = c("NTMs" ,"NTMS_discovered_prop", "NTMS_discovered", "CRI"), values_to = "NTMs", names_to = "Category")



ggplot(data, aes(x = NTMs, y = ti))+
  geom_point()+
  geom_smooth(aes(x = NTMS_discovered, color = "NTMS_discovered"), method = "lm")+  
  geom_smooth(aes(x = NTMS_discovered, color = "NTMS_discovered_w", weight=CRI), method = "lm")+
  geom_smooth(aes(x = NTMs, color = "True"), method = "lm")

rt <- lm(data = data, ti ~NTMS_discovered, weights = CRI); summary(rt)
rt <- lm(data = data, ti ~NTMS_discovered); summary(rt)

# V2. --------------------------------------------------------------------------
GTA.coverage_2 <- readxl::read_xlsx(path = paste0(path.data.out, "GTA_Measurement_index.xlsx"))
GTA.total <- readRDS(file = paste0(path.data.reg, "GTA_final_mc.Rds"))
GTA.total$CRI <- NA
GTA.total <- GTA.total %>% 
  left_join(GTA.coverage_2, by = c("country.1", "country.2", "chapter", "year"))

b_0 <- 500
b_1 <- 0.05 # measures 

#get error term
sim <- GTA.total %>%
  select(country.1, country.2, year, chapter, int, interventions.revealed, CRI_sqrt_gm, gdp_d, gdp_o)


sim <- sim %>%
  mutate(tij = b_0+b_1*int+ rnorm(nrow(.), 100, 30))%>%
  mutate(tij = ifelse(tij < 0, 0, tij))


ggplot(sim, aes(x = interventions.revealed, y = tij))+
  geom_point()+
  geom_smooth(aes(x = interventions.revealed, color = "NTMS_discovered"), method = "lm")+  
  geom_smooth(aes(x = interventions.revealed, color = "NTMS_discovered_w", weight=CRI_sqrt_gm), method = "lm")+
  geom_smooth(aes(x = int, color = "True"), method = "lm")



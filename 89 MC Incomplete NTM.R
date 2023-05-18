
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
  geom_smooth(aes(x = int, color = "True"), method = "lm")+
  theme_minimal()

sim.red <- sim %>%
  filter(gdp_d > 1000000000 & gdp_o > 1000000000)

ggplot(sim.red, aes(x = interventions.revealed, y = tij))+
  geom_point()+
  geom_smooth(aes(x = interventions.revealed, color = "NTMS_discovered"), method = "lm")+  
  geom_smooth(aes(x = interventions.revealed, color = "NTMS_discovered_w", weight=CRI_sqrt_gm), method = "lm")+
  geom_smooth(aes(x = int, color = "True"), method = "lm")+
  theme_minimal()


# V2. --------------------------------------------------------------------------


n = 1000
data <- data.frame("id" = 1:n,
                   "NTM" = 1:n)
data <- data %>%
  mutate(tij = NTM * 1)%>%
  mutate(NTM_prop = NTM * 0.5) %>%
  mutate(NTM_overprop = round(NTM * (NTM/n)))%>%
  mutate(NTM_underprop = round(NTM * (1/NTM)))%>%
  pivot_longer(cols = c(2,4,5,6), values_to = "NTMs", names_to = "Variable")

p1 <- ggplot(data, aes(x = NTMs, y = tij, color = Variable))+
  geom_point()+
  theme_minimal()+
  theme(title = element_text("Distributions of knowledge about NTMs"), legend.position = "None")+
  ylab("Trade Costs")+
  xlab("Number of NTMs implemented")+
  scale_color_manual(values = c(standard.colors), 
                     name = "Knowledge about NTMs",
                     labels = c("All known", "Overproportional to number implemented", "Proportional to number implemented", "Underproportional to number implemented"))

removed <- round(rnorm(10000, 1000, 500))
data2 <- data %>%
  filter(!id >1000)%>%
  filter(!id %in% removed)%>%
  filter(Variable %in% c("NTM", "NTM_overprop" ))%>%
  pivot_wider(id_cols = 1:2, names_from = "Variable", values_from = "NTMs")
  
  
p2 <- ggplot(data2, aes(y = tij))+
  geom_point(aes(x = NTM), color = standard.colors[1])+
  geom_point(aes(NTM_overprop), color = standard.colors[2])+
  geom_smooth(aes(x = NTM_overprop), method = "lm", color = standard.colors[2])+  
  geom_smooth(aes(x = NTM_overprop, weight = NTM), method = "lm", color = standard.colors[3])+  
  theme_minimal()+
  theme(title = element_text("Distributions of knowledge about NTMs"))+
  ylab("Trade Costs")+
  xlab("Number of NTMs implemented")+
  scale_color_manual(name = "Knowledge about NTMs",
                     labels = c("All known", "Overproportional to number implemented", "Ha"),
                     guide= F)
p2



removed <- round(rnorm(5000, 0, 500))
data3 <- data %>%
  filter(!id <0)%>%
  filter(!id %in% removed)%>%
  filter(Variable %in% c("NTM", "NTM_overprop" ))%>%
  pivot_wider(id_cols = 1:2, names_from = "Variable", values_from = "NTMs")


p3 <- ggplot(data3, aes(y = tij))+
  geom_point(aes(x = NTM), color = standard.colors[1])+
  geom_point(aes(NTM_overprop), color = standard.colors[2])+
  geom_smooth(aes(x = NTM_overprop), method = "lm", color = standard.colors[2])+  
  geom_smooth(aes(x = NTM_overprop, weight = NTM), method = "lm", color = standard.colors[3])+  
  theme_minimal()+
  theme(title = element_text("Distributions of knowledge about NTMs"))+
  ylab("Trade Costs")+
  xlab("Number of NTMs implemented")+
  scale_color_manual(name = "Knowledge about NTMs",
                     labels = c("All known", "Overproportional to number implemented", "Ha"))
p3

library(cowplot)
plot_grid(p1,p2,p3, rows = 3)
gta_plot_saver(plot = p_out, 
               path = path.plot, 
               name = "MC_Unknown_NTM", 
               png = T)

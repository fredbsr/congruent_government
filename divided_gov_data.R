library(tidyverse)
library(readxl)
library(ggthemes)
library(foreign)

setwd("/Volumes/Macintosh HD/Dropbox/IPG_Projetos_PMDB/Dados")


# Ideology ----
#### reading and transforming data from Andy Baker ####
ideology_original <-  read_excel("Andy Baker ideology Data/data.xlsx") %>%
  mutate(country=tolower(Country),partycode=tolower(`Party Code`),party=tolower(Party),
         year=as.numeric(as.character(Year)),
         ideo = ((`Ideology Score`-1)*0.5)+1) %>% # transforming 20 - 1 scale in 1 - 10
  dplyr::select(country,year,party,partycode,ideo) %>%
  mutate(party=stringi::stri_trans_general(party, "latin-ascii")) %>%
  distinct() %>%
  dplyr::filter(!is.na(ideo),!is.na(party))


# select(-del,-nat,-rurl,-reg,-rel,-age) 
ideology_to_DPI <-  read_excel("ideology_to_DPI.xlsx") %>%
  dplyr::filter(!is.na(party),!is.na(partycode)) %>%
  mutate(partycode_DPI=partycode) %>%
  dplyr::select(-partycode)

ideology <- ideology_original %>%
  left_join(ideology_to_DPI) %>%
  mutate(partycode=ifelse(is.na(ifs)==T,partycode,partycode_DPI)) %>%
  dplyr::select(-partycode_DPI,-ifs)

basedates_ideology <- expand.grid(year=c(1975:2015),
                                  ifs=unique(paste0(ideology$country,"@",
                                                   ideology$partycode))) %>%
  separate(ifs,c("country","partycode"),sep="@") 

exp.ideology <- left_join(basedates_ideology,ideology) %>%
  group_by(country) %>%
  fill(4:ncol(.)) 

summary(as.factor(ideology_original$partycode))
summary(as.factor(ideology$partycode))

# Reading DPI data and tyding
#DPI <- read_excel("DPI/DPI2015/DPI2015_basefile.v5.xlsx")
#names(DPI)
DPI <- haven::read_dta("DPI/DPI2015/DPI2015_basefile.dta")

#Gathering
DPI_gather <- DPI %>%
  gather(var,value,24:50,54:68) %>%
  separate(var, into=c("del","key"), sep = 4) %>%
  spread(key='key',value='value',convert=F) %>%
  filter(seat>0) %>%
  mutate(coal = ifelse(substr(del,1,3)=="gov",1,0),country=tolower(countryname),partycode=tolower(me),
         ifs=tolower(ifs),
         year=as.numeric(as.character(year)),
         pres=ifelse(tolower(execme)==partycode,1,0)) %>%
  dplyr::select(country,ifs,year,partycode,seat,pres,coal) 



# making some data for training - our data must have this structure ----
## party = party name (acronim)
## ideology = party ideology for a given country-year
## size = number of seats a party holds in congress for a givencountry-year
## coal = dummy indicating if a party is part of the coverning coalition (1=yes)
## pres = dummy indicating if a party holds the presidency (1=yes)
## year = year
## country = country

summary(left_join(DPI_gather,ideology)$ideo)
summary(left_join(DPI_gather,ideology_original)$ideo)


df_parties_long <- 
      #left_join(DPI_gather,exp.ideology) %>%
      left_join(DPI_gather,ideology) %>%
#  filter(is.na(ideo)==F) %>%
  transmute(party = partycode,
            ideology = ideo,
            size = as.numeric(seat),
            coal = coal,
            pres = pres,
            year = as.numeric(year),
            country = country,ifs=ifs) %>%
  mutate(coal.ideo = ideology*coal/coal, #interm. coalition ideology - NaN for non coalition
       pres.ideo = ideology*pres/pres, #interm. president ideology - NaN for non president
       cong.ideo = ideology*size,      #interm. congress ideology
       coal.size = size*coal,          #interm. coalition size
       pres.size = size*pres)
# using dplyr summarise to create my final DF
summary(df_parties_long$ideology)



df_parties <- df_parties_long %>%  
  group_by(country,ifs,year,add=T) %>%
  summarise(
    coal.ideo = mean(coal.ideo, na.rm = TRUE), #coalition ideology 
    pres.ideo = mean(pres.ideo, na.rm = TRUE), #president ideology
    cong.size = sum(size,na.rm = T), #congress size
    cong.ideo = sum(cong.ideo, na.rm = TRUE)/cong.size,#congress ideology
    coal.size = sum(coal.size,na.rm = T), #coalition size
    pres.size = sum(pres.size,na.rm = T) #president size 
    ) %>%
    mutate(div.gov = coal.ideo - cong.ideo, #divided government
           p.div.gov = pres.ideo - cong.ideo, #potential divided government
           effort = abs(p.div.gov)-abs(div.gov)) %>% #president effort to avoid div.gov.
  dplyr::filter(is.na(ifs)==F)

# Organizing data by country
df_countries <- DPI %>%
  mutate_each(funs(replace(.,which(.<0L), 0))) %>%
  mutate(gov_parties=(gov1seat>0)+(gov2seat>0)+(gov3seat>0)+govoth,
         opp_parties=(opp1seat>0)+(opp2seat>0)+(opp3seat>0)+ifelse(oppoth>0,oppoth,0),
         other_parties=ulprty,
         totparties = gov_parties+opp_parties+ulprty,
         govseats=gov1seat+gov2seat+gov3seat+govothst,
         oppseats=opp1seat+opp2seat+opp3seat+oppoth,
         other_parties_seats=numul,
         totalseats=govseats+oppseats+other_parties_seats,
         maj=round(100*govseats/totalseats,2),
         relevparties=(gov1seat>0)+(gov2seat>0)+(gov3seat>0)+(opp1seat>0)+(opp2seat>0)+(opp3seat>0),
         ifs=tolower(ifs),
         year=as.numeric(as.character(year))) %>%
  dplyr::select(ifs,
                year,
                system,
                allhouse,
                totalseats,
                govseats,
                oppseats,
                other_parties_seats,
                totparties,
                gov_parties,
                opp_parties,
                maj,
                mdms,
                pr,
                housesys,
                select,
                fraud,
                liec,
                eiec) %>%
  dplyr::filter(totalseats>0,ifs!="0")

summary(df_parties$div.gov)

# Election indices from PDF
#txt <- pdf_text("http://www.tcd.ie/Political_Science/staff/michael_gallagher/ElSystems/Docts/ElectionIndices.pdf")
#txt <- txt[c(4:36)]
#writeLines(txt)
#txt <- as.data.frame(cat(txt))
#devtools::install_github("ropenscilabs/tabulizer")

# Bringing in country-specific variables present in DPI dataset
df <- left_join(df_parties,df_countries)

# Expanding df for all years and moving one year forward (because the term starts only after elections)
base <- expand.grid(year=c(1990:2015),ifs=unique(df$ifs))

exp.df <- left_join(base,df) %>%
  group_by(ifs) %>%
  fill(3:ncol(.)) %>%
  mutate(year=year+1)


# Selecting Latin America only
exp.df.la <- exp.df %>%
  left_join(data_frame(country=unique(ideology$country),al=1)) %>%
  dplyr::filter(al==1)
summary(exp.df.la$div.gov)

# Saving image ----
#save.image("div_gov.RData")
save(exp.df.la,file="exp_div_gov.RData")

# Export data for synch
ideo_corrigir <- ideology %>%
  dplyr::select(country,party,partycode) %>%
  unique()

DPI_corrigir <- left_join((DPI_gather %>% 
                            dplyr::select(country,ifs,partycode) %>%
                            unique()),
                          (ideology %>%
                            mutate(al=1) %>%
                            dplyr::select(country,al) %>%
                            unique())) %>%
  dplyr::filter(al==1)
                          
correcao_merge <- left_join(DPI_corrigir,ideo_corrigir) %>%
  dplyr::filter(is.na(party)==T)
            
write_excel_csv(ideo_corrigir,"ideologias.csv")
write_excel_csv(correcao_merge,"partidos_que_precisam_ser_achados.csv")
wri

# PLaying with plots ----
ggplot() +
#  stat_smooth() +
  geom_text(data = df %>% 
              filter(!is.na(div.gov),ifs!="bra",totparties>2) %>%
              mutate(allhouse = case_when(
                allhouse == 1 ~ "Majority government",
                TRUE ~ "Minority government"
              )), #%>%
              # ungroup() %>%
              # #dplyr::select(-year) %>%
              # add_count(ifs,coal.ideo,pres.ideo) %>% 
              # filter(n>1)
            
            aes(x=year,y=abs(div.gov),label=paste0(ifs,str_sub(year,-2)),colour=country),
            family = "Times New Roman") +
  
  theme_bw(base_family = "Times", base_size = 11) +
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill="white"),
        plot.background = element_blank(),
        axis.line = element_blank(),
        panel.grid.major.x = element_line(size = 0.1),
        panel.grid.major.y = element_line(size = 0.7),
        panel.grid.minor = element_blank(),
        legend.position="none",
        legend.title=element_blank(),
        axis.ticks = element_blank())+
  facet_wrap(~ allhouse,ncol=1 ) +
  annotate("rect", ymin = 0, ymax = 2,xmin=-Inf,xmax=Inf,alpha = .2) +
  labs(y="Ideological distances",x="",title="")
  


# Loading data GARBAGE ---
load("QoG.RData")
load("PMDB.RData")
QoG <- saveLoadReference
rm(saveLoadReference)
data_pmdb <- read_excel("data_pmdb.xlsx")[c(2,3,8,9,11,12,16,18,20:24)]

# Gathering QoG ---
QoG <- QoG %>%
  gather(party,size,4:17)


# getting rid of non-print and harmonizing year-party-country variables ---
# fa <- function(x) iconv(x, to = "ASCII//TRANSLIT")
# data_pmdb$Country <- tolower(fa(data_pmdb$Country))
# data_pmdb$Party <- tolower(fa(data_pmdb$Party))
# data_pmdb$party_country <- tolower(fa(data_pmdb$party_country))

data_pmdb$Country <- tolower(data_pmdb$Country)
data_pmdb$party_spanish_transl <- tolower(data_pmdb$party_spanish_transl)
data_pmdb$party_country <- tolower(data_pmdb$party_country)


QoG$cname <- tolower(fa(QoG$cname))

# Fuzzy matching ----
# Method 2: applying different string matching methods
#osa Optimal string aligment, (restricted Damerau-Levenshtein distance).
#lv Levenshtein distance (as in R???s native adist).
#dl Full Damerau-Levenshtein distance.
#hamming Hamming distance (a and b must have same nr of characters).
#lcs Longest common substring distance.
#qgram q-gram distance.
#cosine cosine distance between q-gram profiles
#jaccard Jaccard distance between q-gram profiles
#jw Jaro, or Jaro-Winker distance.


distance.methods<-c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw')
dist.methods<-list()
for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(data_pmdb$party_spanish_transl),nrow = length(ideo_ab$Party))
  for(i in 1:length(data_pmdb$party_spanish_transl)) {
    for(j in 1:length(ideo_ab$Party)) { 
      dist.name.enh[j,i]<-stringdist(tolower(data_pmdb[i,]$party_spanish_transl),tolower(ideo_ab[j,]$Party),method = distance.methods[m])      
      #adist.enhance(data_pmdb[i,]$party_spanish_transl,ideo_ab[j,]$Party)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods))
{
  
  dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix, 1, base::min)
  for(i in 1:nrow(dist.matrix))
  {
    s2.i<-match(min.name.enh[i],dist.matrix[i,])
    s1.i<-i
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=data_pmdb[s2.i,]$party_spanish_transl, s1name=ideo_ab[s1.i,]$Party, adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}
# Let's have a look at the results
library(reshape2)
matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix)

# Removing manifesto data, because we wont use ####
rm(mp_main)

write.csv(data_pmdb,"praga/partidos.csv")
write.csv(ideo_ab,"praga/ideologia.csv")
write.csv(QoG,"praga/coalizoes.csv")


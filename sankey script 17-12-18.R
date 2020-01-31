library(tidyverse)
library(ggalluvial)
library(ggrepel)

set.seed(100)

tibble::tibble(id=1:1000,
               segment=sample(c("Segment A","Segment B"),1000,replace=TRUE,prob=c(0.35,0.65)),
               own_product=sample(c("Own","Do not own"),1000,replace=TRUE,prob=c(0.3,0.7)),
               random=sample(1:10,1000,replace=TRUE)) %>%
  mutate(own_status=case_when(own_product=="Own" & random>=5~"Through employer",
                              own_product=="Own" & random>=3~"Through spouse",
                              own_product=="Own"~"Direct",
                              TRUE~NA_character_)) %>%
  mutate(consider_status=case_when(own_product=="Do not own" & random>=5~"Consider",
                              own_product=="Do not own"~"Do not consider",
                              TRUE~NA_character_)) %>%
  select(-random)-> sample_df1


sample_df1 %>%
  count(segment,own_product,own_status,consider_status) %>%
  is_alluvia_form()

sample_df1 %>%
  # count(own_product,own_status,consider_status) -> sample_df1_summed
  count(segment,own_product,own_status,consider_status) -> sample_df1_summed

sample_df1_summed %>%
  ggplot(aes(y=n,axis1=segment,axis2=own_status,axis3=consider_status))+
  geom_alluvium(aes(fill=own_product),width=1/3) +
  geom_stratum(width=1/10,fill="white",color="grey")+
  ggrepel::geom_label_repel(stat="stratum", label.strata = TRUE)+
  # geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Segment", "Channel","Consideration"))+
  ggtitle("Ownership and Consideration - by Segment")






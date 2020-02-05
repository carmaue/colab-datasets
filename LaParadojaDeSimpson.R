#======================================================
# Fichero creado por J. A. Parejo para la Asignatura de Data Science
# Universidad de Sevilla, 2019
#======================================================
library(tidyverse)
# transformamos la table en un tibble (Opcional)
UCBAdmissions <- as.tibble(UCBAdmissions)
# Visualicemos la tabla de admisiones general
View(UCBAdmissions)

#======================================================
# Estudio teniendo en cuenta el sexo solamente:
#======================================================
# Calculamos los totales de rechazo o admisión por sexo
ucb_admit <- UCBAdmissions %>%
              group_by(Gender,Admit) %>%
              summarise(count=sum(n)) 

ucb_admit
View(ucb_admit)

# Caculamos el porcentaje por sexo y resultado
x <- ucb_admit %>% spread(Admit,count)  %>%
  mutate(Perc_Admit = Admitted / (Admitted + Rejected))

x

#======================================================
# Estudio teniendo en cuenta el sexo y el departamento:
#======================================================
admit_by_dept <- UCBAdmissions %>%
  spread(Admit, n) %>%
  mutate(Perc_Admit = Admitted / (Admitted + Rejected),
         Total = Admitted + Rejected)
admit_by_dept

# Visualizamos las admisiones por sexo y departamento 
# como un diagrama de barras:
ggplot(admit_by_dept,aes(x=Dept,y=Perc_Admit,fill=Gender)) +
  geom_bar(stat="identity",position=position_dodge())

admit <- UCBAdmissions %>%
          spread(key=Gender,value=n)
admit

totals <- admit %>% 
          group_by(Dept) %>%
          summarise(Female=sum(Female),Male=sum(Male))    
totals              

totals_tidy <- totals %>% gather("Female","Male",key=Gender,value=Count)

# Visualizamos las solicitudes sexo y departamento 
# como un diagrama de barras:

ggplot(totals_tidy,aes(x=Dept,y=Count,fill=Gender)) +
  geom_bar(stat="identity",position=position_dodge())

# ¿Cual es la causa de que en este caso se esté dando la pardoja de Simpson?
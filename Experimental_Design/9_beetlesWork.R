# --------------------------------------------------------------- Trabajo final 

# Centro Regional Universitario Bariloche - Disenio experimental-
# Ezequiel Perez ^-^


							# tpfinal_beetlesWork



    # Work with all databases ++++

#### Packtages and configuration needed ----

library('readxl')
library('ggplot2') 
library('dplyr') # data manipulation
library('car') # It allows anova type 2 and 3
library('lmerTest')  # tests for general linear mixed models (anova & rand) plus p-values.
options(contrasts=c("contr.sum","contr.poly")) ## needed for correct conditional SS


#### some databases ----

beetles<-read_excel('data/beetles.xlsx')
str(beetles)
View(beetles)

bee_breed<-read.csv('data/beetles_breeding.csv', sep=',', header=T)
str(bee_breed)
View(bee_breed)

bee_eggs<-read.csv('data/beetles_eggs.csv', sep=',', header=T)
str(bee_eggs)
View(bee_eggs)

bee_biparent<-read_excel('data/bee_biparental.xlsx', sheet='Offspring fitness')
str(bee_biparent)

bee_pareneff1<-read_excel('data/bee_parentaleffects_exp1.xlsx')
str(bee_pareneff1)

bee_pareneff2<-read_excel('data/bee_parentaleffects_exp2.xlsx')
str(bee_pareneff2)

bee_parencomp<-read.csv('data/bee_parentcompetition.csv', header=T, sep=',')
str(bee_parencomp)

bee_padres<-read_excel('data/bee_parenting.xlsx')
str(bee_padres)

bee_resources<-read_excel('data/bee_resources.xlsx')
str(bee_resources)

bee_siblings<-read.csv('data/bee_siblings.csv', header=T, sep=',')
str(bee_siblings)


#### data manipulation ----

attach(beetles)
parental_state<-as.factor(parental_state)
treatment<-as.factor(treatment)
mouse_size<-as.factor(mouse_size)
beetles2<-filter(beetles, mouse_size=='5')
detach(beetles)
write.table(beetles2, 'data/beetles2.csv', sep = ',')

attach(beetles2)

mass_loss_72<-carcass_weight_beginning-carcass_weight_72_own_carcass
mass_loss2_72<-carcass_weight_beginning-carcass_weight_new_carcass_72 # que es esto?
avg_brood_mass<-average_larval_weight*number_larvae
female_mass_loss<-female_weight_beginning-female_weight_desertion
male_mass_loss<-male_weight_beginning-male_weight_desertion
male_mass_loss[is.na(male_mass_loss)]<-0

for(i in 1:length(female_mass_loss)){ # numero como 0 a la perdida de masa positiva (la hembra perdió masa durante la crianza)
  if(female_mass_loss[i]>0){
    female_mass_loss[i]<-0
  }
}

for(i in 1:length(male_mass_loss)){ # numero como 0 a la perdida de masa positiva (el macho perdió masa durante la crianza)
  if(male_mass_loss[i]>0){
    male_mass_loss[i]<-0
  }
}

total_mass_loss<-mass_loss_72-female_mass_loss-male_mass_loss # resto a la perdida de masa en las 72 hs la perdida negativa de
# masa (ganancia de masa) de padre y madre. Osea, a la masa perdida en las 72hs le sumo la ganancia de los padres en lo que resta
# del tiempo de crianza.



detach(beetles2)
beetles3<-beetles2[-c(4,18,30,29,17),]

	# bee_breed

bee_breed$treatment<-as.factor(bee_breed$treatment)

	# bee_eggs

bee_eggs$treatment<-as.factor(bee_eggs$treatment)

#### graphics ----

	# graphics with beetles2
    # continuos
attach(beetles2)
plot(data.frame(mass_change_carcass, brood_mass, number_larvae, average_larval_weight, pronotum_female))

plot1<-ggplot(data=beetles2, aes(x=number_larvae, y=average_larval_weight, col=parental_state)) +
	geom_point() + 
	geom_smooth(method=lm)

plot1

ggplot(data=beetles2, aes(x=number_larvae, y=average_larval_weight, col=treatment)) +
  geom_point() + 
  geom_smooth(method=lm)

ggplot(data=beetles2, aes(x=number_larvae, y=average_larval_weight, col=treatment, shape=parental_state))+
  geom_point(aes(col=treatment)) + 
  geom_smooth(method=lm)

    # factoring
beetles2$treatment<-as.factor(beetles2$treatment)
boxplot(brood_mass~treatment, data=beetles2)
detach(beetles2)

	# graphics with bee_breed
attach(bee_breed)
		#continuos variables
plot(data.frame(pair, carcass_mass.g., female_family, female_pronotum.mm., success, 
		larvae, total_brood_mass.g., mean_larval_mass.g.))
ggplot(data=bee_breed, aes(x=total_brood_mass.g., y=mean_larval_mass.g., col=treatment))+
	geom_point() +
	geom_smooth(se=F)
ggplot(data=bee_breed, aes(x=larvae, y=mean_larval_mass.g., col=treatment))+
	geom_point() +
	geom_smooth(se=F)
		#factoring
detach(bee_breed)
boxplot(y=bee_breed$total_brood_mass.g., x=bee_breed$treatment) # no se porque no funciona !!

	# graphics with bee_eggs
attach(bee_eggs)
hist(carcass_mass.g.)
plot(data.frame(pair, carcass_mass.g., female_family, female_pronotum.mm., clutch_size, mean_egg_mass.g.))
ggplot(data=bee_eggs, aes(x=female_pronotum.mm., y=clutch_size, col = treatment))+
	geom_point()+
	geom_smooth(method=lm)
ggplot(data=bee_eggs, aes(x=female_pronotum.mm., y=mean_egg_mass.g., col = treatment))+
	geom_point()+
	geom_smooth(method=lm, )
detach(bee_eggs)

  # graphics with bee_biparent
attach(bee_biparent)
		# continous variables:
plot(data.frame(Female_pronotum, Male_pronotum, TotalCarcassMass, BroodSize, BroodMass, 
	AvgLrvMass, NumberEclosed, PEclosed, NumberDead,
	PSurvivalEclosion))
ggplot(data=bee_biparent, aes(x=BroodMass, y=AvgLrvMass, colour=as.factor(Treatment)))+
	geom_point()+
	geom_smooth(method=lm)
ggplot(data=bee_biparent, aes(x=BroodMass, y=PSurvivalEclosion, colour=as.factor(Treatment)))+
	geom_point()+
	geom_smooth(method=lm)
ggplot(data=bee_biparent, aes(x=BroodMass, y=NumberDead, colour=as.factor(Treatment)))+
	geom_point()+
	geom_smooth(method=lm)
ggplot(data=bee_biparent, aes(x=AvgLrvMass, y=PSurvivalEclosion))+
	geom_point()+
	geom_smooth()
		# factoring
plot(PSurvivalEclosion~as.factor(Treatment))
plot(AvgLrvMass~as.factor(Treatment))
plot(BroodMass~as.factor(Treatment))
plot(BroodSize~as.factor(Treatment))
plot(Female_pronotum~as.factor(Treatment))
detach(bee_biparent)


	# graphics with pareneff1
		# continuos
attach(bee_pareneff1)
plot(data.frame(pronotum_width, larval_mass, carcass, brood_size_dispersal, 
	brood_mass_dispersal, lifespan))
ggplot(data=bee_pareneff1, aes(x=pronotum_width, y = larval_mass, colour=as.factor(brood_treatment)))+ 
	geom_point() +
	geom_smooth(method=lm)

ggplot(data=bee_pareneff1, aes(x=pronotum_width, y = larval_mass, colour=as.factor(develop_treatment)))+ 
	geom_point() +
	geom_smooth(method=lm)

ggplot(data=bee_pareneff1, aes(x=pronotum_width, y = larval_mass, colour=as.factor(sex)))+ 
	geom_point() +
	geom_smooth(method=lm)
detach(bee_pareneff1)


	# graphics with pareneff2
		# continuos
attach(bee_pareneff2)
plot(data.frame(male_larval_mass, male_pronotum, female_larval_mass, female_pronotum, 
	first_carcass, first_brood_mass, first_brood_size, total_brood_size, 
	total_brood_mass, male_lifespan, female_lifespan, average_larval_mass))
ggplot(data=bee_pareneff2, aes(x=total_brood_mass, y=average_larval_mass))+
  geom_point()+
  geom_smooth(method=lm)

detach(bee_pareneff2)


	# graphics with bee_parencomp
		# continuos
attach(bee_parencomp)
plot(data.frame(pre_deprivation_mass, deprivation_mass_change, pre_breeding_mass,
	post_breeding_mass, mass_change_breeding, brood_size_dispersal,
	brood_mass, average_offspring_mass, brood_size_eclosion, female_lifespan))
ggplot(data=bee_parencomp, aes(x=brood_mass, y=average_offspring_mass, colour=as.factor(treatment)))+
	geom_point()+
	geom_smooth(method=lm)

ggplot(data=bee_parencomp, aes(x=brood_mass, y=average_offspring_mass, colour=as.factor(carcass)))+
	geom_point()+
	geom_smooth(method=lm)

ggplot(data=bee_parencomp, aes(y=brood_mass, x=average_offspring_mass, colour=as.factor(carcass)))+
	geom_point()+
	geom_smooth()

ggplot(data=bee_parencomp, aes(y=brood_mass, x=average_offspring_mass, colour=as.factor(treatment)))+
	geom_point()+
	geom_smooth(method=lm)
detach(bee_parencomp)

	# graphics with bee_padres
		# continuos
attach(bee_padres)
str(bee_padres) #leer para entender un poco mas la base de datos.
detach(bee_padres)


	# graphics with bee_resources
		# continuos
attach(bee_resources)
df<-data.frame(time_first_egg, laying_spread, clutch_size, 
	average_egg_size, Hatched, female_indirect_care, female_direct_care, larval_begging,
	larval_feeding, no_larvae_at_dispersal, avg_ind_mass_dispersal)
dff<-apply(df, 2, as.numeric)
apply(dff, 2, class)
dfff<-data.frame(carcass_weight, dff)
plot(dfff)
ggplot(data=bee_resources, aes(x=no_larvae_at_dispersal, y=avg_ind_mass_dispersal, col=as.factor(mating_treatment)))+
  geom_point()+
  geom_smooth(method=lm)
  
detach(bee_resources)
# hacer boxplot con mating treatment y resource treatment y alguna v.r pertinente.


	# graphics with bee_siblings
		# continuos
attach(bee_siblings)
plot(data.frame(Carcass, AliveDisp, DeadDisp, IndMassDisp, AliveEcl, DeadEcl))
		# factors
plot(AliveEcl~as.factor(InbreedingStatus))
plot(AliveEcl~as.factor(ParentRelatedness))
detach(bee_siblings)



#### graphics for ppt ----

ggplot(data=bee_breed, aes(x=larvae, y=mean_larval_mass.g.))+
  geom_point(col='red') +
  geom_smooth(se=F, col='gray') +
  ylab('masa media') +
  xlab('N° larvas')

ggplot(data=beetles2, aes(x=number_larvae, y=average_larval_weight, col=parental_state)) +
  geom_point() + 
  geom_smooth(method=lm) +
  ylab('Masa media por larva (gr)') +
  xlab('N° larvas') 

ggplot(data=beetles2, aes(x=number_larvae, y=average_larval_weight, col=treatment)) +
  geom_point() + 
  geom_smooth(method=lm) +
  ylab('Masa media por larva (gr)') +
  xlab('N° larvas')

ggplot(data=beetles2, aes(x=number_larvae, y=mass_change_carcass, col=parental_state))+
  geom_point() + 
  geom_smooth(method = lm) +
  ylab('perdida de masa del cadaver (gr)') + 
  xlab('N° larvas')


ggplot(data=beetles2, aes(x=mass_change_carcass, y=average_larval_weight, col=parental_state)) +
  geom_point() + 
  geom_smooth(method=lm) +
  ylab('Masa media por larva') +
  xlab('pérdida de masa del cadaver (72hs)')

attach(beetles2)
plot(data.frame(mass_loss_72, mass_loss2_72, female_mass_loss, male_mass_loss, avg_brood_mass, total_mass_loss,
                number_larvae, average_larval_weight
                ))
detach(beetles2)

ggplot(data=beetles2, aes(x=number_larvae, y=total_mass_loss, col=parental_state)) +
  geom_point() + 
  geom_smooth(method=lm) +
  ylab('pérdida total de masa del cadaver') +
  xlab('N° de larvas')

#### models ----

kumiko<-lm(data=beetles2, average_larval_weight~number_larvae*parental_state) # model
par(mfcol=c(2,2))
plot(kumiko) # diagnostic graphs
par(mfcol=c(1,1))
summary(kumiko) # summary 
Anova(kumiko, type=3)# anova type 1

reina<-lm(data=beetles2, mass_change_carcass~number_larvae*parental_state)
par(mfcol=c(2,2))
plot(reina) # diagnostic graphs
par(mfcol=c(1,1))
summary(reina) # summary 
Anova(reina, type=3)# anova type 1

suichi<-lmer(data=beetles2, average_larval_weight~(1|treatment)+number_larvae*parental_state) # mixel model
par(mfcol=c(2,2))
plot(suichi) # diagnostic graphs
par(mfcol=c(1,1))
summary(suichi) # summary 
Anova(suichi, type=3)# anova type 1

#-------------------------otras cosas
beetles4<-filter(beetles2, parental_state=='uni')
beetles5<-filter(beetles2, parental_state=='bi')
length(beetles4$ID)
length(beetles5$ID)


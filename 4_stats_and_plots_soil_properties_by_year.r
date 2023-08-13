

 # libraries

        library(ggplot2)
        library(Hmisc)
        library(lme4)
        library(lmerTest)
        library(ggsignif)
        library(multcompView)
        library(ggpubr)
        library(stargazer)
        library(dplyr)

# 1/ prep data

	# read data
		soil <- read.table("soil_properties.csv", h = T)
		
		soil$treat <- as.factor(soil$treat)

	# clean columns
		soil$year <- as.numeric(as.character(soil$year))
		soil$year_fac <- ordered(soil$year)
		soil$plot <- as.factor(soil$plot)

# macronutrients

	# stats
		# pairwise comparisons
			aov_som <- aov(SOM ~ treat * year_fac, data = soil)
			aov_toc <- aov(TOC ~ treat * year_fac, data = soil)
			aov_N <- aov(Nt ~ year_fac * treat, data = soil)
			aov_P <- aov(P2O5 ~ year_fac * treat, data = soil)
			aov_K <- aov(K2O ~ year_fac * treat, data = soil)
			aov_ph <- aov(pH_water ~ year_fac * treat, data = soil)
			aov_cec <- aov(CEC ~ year_fac * treat, data = soil)
			aov_cn <- aov(C.N ~ year_fac * treat, data = soil)

			tukey_som <- TukeyHSD(aov_som)
			tukey_toc <- TukeyHSD(aov_toc)			
			tukey_N <- TukeyHSD(aov_N)
			tukey_P <- TukeyHSD(aov_P)
			tukey_K <- TukeyHSD(aov_K)
			tukey_ph <- TukeyHSD(aov_ph)
			tukey_cec <- TukeyHSD(aov_cec)	
			tukey_cn <- TukeyHSD(aov_cn)	


			letters_som <- multcompLetters4(aov_som, tukey_som)
			letters_toc <- multcompLetters4(aov_toc, tukey_toc)			
			letters_N <- multcompLetters4(aov_N, tukey_N)
			letters_P <- multcompLetters4(aov_P, tukey_P)
			letters_K <- multcompLetters4(aov_K, tukey_K)
			letters_ph <- multcompLetters4(aov_ph, tukey_ph)
			letters_cec <- multcompLetters4(aov_cec, tukey_cec)
			letters_cn <- multcompLetters4(aov_cn, tukey_cn)


		# letters from tests

			mean_som <- soil %>% 
				group_by(treat, year_fac) %>%
				dplyr::summarize(mean = mean(SOM), quant = quantile(SOM, probs = 0.75))
			mean_som$test <- c("a","a","b","b")

			mean_toc <- soil %>% 
				group_by(treat, year_fac) %>%
				dplyr::summarize(mean = mean(TOC), quant = quantile(TOC, probs = 0.75))
			mean_toc$test <- c("a","a","b","b")

			mean_N <- soil %>% 
				group_by(treat,year_fac) %>%
				dplyr::summarize(mean = mean(Nt), quant = quantile(Nt, probs = 0.75))
			mean_N$test <- c("a","a","b","b")

			mean_P <- soil %>% 
				group_by(treat,year_fac) %>%
				dplyr::summarize(mean = mean(P2O5), quant = quantile(P2O5, probs = 0.75))
			mean_P$test <- c("a","b","a","ab")

			mean_K <- soil %>% 
				group_by(treat,year_fac) %>%
				dplyr::summarize(mean = mean(K2O), quant = quantile(K2O, probs = 0.75))
			mean_K$test <- c("a","b","b","b")

			mean_ph <- soil %>% 
				group_by(treat,year_fac) %>%
				dplyr::summarize(mean = mean(pH_water), quant = quantile(pH_water, probs = 0.75))
			mean_ph$test <- c("a","ab","ab","b")

			mean_cec <- soil %>% 
				group_by(treat,year_fac) %>%
				dplyr::summarize(mean = mean(CEC), quant = quantile(CEC, probs = 0.75))
			mean_cec$test <- c("a","ab","ab","b")

			mean_cn <- soil %>% 
				group_by(treat,year_fac) %>%
				dplyr::summarize(mean = mean(C.N), quant = quantile(C.N, probs = 0.75))
			mean_cn$test <- c("a","b","b","b")




	# plot

		# dummy plot to create simplified legend

		gg_leg <- get_legend(ggplot(soil, aes(x = treat, y = TOC, fill = treat)) + geom_boxplot() + 
							scale_fill_manual(	name = "Treatment", 
									breaks = c("R","RC"),
									values = c("#EC9755","#833F0B"), 
									labels = c("Fine Earth","Fine Earth + Compost")
									) +
							theme_classic() +
							theme(legend.direction = "horizontal", legend.title = element_blank()))

		# 1st line

		# TOC
			gg_toc <- ggplot(soil, aes(x = year_fac, y = TOC, fill = interaction(year_fac, treat))) +
								geom_boxplot() + 
								#geom_smooth( aes(x = as.numeric(treat), y = TOC, col = year_fac), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +	
								geom_text(data = mean_toc, aes(x = c(.85, 1.90, 1.25, 2.25), y = quant, label = test), vjust = -.8, size = 2.5) +	
								#geom_text(data = mean_toc, aes(x = c(.8, 1.2, 1.8, 2.2), y = rep(.2,4), label = c(2013,2016,2013,2016)), vjust = 0.8 , size = 2.5) +
								ylab ("Organic Carbon") +
								ylim(0, 18) +
								scale_fill_manual(name = "Treatment", 
												breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
												values = c("#EC9755", "#833F0B", "#EC9755",  "#833F0B"), 
												labels = c("Fine Earth","Fine Earth + Compost","Fine Earth","Fine Earth + Compost"))  +
								theme_classic() +
								theme(legend.position =  "none", axis.title.x = element_blank(), axis.text.x = element_blank())	

		# SOM
			gg_som <- ggplot(soil, aes(x = year_fac, y = SOM, fill = interaction(year_fac, treat))) +
								geom_boxplot() + 
								geom_smooth( aes(x = as.numeric(treat), y = SOM, col = year_fac), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +	
								geom_text(data = mean_som, aes(x = c(.85, 1.9, 1.25, 2.25), y = quant, label = test), vjust = -.8) +	
								ylab ("Organic Carbon") +
								ylim(0, 25) +
								scale_fill_manual(	name = "Treatment", 
													breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
													values = c("#EC9755","#833F0B",  "#EC9755", "#833F0B"), 
													labels = c("Fine Earth","Fine Earth + Compost","Fine Earth","Fine Earth + Compost"))  +
								theme_classic() +
								theme(legend.position =  "none", axis.title.x = element_blank(), axis.text.x = element_blank())





		# N
			gg_N <- ggplot(soil, aes(x = year_fac, y = Nt, fill = interaction(year_fac, treat))) +
										geom_boxplot() + 
										geom_smooth( aes(x = as.numeric(treat), y = Nt, col = year_fac), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +	
										geom_text(data = mean_N, aes(x = c(.85, 1.90, 1.25, 2.25), y = quant, label = test), vjust = -.8, size = 2.5) +	
										#geom_text(data = mean_cn, aes(x = c(.8, 1.2, 1.8, 2.2), y = rep(1,4), label = c(2013,2016,2013,2016)), vjust = 0.8 , size = 2.5) +	
										ylab ("Nitrogen") +
										xlab("Type of Technosol") +
										ylim(0, 1) +
										scale_fill_manual(	name = "Treatment", 
															breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
															values = c("#EC9755","#833F0B",  "#EC9755", "#833F0B"), 
															labels = c("Fine Earth","Fine Earth + Compost","Fine Earth","Fine Earth + Compost"))  +
										scale_x_discrete(labels = c("Fine Earth","Fine Earth + Compost")) +
										theme_classic() +
										theme(legend.position =  "none", axis.title.x = element_blank(), axis.text.x = element_blank())	

		# C/N
			gg_CN <- ggplot(soil, aes(x = year_fac, y = C.N, fill = interaction(year_fac, treat))) +
								geom_boxplot() + 
								geom_text(data = mean_cn, aes(x = c(.9, 1.9, 1.25, 2.25), y = quant, label = test), vjust = -.8, size = 2.5) +
								ylab ("C:N") +
								xlab("Type of Technosol") +
								scale_fill_manual(	name = "Treatment", 
													breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
													values = c("#EC9755","#833F0B",  "#EC9755", "#833F0B"), 
													labels = c("Fine Earth","Fine Earth + Compost","Fine Earth","Fine Earth + Compost"))  +
								scale_y_log10(limits = c(5, 330) ) +
								annotation_logticks(sides="l") +
								theme_classic() +
								theme(legend.position =  "none", axis.title.x = element_blank(), axis.text.x = element_blank())	



		# 2nd line

		# pH
			gg_ph <- ggplot(soil, aes(x = year_fac, y = pH_water, fill = interaction(year_fac, treat))) +
								geom_boxplot() + 
								geom_smooth( aes(x = as.numeric(treat), y = pH_water, col = year_fac), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +
								geom_text(data = mean_ph, aes(x = c(.9, 1.9, 1.25, 2.25), y = quant, label = test), vjust = -.8, size = 2.5) +
								ylab ("pH") +
								ylim(7, 9) +
								scale_fill_manual(	name = "Treatment", 
													breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
													values = c("#EC9755","#833F0B",  "#EC9755", "#833F0B"), 
													labels = c("Fine Earth","Fine Earth + Compost","Fine Earth","Fine Earth + Compost")) +
								theme_classic() +
								theme(legend.position =  "none", axis.title.x = element_blank())	



		# P
			gg_P <- ggplot(soil, aes(x = year_fac, y = P2O5, fill = interaction(year_fac, treat))) +
								geom_boxplot() + 
								geom_smooth( aes(x = as.numeric(treat), y = P2O5, col = year_fac), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +
								geom_text(data = mean_P, aes(x = c(.9, 1.9, 1.25, 2.3), y = quant, label = test), vjust = -.8 , size = 2.5) +
								ylab ("Phosphorus") +
								xlab("Type of Technosol") +
								ylim(0, .075) +
								scale_fill_manual(	name = "Treatment", 
													breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
													values = c("#EC9755","#833F0B",  "#EC9755", "#833F0B"), 
													labels = c("Fine Earth","Fine Earth + Compost","Fine Earth","Fine Earth + Compost"))  +
								theme_classic() +
								theme(legend.position =  "none", axis.title.x = element_blank())	


		# K
			gg_K <- ggplot(soil, aes(x = year_fac, y = K2O, fill = interaction(year_fac, treat))) +
								geom_boxplot() + 
								geom_smooth( aes(x = as.numeric(treat), y = K2O, col = year_fac), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +	
								geom_text(data = mean_K, aes(x = c(.85, 1.9, 1.25, 2.25), y = c(.035, .15, .2, .145), label = test), vjust = -.8, size = 2.5) +
								ylab ("Potassium") +
								xlab("Type of Technosol") +
								ylim(0, .26) +
								scale_fill_manual(	name = "Treatment", 
													breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
													values = c("#EC9755","#833F0B",  "#EC9755", "#833F0B"), 
													labels = c("Fine Earth","Fine Earth + Compost","Fine Earth","Fine Earth + Compost"))  +
								theme_classic() +
								theme(legend.position =  "none", axis.title.x = element_blank())	



		ggarrange(gg_toc, gg_N, gg_CN, gg_ph, gg_P, gg_K,   ncol = 3, nrow = 2, widths = c(0.22, .225,.225,.225, .225), labels = "AUTO", common.legend = TRUE, legend = "bottom", legend.grob = gg_leg)


		ggsave("..//Fig_2_soil_nutrient_by_year.pdf", device = cairo_pdf, width = 6, height = 4)


		# ggarrange( gg_som, gg_N, gg_P, gg_K, gg_ph, ncol = 5, nrow = 1, widths = c(0.22, .225,.225,.225, .225), common.legend = TRUE, legend="bottom",labels="AUTO")
		# ggsave("Fig_2_soil_nutrient.pdf", device = cairo_pdf, width = 9, height = 5)




# metals

	# stats

		# pairwise comparisons
			aov_cu <- aov(Cu_ER ~ treat * year_fac, data = soil)
			aov_pb <- aov(Pb_ER ~ treat * year_fac, data = soil)
			aov_ni <- aov(Ni_ER ~ treat * year_fac, data = soil)
			aov_zn <- aov(Zn_ER ~ treat * year_fac, data = soil)

			tukey_cu <- TukeyHSD(aov_cu)
			tukey_pb <- TukeyHSD(aov_pb)
			tukey_ni <- TukeyHSD(aov_ni)
			tukey_zn <- TukeyHSD(aov_zn)

			letters_cu <- multcompLetters4(aov_cu, tukey_cu)
			letters_pb <- multcompLetters4(aov_pb, tukey_pb)
			letters_ni <- multcompLetters4(aov_ni, tukey_ni)
			letters_zn <- multcompLetters4(aov_zn, tukey_zn)

			summary(aov(Cu_ER ~ year * treat, data = soil))
			summary(aov(Pb_ER ~ year * treat, data = soil))
			summary(aov(Ni_ER ~ year * treat, data = soil))
			summary(aov(Zn_ER ~ year * treat, data = soil))

		# letters from tests

			mean_cu <- soil %>% 
				group_by(treat,year_fac) %>%
				dplyr::summarize(mean = mean(Cu_ER), quant = quantile(Cu_ER, probs = 0.75))
			mean_cu$test <- c("ab","a","b","ab")

			mean_pb <- soil %>% 
				group_by(treat,year_fac) %>%
				dplyr::summarize(mean = mean(Pb_ER), quant = quantile(Pb_ER, probs = 0.75))
			mean_pb$test <- c("a","b","a","c")

			mean_ni <- soil %>% 
				group_by(treat,year_fac) %>%
				dplyr::summarize(mean = mean(Ni_ER), quant = quantile(Ni_ER, probs = 0.75))
			mean_ni$test <- c("ab","bc","a","c")

			mean_zn <- soil %>% 
				group_by(treat,year_fac) %>%
				dplyr::summarize(mean = mean(Zn_ER), quant = quantile(Zn_ER, probs = 0.75))
			mean_zn$test <- c("a","a","a","a")


letters_cu[[3]]$Letters

		# lm
		    lm_cu <- lm(Cu_ER ~ year * treat, data = soil)
		    lm_pb <- lm(Pb_ER ~ year * treat, data = soil)
		    lm_ni <- lm(Ni_ER ~ year * treat, data = soil)
		    lm_cec <- lm(Zn_ER ~ year * treat, data = soil)


		# summary table
		    stargazer(lm_cu,lm_pb,lm_ni, lm_cec, type = "text", digits = 2 , 
		        align =F,
		        dep.var.labels = c("Cu", "Pb", "Ni", "Zn"),
		        covariate.labels = c("Year", "Treatment", "Year * Treatment"),
		        omit = "Constant",
		        #single.row = T,
		        omit.stat = "ser")


	# plots

		# Cu
			gg_cu <- ggplot(soil, aes(x = year_fac, y = Cu_ER, fill = interaction(year_fac, treat))) +
								geom_boxplot() + 
								geom_smooth( aes(x = as.numeric(treat), y = Cu_ER, col = year_fac), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +		
								geom_text(data = mean_cu, aes(x = c(.9, 1.25, 1.90, 2.3), y = quant, label = test), vjust = -.8, size = 2.5) +
								ylab ("soil Cu") +
								ylim(0, 25) +
								scale_fill_manual(	name = "Treatment", 
													breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
													values = c("#EC9755","#833F0B",  "#EC9755", "#833F0B"), 
													labels = c("Fine Earth","Fine Earth + Compost"))  +
								scale_x_discrete(labels = c("Fine Earth","Fine Earth + Compost")) +
								theme_classic() +
								theme(legend.position =  "none", axis.title.x = element_blank(), axis.text.x = element_blank())	
								# theme(legend.position = c(0.85, 0.25))



		# Pb
			gg_pb <- ggplot(soil, aes(x = year_fac, y = Pb_ER, fill = interaction(year_fac, treat))) +
								geom_boxplot() + 
								geom_smooth( aes(x = as.numeric(treat), y = Pb_ER, col = year_fac), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +	
								geom_text(data = mean_pb, aes(x = c(.85, 1.9, 1.25, 2.25), y = quant, label = test), vjust = -.8, size = 2.5) +									
								ylab ("soil Pb") +
								ylim(0, 40) +
								scale_fill_manual(	name = "Treatment", 
													breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
													values = c("#EC9755","#833F0B",  "#EC9755", "#833F0B"), 
													labels = c("Fine Earth","Fine Earth + Compost"))  +
								scale_x_discrete(labels = c("Fine Earth","Fine Earth + Compost")) +
								theme_classic() +
								theme(legend.position =  "none", axis.title.x = element_blank(), axis.text.x = element_blank())	
								# theme(legend.position = c(0.85, 0.25))



		# Ni
			gg_ni <- ggplot(soil, aes(x = year_fac, y = Ni_ER, fill = interaction(year_fac, treat))) +
								geom_boxplot() + 
								geom_smooth( aes(x = as.numeric(treat), y = Ni_ER, col = year_fac), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +		
								geom_text(data = mean_ni, aes(x = c(.9, 1.25, 1.90, 2.25), y = quant, label = test), vjust = -.8, size = 2.5) +									
								geom_text(data = mean_ni, aes(x = c(.85, 1.2, 1.8, 2.2), y = rep(0,4), label = c(2013,2016,2013,2016)), vjust = 0.8 , size = 2.5) +																									
								ylab ("soil Ni") +
								xlab("Type of Technosol") +
								ylim(0, 18) +
								scale_fill_manual(	name = "Treatment", 
													breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
													values = c("#EC9755","#833F0B",  "#EC9755", "#833F0B"), 
													labels = c("Fine Earth","Fine Earth + Compost"))  +
								scale_x_discrete(labels = c("Fine Earth","Fine Earth + Compost")) +
								theme_classic() +
								theme(legend.position =  "none")	
								# theme(legend.position = c(0.85, 0.25))


		# Zn
			gg_cec <- ggplot(soil, aes(x = year_fac, y = Zn_ER, fill = interaction(year_fac, treat))) +
								geom_boxplot() + 
								geom_smooth( aes(x = as.numeric(treat), y = Zn_ER, col = year_fac), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +		
								geom_text(data = mean_cec, aes(x = c(.85, 1.9, 1.25, 2.25), y = c(25.5, 24, 29, 39.5), label = test), vjust = -.8, size = 2.5) +								
								geom_text(data = mean_ni, aes(x = c(.85, 1.2, 1.8, 2.2), y = rep(0,4), label = c(2013,2016,2013,2016)), vjust = 0.8, size = 2.5) +								
								ylab ("soil Zn") +
								xlab("Type of Technosol") +
								ylim(0, 55) +
								scale_fill_manual(	name = "Treatment", 
													breaks = c("2013.R", "2013.RC", "2016.R",  "2016.RC"),
													values = c("#EC9755","#833F0B",  "#EC9755", "#833F0B"), 
													labels = c("Fine Earth","Fine Earth + Compost"))  +
								scale_x_discrete(labels = c("Fine Earth","Fine Earth + Compost")) +
								theme_classic() +
								theme(legend.position =  "none")	
								# theme(legend.position = c(0.85, 0.25))							
																						

	ggarrange	(gg_cu, gg_pb, gg_ni, gg_cec, ncol = 2, nrow = 2, widths = c(.5, .5), labels = "AUTO")
	ggsave("Fig_3_metals.pdf", device = cairo_pdf, width = 5, height = 5)

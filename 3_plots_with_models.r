### script from the paper 
### contact : Jerome Mathieu

# libraries
	library(ggplot2)
	library(ggpubr)


# 3/ plots data with gam models from script 2

# --- MF ---

	# alpha div

		MF_alpha_model <- ggplot(MF_grassland2, aes(x = year_fac, y = n_taxa)) +
							geom_boxplot(aes(x = year_fac, y = n_taxa, fill = treat)) +
							geom_smooth( aes(x = year_tofit, y = .fitted),se = F, span = 3, size = 1, col = "grey50") +		
							geom_boxplot(aes(x = year_fac, y = n_taxa, fill = treat)) +		
							ylab ("Soil Macrofauna\nAlpha diversity") +
							xlab("Year") +
							ylim(0,15) +
							scale_fill_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							scale_colour_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							theme_classic() +
							theme(legend.position = c(0.85, 0.15),
								axis.title.x = element_blank(),
					        	axis.text.x = element_blank()
					        	)



	# beta div

		MF_beta_model <- ggplot(beta_div_plot_MF_long_ok2, aes(x = year1_fac, y = dist, fill = treat1)) +
							geom_boxplot(aes(x = year1_fac, y = dist)) +
							geom_line( aes(x = year1_tofit, y = .fitted), se = F, span = 3, size = 1, col = "grey50") +		
							geom_boxplot(aes(x = year1_fac, y = dist)) +		
							ylab ("Beta diversity") +
							xlab("Year") +
							ylim(0, .70) +
							scale_fill_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							scale_colour_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							theme_classic() +
							theme(legend.position = c(0.85, 0.75),
								axis.title.x = element_blank(),
								axis.text.x = element_blank()
								)





	# gamma div

		MF_gamma_model <- ggplot(MF_grassland_agg_trt2, aes(x = year, y = n_taxa)) +
							geom_point(aes(x = year, y = n_taxa, shape = treat, fill = treat, col = treat) , size = 2.5) +
							geom_smooth( aes(x= year, y = .fitted) ,se = F, span = 3, size = 1, linetype = "dashed", col = "grey50") +	
							#geom_line( aes(x= year, y = .fitted) , size = 1, linetype = "dashed") +							
							ylab ("Gamma diversity") +
							xlab("Year") +
							ylim(0,30) +
							scale_fill_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							scale_colour_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							theme_classic() +
							theme(legend.position = c(0.85, 0.15),
								axis.title.x = element_blank(),
					        	axis.text.x = element_blank()
					        	)





	# abundance

		MF_ab_model <- ggplot(MF_grassland3, aes(x = year, y = ab)) +
						geom_point(aes(x = year, y = ab, shape = treat, fill = treat, col = treat) , size = 2.5) +
						geom_smooth( aes(x= year, y = .fitted), se = F, span = 3, size = 1, col = "grey50") +
						geom_point(aes(x = year, y = ab, shape = treat, fill = treat, col = treat) , size = 2.5) +								
						ylab (expression(Individuals.~m^{"-2"})) +
						xlab("Year") +
						scale_fill_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
						scale_colour_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
						theme_classic() +
						theme(legend.position = c(0.85, 0.15),
							axis.title.x = element_blank(),
				        	axis.text.x = element_blank()
				        	)




# --- plants ---

	# alpha div
		veg_alpha_model <- ggplot(veg_grassland2, aes(x = year_fac, y = n_taxa)) +
							geom_boxplot(aes(x = year_fac, y = n_taxa, fill = treat)) +
							geom_smooth( aes(x = year_tofit, y = .fitted) ,se = F, method = "lm", size = 1, linetype = "dashed" , col = "grey50") +	 #505743	
							geom_boxplot(aes(x = year_fac, y = n_taxa, fill = treat)) +		
							ylab ("Plant\nAlpha diversity") +
							xlab("Year") +
							ylim(0,30) +
							scale_fill_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							scale_colour_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							theme_classic() +
							theme(legend.position = c(0.85, 0.15))


	# beta div
		veg_beta_model <- ggplot(beta_div_plot_veg_long_ok2, aes(x = year1_fac, y = dist)) +
							geom_boxplot(aes(x = year1_fac, y = dist, fill = treat1)) +
							geom_smooth( aes(x= as.numeric(year1_fac), col = treat1), se = F, method = "lm", size = 1, linetype="dashed", col = "grey50") +		
							geom_boxplot(aes(x = year1_fac, y = dist, fill = treat1)) +		
							ylab ("Beta diversity") +
							xlab("Year") +
							ylim(0, .70) +
							scale_fill_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							scale_colour_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							theme_classic() +
							theme(legend.position = c(0.85, 0.75))

	# gamma div
		veg_gamma_model <- ggplot(veg_grassland_agg_01_2, aes(x = year, y = n_taxa)) +
							geom_point(aes(x = year, y = n_taxa, shape = treat, fill = treat, col = treat) , size = 2.5) +
							geom_smooth( aes(x= year, y = .fitted) ,se = F, span = 3, size = 1, linetype="dashed", col = "grey50") +		
							ylab ("Gamma diversity") +
							xlab("Year") +
							ylim(0,40) +
							scale_fill_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							scale_colour_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							theme_classic() +
							theme(legend.position = c(0.85, 0.15))

	# abundance
		veg_ab_model <- ggplot(veg_bm_2, aes(x = year, y = value, fill = treat, col = treat)) +
							geom_point(aes(x = year, y = value, shape = treat) , size = 2.5) +
							geom_smooth( aes(x = year,y = .fitted ) , se = F, span = 3, size = 1) +		#, linetype="dashed"
							ylab (expression(Dry~weight.~g~m^{"-2"})) +
							xlab("Year") +
							ylim(0,820) +
							scale_fill_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							scale_colour_manual(name = "Treatment", values = c("R" = "#A9936A", "RC"="#939F79"), labels = c("EDH","EDH + GWC"))  +
							theme_classic() +
							theme(legend.position = c(0.85, 0.15))


# I/0

	# ggarrange(MF_alpha_model, MF_beta_model, MF_ab_model,  veg_alpha_model, veg_beta_model,  veg_ab_model , ncol = 3, nrow = 2, widths = c(0.32, .3,.33), common.legend = TRUE, legend="bottom",labels="AUTO")
	# ggsave("D:/Dropbox/en_cours/recherche/phDs/charlotte_pruvost/2022/2022_07/analyses_jerome_juillet_2022/Fig_2_3_panels.pdf", device = cairo_pdf, width = 8.5, height = 5)

	ggarrange	(veg_alpha_model, veg_beta_model, veg_gamma_model, veg_ab_model, MF_alpha_model, MF_beta_model, MF_gamma_model, MF_ab_model,  ncol = 4, nrow = 2, widths = c(0.25, .225,.225,.25), common.legend = TRUE, legend="bottom",labels="AUTO")
	ggsave("..//Fig_1_4_panels.pdf", device = cairo_pdf, width = 9, height = 5)




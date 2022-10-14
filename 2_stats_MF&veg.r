### script from the paper 
### contact : Jerome Mathieu


# libraries
	library(mgcv)
	library(plotmo)
	library(visreg)
	library(broom)
	library(tidyr)	
	library(stargazer)


### /!\ run scripts "1_prep_XX" to get the data in right format /!\ 


# rescale year to make it compatible with plots (in plots year is categorical)

	MF_grassland$year_tofit <- MF_grassland$year - 2013
	beta_div_plot_MF_long_ok$year1_tofit <- beta_div_plot_MF_long_ok$year1 -2013

	veg_grassland$year_tofit <- veg_grassland$year - 2013
	beta_div_plot_veg_long_ok$year1_tofit <- beta_div_plot_veg_long_ok$year1 - 2013


# MF
	# div alpha

		# interaction without detail by plot
			gam_mf_a <- gam(n_taxa ~ treat + s(year_tofit, plot, k = 2, bs = "fs"), data = MF_grassland, method = 'REML')
			summary(gam_mf_a)
				# treat 	   ns
				# year * plot  ***

			gam_mf_a_plot <- gam(n_taxa ~ s(year_tofit, plot, k = 2, bs = "fs"), data = MF_grassland, method = 'REML')
			summary(gam_mf_a_plot)
				# treat 	   ns
				# year * plot  *
			
			MF_grassland2 <- augment(gam_mf_a_plot , MF_grassland )


			gam_mf_a_s <- gam(n_taxa ~ s(year_tofit, k = 2, bs = "cs"), data = MF_grassland, method = 'REML')
			summary(gam_mf_a_s)
				# year  ***

	# div beta
			gam_mf_b <- gam(dist ~ treat1 + s(year1_tofit, k = 3), data = beta_div_plot_MF_long_ok, method = 'REML')
			summary(gam_mf_b)
				# treat ns
				# year  ***

			gam_mf_b_s <- gam(dist ~ s(year1_tofit, k = 3), data = beta_div_plot_MF_long_ok, method = 'REML')
			summary(gam_mf_b_s)
				# year  ***

	# div gamma

			gam_mf_g <- gam(n_taxa ~ treat + s(year, k = 3, bs = "cs"), data = MF_grassland_agg_trt, method = 'REML')
			summary(gam_mf_g)
				# treat ns
				# year  ns

			gam_mf_g_s <- gam(n_taxa ~ s(year, k = 3, bs = "cs"), data = MF_grassland_agg_trt, method = 'REML')
			summary(gam_mf_g_s)
				# year  ...

	# abundance

		# interaction without detail by plot
			gam_mf_ab <- gam(ab ~ treat + s(year, plot, k = 2, bs = "fs"), data = MF_grassland, method = 'REML')
			summary(gam_mf_ab)
				# treat ns
				# year  ***

			gam_mf_ab_s <- gam(ab ~ s(year, k = 3), data = MF_grassland, method = 'REML')
			summary(gam_mf_g_s)
				# year  ***



# veg
	# div alpha

		# interaction without detail by plot
 
			gam_veg_a <- gam(n_taxa ~ treat + s(year_tofit, plot, k = 2, bs = "fs"), data = veg_grassland, method = 'REML')
			summary(gam_veg_a)
				# treat 	   ns
				# year * plot  ns


			gam_veg_a_s <- gam(n_taxa ~ s(year_tofit, k = 2), data = veg_grassland, method = 'REML')
			summary(gam_veg_a_s)
				# year  ns

			gam_veg_a_s0 <- gam(n_taxa ~ treat, data = veg_grassland, method = 'REML')
			summary(gam_veg_a_s0)
				# treat ns


	# div beta
			gam_veg_b <- gam(dist ~ treat1 + s(year1_tofit, k = 3), data = beta_div_plot_veg_long_ok, method = 'REML')
			summary(gam_veg_b)
				# treat ns
				# year  ns

			gam_veg_b_s <- gam(dist ~ s(year1_tofit, k = 3), data = beta_div_plot_veg_long_ok, method = 'REML')
			summary(gam_veg_b_s)
				# year  ns

			gam_veg_b_s0 <- gam(dist ~ treat1, data = beta_div_plot_veg_long_ok, method = 'REML')
			summary(gam_veg_b_s0)
				# treat ns


	# div gamma

			gam_veg_g <- gam(n_taxa ~ treat + s(year, k = 3, bs = "cs"), data = veg_grassland_agg_01, method = 'REML')
			summary(gam_veg_g)
				# treat ns
				# year  ns

			gam_veg_g_s <- gam(n_taxa ~ s(year, k = 3, bs = "cs"), data = veg_grassland_agg_01, method = 'REML')
			summary(gam_veg_g_s)
				# year  ns

			gam_veg_g_s0 <- gam(n_taxa ~ treat, data = veg_grassland_agg_01, method = 'REML')
			summary(gam_veg_g_s0)
				# treat  ns

	# abundance

		veg_bm2 <- na.omit(veg_bm)
		
		# interaction without detail by plots
			gam_veg_ab <- gam(value ~ treat + s(year, k = 2, bs = "cs"), data = veg_bm, method = 'REML')
			summary(gam_veg_ab)
				# treat ***
				# year  ***

			gam_veg_ab_s <- gam(value ~ s(year, k = 3), data = veg_bm, method = 'REML')
			summary(gam_veg_g_s)
				# year  ***


# tests of the models

	# macrofauna
		anova(gam_mf_a)
		anova(gam_mf_b)
		anova(gam_mf_g)
		anova(gam_mf_ab)

	# vegetation
		anova(gam_veg_a)
		anova(gam_veg_b)
		anova(gam_veg_g)
		anova(gam_veg_ab)




# predictions (library broom) - will be used in next script for the figures


	# MF
		# div alpha

			#new_data <- MF_grassland %>% expand( nesting(treat, year_tofit,plot))
			#MF_grassland$a_predict <- predict(gam_mf_a, new_data)

			#new_data_MF_a <- MF_grassland %>% expand(year_tofit = full_seq(year_tofit, .1))
			#new_data_MF_a$a_predict <- predict(gam_mf_a_s, new_data_MF_a)

			#MF_grassland2 <- augment(gam_mf_a , MF_grassland )
			MF_grassland2 <- augment(gam_mf_a_s , MF_grassland )



		# div beta
			new_data_MF_b <- beta_div_plot_MF_long_ok %>% expand(year1_tofit = full_seq(year1_tofit, .1))
			new_data_MF_b$b_predict <- predict(gam_mf_b_s, new_data_MF_b)

			beta_div_plot_MF_long_ok2 <- augment(gam_mf_b_s, beta_div_plot_MF_long_ok)

		# div gamma
			MF_grassland_agg_trt2 <- augment(gam_mf_g_s, MF_grassland_agg_trt)

		# abundance
			MF_grassland3 <- augment(gam_mf_ab_s, MF_grassland)


	# veg
		# div alpha
			veg_grassland2 <- augment(gam_veg_a_s, veg_grassland)
		
		# div beta
			beta_div_plot_veg_long_ok2 <- augment(gam_veg_b_s, beta_div_plot_veg_long_ok)
		
		# div gamma
			veg_grassland_agg_01_2 <- augment (gam_veg_g_s, veg_grassland_agg_01)

		# abundance
			veg_bm_2 <- augment(gam_veg_ab, veg_bm)



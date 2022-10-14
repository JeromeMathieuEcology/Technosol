### script from the paper 
### contact : Jerome Mathieu

# libraries
	library(betapart)
	library(vegan)
	library(dplyr)
	library(ggpubr)


# 1/ prep data

	# read data

		# read data
		veg_grassland <- read.table("vegetation.csv", h = T)

		veg_bm_grassland <- read.table("vegetation_biomass.csv", h = T)
		veg_bm_grassland <- na.omit(veg_bm_grassland)
		
		veg_grassland$year <- as.numeric(as.character(veg_grassland$year))
		veg_grassland$year_fac <- ordered(veg_grassland$year)
		veg_grassland$plot <- as.factor(veg_grassland$plot)


		# keep only taxa and remove taxa with 0 abundance
			veg_grassland_veg_only <- veg_grassland[,6:(ncol(veg_grassland)-1)]
			veg_grassland_veg_only <- veg_grassland_veg_only[ , colSums(veg_grassland_veg_only)>0]



	# transform to presence absence
		veg_grassland_veg_only_01 <- ifelse(veg_grassland_veg_only > 0, 1, 0)

		# set row names
			row.names(veg_grassland_veg_only_01) <-  paste(veg_grassland$treat, veg_grassland$plot, veg_grassland$year,  sep = "_")


	# check sampling design => complete design, 1 sample/plot/year 3 plots/trt, 4 years
				table(veg_grassland$plot,veg_grassland$year)
				table(veg_grassland$treat,veg_grassland$year)
				table(veg_grassland$treat,veg_grassland$plot)			
				table(veg_grassland$treat,veg_grassland$year,veg_grassland$plot)




	# aggregate data by year and treatment

			veg_grassland_agg <- aggregate(.~ year + treat, data = veg_grassland[, c(2,4,6:(ncol(veg_grassland)-1))] , sum)
	
			veg_grassland_agg_var <- veg_grassland_agg[,c(1:2, ncol(veg_grassland_agg))]
			veg_grassland_agg_veg_only <- veg_grassland_agg[,-c(1:2)]

			# transform into pres abs data
				veg_grassland_agg_veg_only_01 <- ifelse(veg_grassland_agg_veg_only > 0, 1, 0)

			# remove taxa with 0 abundance
				veg_grassland_agg_01 <- data.frame(veg_grassland_agg_var , veg_grassland_agg_veg_only_01[ , colSums(veg_grassland_agg_veg_only_01)>0])

			# set row names
				row.names(veg_grassland_agg_01) <-  paste(veg_grassland_agg_var$treat, veg_grassland_agg_var$year, sep = "_")




	# --- alpha div number of taxa per monolith ---

		veg_grassland$n_taxa <- vegan::specnumber(veg_grassland_veg_only)


	# --- gamma div ---

		veg_grassland_agg_01$n_taxa <- vegan::specnumber(veg_grassland_agg_01[,-c(1,2,(ncol(veg_grassland_agg_01)-1))])


	# --- beta div ---

	
		# at plot level : comparison among plots

				beta_div_plot_veg <- betapart::beta.pair(veg_grassland_veg_only_01[,-c(1:2)], index.family = "sorensen")

				# transformation into dataframe
						beta_div_plot_veg_sor_square <- as.data.frame(as.matrix(beta_div_plot_veg[[3]]))

				# transform into long format
					beta_div_plot_veg_long <- harrietr::melt_dist(as.matrix(beta_div_plot_veg_sor_square ))

					beta_div_plot_veg_long$treat1 <- gsub("_", "", substr(beta_div_plot_veg_long$iso1, 1, 2))
					beta_div_plot_veg_long$treat2 <- gsub("_", "", substr(beta_div_plot_veg_long$iso2, 1, 2))
					beta_div_plot_veg_long$year1 <- gsub("_", "", substr(beta_div_plot_veg_long$iso1, 6, 10))
					beta_div_plot_veg_long$year2 <- gsub("_", "", substr(beta_div_plot_veg_long$iso2, 6, 10))

				# select with treatment and year comparison
					beta_div_plot_veg_long_ok <- beta_div_plot_veg_long %>% filter(treat1==treat2 & year1 == year2)

				beta_div_plot_veg_long_ok$treat1 <- as.factor(beta_div_plot_veg_long_ok$treat1)

				beta_div_plot_veg_long_ok$year1 <- as.numeric(beta_div_plot_veg_long_ok$year1)
				beta_div_plot_veg_long_ok$year1_fac <- ordered(beta_div_plot_veg_long_ok$year1)





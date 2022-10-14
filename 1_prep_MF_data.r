### script from the paper ""
### contact : jerome.mathieu@sorbonne-universite.fr



library(betapart)
library(vegan)
library(dplyr)
library(ggpubr)


################# reprendre MF_grassland_MF_only

# 1/ prep data

	# read data with high taxonomic resolution
		MF <- read.table("macrofauna.csv", h = T)
	
		MF$year <- as.factor(MF$year)

	# remove controls
			MF_grassland <- MF[grep("R|RC",MF$treat),]

	# clean columns
		MF_grassland$year_fac <- ordered(MF_grassland$year)
		MF_grassland$year <- as.numeric(as.character(MF_grassland$year))

		MF_grassland$treat <- as.factor(MF_grassland$treat)		
		MF_grassland$plot <- as.factor(MF_grassland$plot)		
				
		MF_grassland <- droplevels(MF_grassland)	




	# matrix with taxa abundance only
		MF_grassland_MF_only <- MF_grassland[, c(7:38,40:49)]

	# remove absent taxa
		MF_grassland_MF_only <- MF_grassland_MF_only[, which(apply(MF_grassland_MF_only,2,sum) > 0)]

	# remove taxa with 0 abundance
		#MF_grassland_MF_only <- MF_grassland[,10:49]
		#MF_grassland_MF_only <- MF_grassland_MF_only[ , colSums(MF_grassland_MF_only)>0]

	# transform to presence absence
		MF_grassland_MF_only_01 <- ifelse(MF_grassland_MF_only > 0, 1, 0)

	# total fauna abundance
		MF_grassland$ab <- apply(MF_grassland_MF_only, 1, sum)




	# check sampling design => complete design, 3 samples/plot/year 3 plots/trt, 4 years
				table(MF_grassland$plot,MF_grassland$year)
				table(MF_grassland$treat,MF_grassland$year)
				table(MF_grassland$treat,MF_grassland$plot)			
				table(MF_grassland$treat,MF_grassland$year,MF_grassland$plot)



	# aggregate data by year and plot - for beta div plot level

			# MF_grassland_agg <- aggregate(.~ year + treat + plot, data = MF_grassland[, c(2,3,4,10:49)] , sum)
			MF_grassland_agg <- aggregate(.~ year + treat + plot, data = MF_grassland[, c(2,3,4,7:38,40:49)] , sum)

			# MF_grassland[, c(2,4,10:49)] %>% group_by(year, treat) %>% summarise_all(sum)

			MF_grassland_agg_var <- MF_grassland_agg[,c(1:3)]
			MF_grassland_agg_MF_only <- MF_grassland_agg[,-c(1:3)]

			# transform into pres abs data
				MF_grassland_agg_MF_only_01 <- ifelse(MF_grassland_agg_MF_only > 0, 1, 0)

			# remove taxa with 0 abundance
				MF_grassland_agg_01 <- data.frame(MF_grassland_agg_var , MF_grassland_agg_MF_only_01[ , colSums(MF_grassland_agg_MF_only_01)>0])

			# set row names
				row.names(MF_grassland_agg_01) <-  paste(MF_grassland_agg_var$year,  MF_grassland_agg_var$plot, MF_grassland_agg_var$treat, sep = "_")

				rm(MF_grassland_agg_var); rm(MF_grassland_agg_MF_only)


	
	# aggregate data by year and treatment - for gamma div

			# MF_grassland_agg_trt <- aggregate(.~ year + treat , data = MF_grassland[, c(2,4,10:49)] , sum)
			MF_grassland_agg_trt <- aggregate(.~ year + treat , data = MF_grassland[, c(2,4,7:38,40:49)] , sum)
						
			# clean varibables
				MF_grassland_agg_trt$year <- as.character(MF_grassland_agg_trt$year)
				MF_grassland_agg_trt$year <- as.numeric(MF_grassland_agg_trt$year)





# compute diversity indices


	# --- alpha div number of taxa per monolith ---

		MF_grassland$n_taxa <- vegan::specnumber(MF_grassland_MF_only)



	# --- gamma div ---

		MF_grassland_agg_trt$n_taxa <- vegan::specnumber(MF_grassland_agg_trt[,-c(1,2)])

	# --- beta div ---

		# at sample level

				beta_div_MF <- betapart::beta.pair(MF_grassland_MF_only_01, index.family="sorensen")
					# To get the pairwise turnover partition between communities, type: dist[[1]]. 
					# To get nestedness partition, type: dist[[2]]. 
					# To get all beta diversity: dist[[3]].

				# transformation in dataframe
					beta_div_MF_sor_square <- as.data.frame(as.matrix(beta_div_MF[[3]]))
					row.names(beta_div_MF_sor_square) <- paste(MF_grassland$treat, MF_grassland$sample, sep = "_")
					names(beta_div_MF_sor_square) <- paste(MF_grassland$treat, MF_grassland$sample, sep = "_")

				# transform into long format
					beta_div_MF_long <- harrietr::melt_dist(as.matrix(beta_div_MF_sor_square))
					beta_div_MF_long$treat1 <- gsub("_", "", substr(beta_div_MF_long$iso1, 0, 2))
					beta_div_MF_long$treat2 <- gsub("_", "", substr(beta_div_MF_long$iso2, 0, 2))
					beta_div_MF_long$year1 <- gsub("_", "", substr(beta_div_MF_long$iso1, 3, 7))
					beta_div_MF_long$year2 <- gsub("_", "", substr(beta_div_MF_long$iso2, 3, 7))


				# select with treatment and year comparison
					beta_div_MF_long_ok <- beta_div_MF_long %>% filter(treat1==treat2 & year1 == year2)
					beta_div_MF_long_ok$year1 <- as.numeric(beta_div_MF_long_ok$year1)

			#  multivariate homogeneity of group dispersions (variances) (compare within treat betadivs)
				plot( betadisper(beta_div_MF[[3]],MF_grassland$treat) )



		# at plot level : comparison among plots

				beta_div_plot_MF <- betapart::beta.pair(MF_grassland_agg_01[,-c(1:3)], index.family="sorensen")

				# transformation into dataframe
					beta_div_plot_MF_sor_square <- as.data.frame(as.matrix(beta_div_plot_MF[[3]]))

				# transform into long format
					beta_div_plot_MF_long <- harrietr::melt_dist(as.matrix(beta_div_plot_MF_sor_square ))

					beta_div_plot_MF_long$treat1 <- gsub("_", "", substr(beta_div_plot_MF_long$iso1, 9, 10))
					beta_div_plot_MF_long$treat2 <- gsub("_", "", substr(beta_div_plot_MF_long$iso2, 9, 10))
					beta_div_plot_MF_long$year1 <- gsub("_", "", substr(beta_div_plot_MF_long$iso1, 1, 4))
					beta_div_plot_MF_long$year2 <- gsub("_", "", substr(beta_div_plot_MF_long$iso2, 1, 4))

				# select with treatment and year comparison
					beta_div_plot_MF_long_ok <- beta_div_plot_MF_long %>% filter(treat1 == treat2 & year1 == year2)

				beta_div_plot_MF_long_ok$treat1 <- as.factor(beta_div_plot_MF_long_ok$treat1)
				
				beta_div_plot_MF_long_ok$year1 <- as.numeric(as.character(beta_div_plot_MF_long_ok$year1))
				beta_div_plot_MF_long_ok$year1_fac <- as.factor(beta_div_plot_MF_long_ok$year1)


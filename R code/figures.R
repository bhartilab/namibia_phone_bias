#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

suppl1_2_man <- function(boot=1000){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_mis <- data2016_ind %>%
						dplyr::mutate(
							phone=ifelse((other_id %in% na.omit(as.character(nodes2015$id)))==TRUE, NA, phone)) %>%
						dplyr::select(-other_id)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	data2015_mis <- data2015 %>%
						dplyr::mutate(
							phone=ifelse((as.character(id) %in% na.omit(nodes2016$other_id))==TRUE, NA, phone))

	note("\nLove plot...")
	data_imp <- mice::mice(rbind(data2015_red, data2016_ind) %>% select(-id), m=15, seed=150)
	data_ind_imp <- mice::mice(rbind(data2015, data2016_ind) %>% select(-id), m=15, seed=150)
	data_imp_2015 <- mice::mice(data2015 %>% select(-id), m=15, seed=150)
	data_imp_2016 <- mice::mice(data2016_ind %>% select(-id), m=15, seed=150)

	data_imp_2015 <- lapply(1:15, function(x){
					temp <- mice::complete(data_ind_imp, x) %>%
								filter(year==2015) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_2016 <- lapply(1:15, function(x){
						temp <- mice::complete(data_ind_imp, x) %>%
									filter(year==2016) %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	data_ind_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_ind_imp, x) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	# 2016 values instead of 2015
	mean_diff_fact <- data_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	var_num <- data_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])

	## Pooling difference testing
	z_fact <- data_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:9] <- z_fact[,2:9]/sqrt(se_fact[,2:9])

	t_num <- data_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	glob <- data.frame(
				t=apply(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num))) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Whole dataset",
					group="2016 values for participants\ninterviewed twice")

	# values of both 2015 and 2016
	mean_diff_fact <- data_ind_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_ind_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_ind_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	var_num <- data_ind_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])

	## Pooling difference testing
	z_fact <- data_ind_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_ind_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:9] <- z_fact[,2:9]/sqrt(se_fact[,2:9])

	t_num <- data_ind_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_ind_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	glob_both <- data.frame(
				t=apply(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num))) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Whole dataset",
					group="2015 and 2016 values for\nparticipants interviewed twice")

	# 2015
	mean_diff_fact <- data_imp_2015[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp_2015[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:9] <- mean_diff_fact[,2:9]/sqrt(var_fact[,2:9])

	mean_diff_num <- data_imp_2015[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	var_num <- data_imp_2015[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])

	## Pooling difference testing
	z_fact <- data_imp_2015[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_2015[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:8] <- z_fact[,2:8]/sqrt(se_fact[,2:8])

	t_num <- data_imp_2015[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_2015[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_2015 <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="Year strata",
						group="Rainy season - 2015")

	# 2016
	mean_diff_fact <- data_imp_2016[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp_2016[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:9] <- mean_diff_fact[,2:9]/sqrt(var_fact[,2:9])

	mean_diff_num <- data_imp_2016[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	var_num <- data_imp_2016[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])

	## Pooling difference testing
	z_fact <- data_imp_2016[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_2016[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:8] <- z_fact[,2:8]/sqrt(se_fact[,2:8])

	t_num <- data_imp_2016[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_2016[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_2016 <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="Year strata",
						group="Dry season - 2016")


	note("\nMean reduction...")
	note("\nPooled estimate based on matching...")
	data_imp <- mice::mice(rbind(data2015_red, data2016_ind), m=15, seed=1234, print=FALSE)
	data_ind_imp <- mice::mice(rbind(data2015, data2016_ind), m=15, seed=1234, print=FALSE)
	data_imp_man <- mice::mice(rbind(data2015, data2016), m=15, seed=1234, print=FALSE)

	# data_imp <- mice::mice(rbind(data2015, data2016), m=15, seed=150, print=FALSE)
	# data_imp_ind <- mice::mice(rbind(data2015, data2016_ind), m=15, seed=150, print=FALSE)

	trim_iter <- function(imp){
		mod_mat <- stats::model.matrix(phone~age+sex+dest+child_house+adult_house+travel+clinic_time+year+hsv, data=imp)
		imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
		colnames(imp) <- gsub("-", "_", colnames(imp))
		colnames(imp) <- gsub(">=", "_", colnames(imp))
		colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
		colnames(imp) <- gsub("\\.", "_", colnames(imp))
		repeat{
			n <- nrow(imp)
			mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
				data=imp)
			check <- apply(mod_mat, 2, mean) # Initiating a check
			check <- !(check==1 | check==0) & names(check)!="clinic_time"
			form <- paste0("phone~", paste(names(check)[check], collapse="+"))
			mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
			imp$p <- stats::predict(mod, type="response")
			cutoff_low <- min(imp$p[imp$phone==1])
			cutoff_high <- max(imp$p[imp$phone==0])
			imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
			n_trim <- nrow(imp)
			if(n_trim==n){
				break
			}
		}
		return(imp)
	}

	data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	data_ind_imp <- lapply(1:15, function(x){mice::complete(data_ind_imp, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	data_imp_man <- lapply(1:15, function(x){mice::complete(data_imp_man, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	for(i in 1:length(data_imp)){
		data_imp[[i]]$m <- i
	}
	for(i in 1:length(data_ind_imp)){
		data_ind_imp[[i]]$m <- i
	}
	for(i in 1:length(data_imp_man)){
		data_imp_man[[i]]$m <- i
	}

	# Doing the calculation with matching
	match_est <- data_imp %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_combined <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_combined$varest)
	var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

	match_combined <- rbind(match_combined,
							data.frame(m=99,
								est=mean(match_combined$est),
								varest=var_within+(1+(1/nrow(match_combined)))*var_between))

	match_est <- data_ind_imp %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_ind_combined <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_ind_combined$varest)
	var_between <- (1/(nrow(match_ind_combined)-1))*sum((match_ind_combined$est-mean(match_ind_combined$est))^2)

	match_ind_combined <- rbind(match_ind_combined,
							data.frame(m=99,
								est=mean(match_ind_combined$est),
								varest=var_within+(1+(1/nrow(match_ind_combined)))*var_between))

	match_est <- data_imp_man %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_combined_man <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_combined_man$varest)
	var_between <- (1/(nrow(match_combined_man)-1))*sum((match_combined_man$est-mean(match_combined_man$est))^2)

	match_combined_man <- rbind(match_combined_man,
							data.frame(m=99,
								est=mean(match_combined_man$est),
								varest=var_within+(1+(1/nrow(match_combined_man)))*var_between))

	match_est <- data_imp_man %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_combined_man <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_combined_man$varest)
	var_between <- (1/(nrow(match_combined_man)-1))*sum((match_combined_man$est-mean(match_combined_man$est))^2)

	match_combined_man <- rbind(match_combined_man,
							data.frame(m=99,
								est=mean(match_combined_man$est),
								varest=var_within+(1+(1/nrow(match_combined_man)))*var_between))

	note("Plotting...\n")
	redu <- rbind(
				match_combined_man[16,] %>% dplyr::mutate(type="2015 values for participants\ninterviewed twice", col="black"),
				match_combined[16,] %>% dplyr::mutate(type="2016 values for participants\ninterviewed twice", col=viridis::viridis(4, option="D")[2]),
				match_ind_combined[16,] %>% dplyr::mutate(type="2015 and 2016 values for\nparticipants interviewed twice", col=viridis::viridis(4, option="D")[1])) %>%
				dplyr::mutate(
					type=factor(type,
						levels=c("2015 and 2016 values for\nparticipants interviewed twice",
							"2016 values for participants\ninterviewed twice",
							"2015 values for participants\ninterviewed twice"))) %>%
				ggplot2::ggplot(., ggplot2::aes(x=type, y=est, color=I(col))) +
					ggplot2::geom_errorbar(
						ggplot2::aes(ymin=est-1.96*sqrt(varest), ymax=est+1.96*sqrt(varest)),
						width=0.05) +
					ggplot2::geom_hline(yintercept = 0, linetype="dashed") +
					ggplot2::geom_point(
						shape=21,
						stroke=1.5,
						size=3) +
					# ggplot2::scale_color_manual(
					# 	name="",
					# 	breaks=c("Whole\ndataset", "Men\nstratum", "Women\nstratum",
					# 		"Rainy season -\n2015 stratum", "Dry season -\n2016 stratum"),
					# 	values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2],
					# 		"gray65", "gray50")) +
					ggplot2::guides(colour="none") +
					ggplot2::coord_flip() +
					ggplot2::ylim(c(-7, 2)) +
					ggplot2::xlab("") +
					ggplot2::ylab("Mean reduction in time (hours) necessary to reach a health care\ncenter for mobile phone owners compared to non-phone owners") +
					ggplot2::theme_bw()

	love_fig <- rbind(glob,
					glob_both,
					stratum_2015,
					stratum_2016) %>%
					dplyr::mutate(
						type=factor(type,
							levels=c("Whole dataset", "Year strata")),
						var=factor(var,
						levels=c("age16_25","age26_35", "age36_45", "age46_59", "age_60", "sexM", "hsv", "clinic_unable", "clinic_time", "travelCar", "dest"),
						labels=c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
							"Proportion of men", "HSV shedding", "Unable to access a\nhealth care center", "Time to reach a\nhealth care center",
							"Travel using a car", "Number of\ndestinations reached"))) %>%
					ggplot2::ggplot(.,
						ggplot2::aes(x=t, y=var, color=group)) +
						ggplot2::geom_point(
							ggplot2::aes(shape=p2),
							# position=ggplot2::position_dodge(width=1),
							size=2,
							position=ggplot2::position_dodge(width=0.5)) +
					# ggplot2::geom_point(
					# 	ggplot2::aes(shape=I(shape)),
					# 	stroke=2,
					# 	size=3,
					# 	position=ggplot2::position_dodge(width=0.5)) +
					ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
					viridis::scale_color_viridis(
						name="",
						option="D",
						discrete=TRUE) +
					# ggplot2::facet_wrap(.~type, ncol=2) +
					ggplot2::scale_shape_manual(
						values=c(21,16)) +
					ggplot2::guides(
						shape=ggplot2::guide_legend(
							title="",
							order=1),
						color=ggplot2::guide_legend(
							title="",
							order=2)) +
					ggplot2::xlab("Standardized difference between mobile phone owners and non-owners") +
					ggplot2::ylab("") +
					ggplot2::theme_bw()

	ggplot2::ggsave(
		file.path(path.workspace, "suppl1_manuscript.tiff"),
		love_fig,
		width=12,
		height=8)

	ggplot2::ggsave(
		file.path(path.workspace, "suppl2_manuscript.tiff"),
		redu,
		width=6.5,
		height=4.5)

	note("\nFigure saved...")

	note("\nTable saved...")
	redu_table <- rbind(
					match_combined_man[16,] %>% dplyr::mutate(type="2015 values for participants\ninterviewed twice"),
					match_combined[16,] %>% dplyr::mutate(type="2016 values for participants\ninterviewed twice"),
					match_ind_combined[16,] %>% dplyr::mutate(type="2015 and 2016 values for\nparticipants interviewed twice")) %>%
					dplyr::mutate(
						red=round(est, 2),
						ci=paste(round(est-1.96*sqrt(varest), 2), round(est+1.96*sqrt(varest), 2), sep="-")) %>%
					dplyr::select(type, red, ci)

	res <- redu_table
	title <- "mean reduction plot"
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=file.path(path.workspace, "suppl_redu_table.html"), output_format = rmarkdown::html_document(toc=FALSE))

}

#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

suppl_fig1_man <- function(boot=1000, file=file.path(path.workspace, "suppl_fig1_manuscript")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_mis <- data2016_ind %>%
						dplyr::mutate(
							phone=ifelse((other_id %in% na.omit(as.character(nodes2015$id)))==TRUE, NA, phone)) %>%
						dplyr::select(-other_id)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	data2015_mis <- data2015 %>%
						dplyr::mutate(
							phone=ifelse((as.character(id) %in% na.omit(nodes2016$other_id))==TRUE, NA, phone))

	note("\nLove plot...\n")
	data_imp <- mice::mice(rbind(data2015, data2016) %>% select(-id), m=15, seed=150)
	data_ind_imp <- mice::mice(rbind(data2015, data2016_ind) %>% select(-id), m=15, seed=150)

	data_imp_M <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								filter(sex=="M") %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_F <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									filter(sex=="F") %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_2015 <- lapply(1:15, function(x){
					temp <- mice::complete(data_ind_imp, x) %>%
								filter(year==2015) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_2016 <- lapply(1:15, function(x){
						temp <- mice::complete(data_ind_imp, x) %>%
									filter(year==2016) %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	comp <- function(temp, type, group){
		mean_diff_fact <- temp[, c(1:7,10:13,15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(age16_25=mean(age16_25),
							age26_35=mean(age26_35),
							age36_45=mean(age36_45),
							age46_59=mean(age46_59),
							age_60=mean(age_60),
							sexM=mean(sexM),
							clinic_unable=mean(clinic_unable),
							travelCar=mean(travelCar),
							d_child0=mean(d_child0),
							hsv=mean(hsv)) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(age16_25=diff(age16_25),
							age26_35=diff(age26_35),
							age36_45=diff(age36_45),
							age46_59=diff(age46_59),
							age_60=diff(age_60),
							sexM=diff(sexM),
							clinic_unable=diff(clinic_unable),
							travelCar=diff(travelCar),
							d_child0=diff(d_child0),
							hsv=diff(hsv))

		var_fact <- temp[, c(1:7,10:13,15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::mutate(age16_25=mean(age16_25),
							age26_35=mean(age26_35),
							age36_45=mean(age36_45),
							age46_59=mean(age46_59),
							age_60=mean(age_60),
							sexM=mean(sexM),
							clinic_unable=mean(clinic_unable),
							travelCar=mean(travelCar),
							d_child0=mean(d_child0),
							hsv=mean(hsv)) %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
							age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
							age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
							age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
							age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
							sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
							clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
							travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
							d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
							hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(age16_25=sum(age16_25),
							age26_35=sum(age26_35),
							age36_45=sum(age36_45),
							age46_59=sum(age46_59),
							age_60=sum(age_60),
							sexM=sum(sexM),
							clinic_unable=sum(clinic_unable),
							travelCar=sum(travelCar),
							d_child0=sum(d_child0),
							hsv=sum(hsv))

		mean_diff_fact[,2:11] <- mean_diff_fact[,2:11]/sqrt(var_fact[,2:11])

		mean_diff_num <- temp[, c(1, 8:9, 14:15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(dest=mean(dest),
							clinic_time=mean(clinic_time),
							adult_house=mean(adult_house)) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(dest=diff(dest),
							clinic_time=diff(clinic_time),
							adult_house=diff(adult_house))

		var_num <- temp[, c(1, 8:9, 14:15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(dest=var(dest)/dplyr::n(),
							clinic_time=var(clinic_time)/dplyr::n(),
							adult_house=var(adult_house)/dplyr::n()) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(dest=sum(dest),
							clinic_time=sum(clinic_time),
							adult_house=sum(adult_house))

		mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

		## Pooling difference testing
		z_fact <- temp[, c(1:7,10:13,15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(age16_25=mean(age16_25),
							age26_35=mean(age26_35),
							age36_45=mean(age36_45),
							age46_59=mean(age46_59),
							age_60=mean(age_60),
							sexM=mean(sexM),
							clinic_unable=mean(clinic_unable),
							travelCar=mean(travelCar),,
							d_child0=mean(d_child0),
							hsv=mean(hsv)) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(age16_25=diff(age16_25),
							age26_35=diff(age26_35),
							age36_45=diff(age36_45),
							age46_59=diff(age46_59),
							age_60=diff(age_60),
							sexM=diff(sexM),
							clinic_unable=diff(clinic_unable),
							travelCar=diff(travelCar),
							d_child0=diff(d_child0),,
							hsv=diff(hsv))

		between_var_fact <- apply(z_fact[,-1], 2, function(x){
			(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
		})

		se_fact <- temp[, c(1:7,10:13,15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::mutate(n=dplyr::n()) %>%
						dplyr::group_by(m) %>%
						dplyr::mutate(n1=min(n), n2=max(n)) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(age16_25=mean(age16_25),
							age26_35=mean(age26_35),
							age36_45=mean(age36_45),
							age46_59=mean(age46_59),
							age_60=mean(age_60),
							sexM=mean(sexM),
							clinic_unable=mean(clinic_unable),
							travelCar=mean(travelCar),
							d_child0=mean(d_child0),,
							hsv=mean(hsv),
							n1=mean(n1),
							n2=mean(n2)) %>%
						dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
							age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
							age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
							age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
							age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
							sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
							clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
							travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
							d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),,
							hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
						dplyr::select(m:hsv)

		within_var_fact <- apply(se_fact[,-1], 2, mean)
		r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
		df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

		# z_fact[,2:9] <- z_fact[,2:9]/sqrt(se_fact[,2:9])

		t_num <- temp[, c(1, 8:9, 14:15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(dest=mean(dest),
							clinic_time=mean(clinic_time),
							adult_house=mean(adult_house)) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(dest=diff(dest),
							clinic_time=diff(clinic_time),
							adult_house=diff(adult_house))

		between_var_num <- apply(t_num[,-1], 2, function(x){
			(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
		})

		se_num <- temp[, c(1, 8:9, 14:15)] %>%
						dplyr::group_by(m, phone) %>%
						dplyr::mutate(n=dplyr::n()) %>%
						dplyr::group_by(m, phone) %>%
						dplyr::summarise(dest=var(dest),
							clinic_time=var(clinic_time),
							adult_house=var(adult_house),
							n=mean(n)) %>%
						dplyr::mutate(dest=dest/n,
							clinic_time=clinic_time/n,
							adult_house=adult_house/n) %>%
						dplyr::group_by(m) %>%
						dplyr::summarise(dest=sum(dest),
							clinic_time=sum(clinic_time),
							adult_house=sum(adult_house))

		within_var_num <- apply(se_num[,-1], 2, mean)
		r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
		df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

		result <- data.frame(
					t=apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean),
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num)))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type=type,
						group=group)
		return(result)
	}

	stratum_F <- comp(data_imp_F, "Gender strata", "Women")
	stratum_M <- comp(data_imp_M, "Gender strata", "Men")
	stratum_2015 <- comp(data_imp_2015, "Yearly strata", 2015)
	stratum_2016 <- comp(data_imp_2016, "Yearly strata", 2016)

	note("Plotting...\n")
	love_figA <- rbind(
					stratum_M[-6,],
					stratum_F[-6,]) %>%
					dplyr::mutate(
						group=factor(group,
							levels=c("Women", "Men")), 
						var=factor(var,
						levels=rev(c("age16_25","age26_35", "age36_45", "age46_59", "age_60", "sexM", "adult_house", "hsv", "d_child0", "clinic_unable", "clinic_time", "travelCar", "dest")),
						labels=rev(c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
							"Proportion of men", "Number of adults\nin the household","HSV shedding",
							"Reported at least\none deceased child", "Unable to access a\nhealth care center", "Travel time to a\nhealth care center",
							"Able to travel to a health\ncare center with a car", "Number of\ntravel destinations")))) %>%
					ggplot2::ggplot(., ggplot2::aes(x=t, y=var)) +
						ggplot2::geom_point(
							ggplot2::aes(shape=p2),
							# position=ggplot2::position_dodge(width=1),
							stroke=1,
							size=2) +
						ggplot2::scale_shape_manual(values=c(16, 21)) +
						ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
						ggplot2::facet_grid(.~group) +
						ggplot2::guides(shape=ggplot2::guide_legend(title="")) +
						ggplot2::xlab("Standardized difference between mobile phone owners and non-phone owners") +
						ggplot2::ylab("") +
						ggplot2::theme_bw() +
						ggplot2::theme(legend.position="none")

	love_figB <- rbind(
					stratum_2015,
					stratum_2016) %>%
					dplyr::mutate(
						group=factor(group,
							levels=2015:2016,
							labels=c("2015 - Rainy season", "2016 - Dry season")), 
						var=factor(var,
						levels=rev(c("age16_25","age26_35", "age36_45", "age46_59", "age_60", "sexM", "adult_house", "hsv", "d_child0", "clinic_unable", "clinic_time", "travelCar", "dest")),
						labels=rev(c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
							"Proportion of men", "Number of adults\nin the household","HSV shedding",
							"Reported at least\none deceased child", "Unable to access a\nhealth care center", "Travel time to a\nhealth care center",
							"Able to travel to a health\ncare center with a car", "Number of\ntravel destinations")))) %>%
					ggplot2::ggplot(., ggplot2::aes(x=t, y=var)) +
						ggplot2::geom_point(
							ggplot2::aes(shape=p2),
							# position=ggplot2::position_dodge(width=1),
							stroke=1,
							size=2) +
						ggplot2::scale_shape_manual(values=c(16, 21)) +
						ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
						ggplot2::facet_grid(.~group) +
						ggplot2::guides(shape=ggplot2::guide_legend(title="")) +
						ggplot2::xlab("Standardized difference between mobile phone owners and non-phone owners") +
						ggplot2::ylab("") +
						ggplot2::theme_bw() +
						ggplot2::theme(legend.position="none")

	love_table_sex <- cbind(
					stratum_F[-6, c(2,1,3)] %>%
						dplyr::mutate(
							t=round(t, digit=3),
							p=round(p, digit=3)),
					stratum_M[-6, c(1,3)] %>%
						dplyr::mutate(
							t=round(t, digit=3),
							p=round(p, digit=3)))
	empty <- rep(NA, 4)
	love_table <- cbind(
					stratum_2015[, c(2,1,3)] %>%
						dplyr::mutate(
							t=round(t, digit=3),
							p=round(p, digit=3)),
					stratum_2016[, c(1,3)] %>%
						dplyr::mutate(
							t=round(t, digit=3),
							p=round(p, digit=3)))
	love_table <- cbind(
					love_table,
					rbind(
						love_table_sex[1:5,-1],
						empty,
						love_table_sex[6:12,-1]))

	fig1 <- multipanelfigure::multi_panel_figure(
				width=240,
				height=c(120, 120),
				columns=1) %>%
				multipanelfigure::fill_panel(
					love_figB,
					label="A",
					row=1, column=1) %>%
				multipanelfigure::fill_panel(
					love_figA, label="B",
					row=2, column=1) %>%
				multipanelfigure::save_multi_panel_figure(paste0(file, ".tiff"), dpi=300)
	note("Figure saved...\n")

	note("Table saved...\n")
	res <- love_table
	title <- "love plot"
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "_table.html"), output_format = rmarkdown::html_document(toc=FALSE))

}


#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

comp_man <- function(boot=1000, file=file.path(path.workspace, "comp_manuscript.csv")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2015$phone_use_ever <- ifelse(nodes2015$phone_use_ever=="No data", NA,
									ifelse(nodes2015$phone_use_ever=="Already used a phone", 1, 0))
	nodes2016$phone_use_ever <- ifelse(nodes2016$phone_use_ever=="No data", NA,
									ifelse(nodes2016$phone_use_ever=="Already used a phone", 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_mis <- data2016_ind %>%
						dplyr::mutate(
							phone=ifelse((other_id %in% na.omit(as.character(nodes2015$id)))==TRUE, NA, phone)) %>%
						dplyr::select(-other_id)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	data2015_mis <- data2015 %>%
						dplyr::mutate(
							phone=ifelse((as.character(id) %in% na.omit(nodes2016$other_id))==TRUE, NA, phone))

	data_imp <- mice::mice(rbind(data2015, data2016) %>% select(-id), m=15, seed=150)
	data_ind_imp <- mice::mice(rbind(data2015, data2016_ind) %>% select(-id), m=15, seed=150)

	data_imp_s <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x)
					temp <- cbind(sex=temp$sex,
								data.frame(stats::model.matrix(sex~phone+phone_use_ever, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_y <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x)
					temp <- cbind(year=temp$year,
								data.frame(stats::model.matrix(year~phone+phone_use_ever, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	# Sex difference
	mean_diff_fact <- data_imp_s %>%
					dplyr::mutate(sex=factor(sex, levels=c("F", "M"))) %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=diff(phone),
						phone_use_ever=diff(phone_use_ever))

	var_fact <- data_imp_s %>%
					dplyr::mutate(sex=factor(sex, levels=c("F", "M"))) %>%
					dplyr::group_by(m, sex) %>%
					dplyr::mutate(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever)) %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						phone=mean(phone)*(1-mean(phone))/dplyr::n(),
						phone_use_ever=mean(phone_use_ever)*(1-mean(phone_use_ever))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=sum(phone),
						phone_use_ever=sum(phone_use_ever))

	mean_diff_fact[,2:3] <- mean_diff_fact[,2:3]/sqrt(var_fact[,2:3])

	## Pooling difference testing
	z_fact <- data_imp_s %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=diff(phone),
						phone_use_ever=diff(phone_use_ever))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_s %>%
					dplyr::group_by(m, sex) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(
						phone=phone*(1-phone)*((1/n1)+(1/n2)),
						phone_use_ever=phone_use_ever*(1-phone_use_ever)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:phone_use_ever)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	sex_comp <- data.frame(
				t=apply(mean_diff_fact[,-1], 2, mean),
				var=colnames(mean_diff_fact)[-1],
				p=apply(z_fact[,-1], 2, mean)^2) %>%
				dplyr::mutate(p=p/(within_var_fact+(1+(1/max(z_fact$m)))*between_var_fact)) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=df_fact)) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Sex comparison")

	# Sex difference
	mean_diff_fact <- data_imp_s %>%
					dplyr::mutate(sex=factor(sex, levels=c("F", "M"))) %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=diff(phone),
						phone_use_ever=diff(phone_use_ever))

	var_fact <- data_imp_s %>%
					dplyr::mutate(sex=factor(sex, levels=c("F", "M"))) %>%
					dplyr::group_by(m, sex) %>%
					dplyr::mutate(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever)) %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						phone=mean(phone)*(1-mean(phone))/dplyr::n(),
						phone_use_ever=mean(phone_use_ever)*(1-mean(phone_use_ever))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=sum(phone),
						phone_use_ever=sum(phone_use_ever))

	mean_diff_fact[,2:3] <- mean_diff_fact[,2:3]/sqrt(var_fact[,2:3])

	## Pooling difference testing
	z_fact <- data_imp_s %>%
					dplyr::group_by(m, sex) %>%
					dplyr::summarise(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=diff(phone),
						phone_use_ever=diff(phone_use_ever))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_s %>%
					dplyr::group_by(m, sex) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(
						phone=phone*(1-phone)*((1/n1)+(1/n2)),
						phone_use_ever=phone_use_ever*(1-phone_use_ever)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:phone_use_ever)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	sex_comp <- data.frame(
				t=apply(mean_diff_fact[,-1], 2, mean),
				var=colnames(mean_diff_fact)[-1],
				p=apply(z_fact[,-1], 2, mean)^2) %>%
				dplyr::mutate(p=p/(within_var_fact+(1+(1/max(z_fact$m)))*between_var_fact)) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=df_fact)) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Sex comparison")

	# Year difference
	mean_diff_fact <- data_imp_y %>%
					dplyr::group_by(m, year) %>%
					dplyr::summarise(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=diff(phone),
						phone_use_ever=diff(phone_use_ever))

	var_fact <- data_imp_y %>%
					dplyr::group_by(m, year) %>%
					dplyr::mutate(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever)) %>%
					dplyr::group_by(m, year) %>%
					dplyr::summarise(
						phone=mean(phone)*(1-mean(phone))/dplyr::n(),
						phone_use_ever=mean(phone_use_ever)*(1-mean(phone_use_ever))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=sum(phone),
						phone_use_ever=sum(phone_use_ever))

	mean_diff_fact[,2:3] <- mean_diff_fact[,2:3]/sqrt(var_fact[,2:3])

	## Pooling difference testing
	z_fact <- data_imp_y %>%
					dplyr::group_by(m, year) %>%
					dplyr::summarise(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=diff(phone),
						phone_use_ever=diff(phone_use_ever))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_y %>%
					dplyr::group_by(m, year) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						phone=mean(phone),
						phone_use_ever=mean(phone_use_ever),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(
						phone=phone*(1-phone)*((1/n1)+(1/n2)),
						phone_use_ever=phone_use_ever*(1-phone_use_ever)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:phone_use_ever)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	year_comp <- data.frame(
				t=apply(mean_diff_fact[,-1], 2, mean),
				var=colnames(mean_diff_fact)[-1],
				p=apply(z_fact[,-1], 2, mean)^2) %>%
				dplyr::mutate(p=p/(within_var_fact+(1+(1/max(z_fact$m)))*between_var_fact)) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=df_fact)) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Year comparison")

	write.table(rbind(sex_comp, year_comp), file, row.names=FALSE)
	note('Results saved...\n')

}


#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

fig1_man <- function(boot=1000, file=file.path(path.workspace, "fig1_manuscript")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_mis <- data2016_ind %>%
						dplyr::mutate(
							phone=ifelse((other_id %in% na.omit(as.character(nodes2015$id)))==TRUE, NA, phone)) %>%
						dplyr::select(-other_id)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	data2015_mis <- data2015 %>%
						dplyr::mutate(
							phone=ifelse((as.character(id) %in% na.omit(nodes2016$other_id))==TRUE, NA, phone))

	note("Love plot...\n")
	data_imp <- mice::mice(rbind(data2015, data2016) %>% select(-id), m=15, seed=150)
	data_ind_imp <- mice::mice(rbind(data2015, data2016_ind) %>% select(-id), m=15, seed=150)

	data_imp_M <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								filter(sex=="M") %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_F <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									filter(sex=="F") %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	mean_diff_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	mean_diff_fact_table <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv))

	var_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,2:11] <- mean_diff_fact[,2:11]/sqrt(var_fact[,2:11])

	mean_diff_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	mean_diff_num_table <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house))

	var_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),,
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),,
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	# total_var_fact <- within_var_fact+(1+(1/nrow(z_fact)))*between_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:9] <- z_fact[,2:9]/sqrt(se_fact[,2:9])

	t_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	# total_var_num <- within_var_num+(1+(1/nrow(t_num)))*between_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	glob <- data.frame(
				t=apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num))) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Whole dataset",
					group="Whole dataset",
					stroke=2)

	glob_table <- data.frame(
				t=apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num)))

	# Men
	mean_diff_fact <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	mean_diff_fact_table_M <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv))

	var_fact <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),,
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	mean_diff_num_table_M <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house))

	var_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp_M[, c(1:6,9:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_M[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:8] <- z_fact[,2:8]/sqrt(se_fact[,2:8])

	t_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_M[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_M <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="Gender strata",
						group="Men",
						stroke=2) %>%
					dplyr::filter(var %in% c("clinic_time", "dest")) %>%
					dplyr::mutate(var=ifelse(var=="clinic_time", "clinic_time_m", "dest_m"))

	stratum_table_M <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))

	# Women
	mean_diff_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	mean_diff_fact_table_F <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv))

	var_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	mean_diff_num_table_F <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house))

	var_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_F[, c(1:6,9:12, 14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:8] <- z_fact[,2:8]/sqrt(se_fact[,2:8])

	t_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_F[, c(1, 7:8, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_F <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="Gender strata",
						group="Women",
						stroke=2) %>%
					dplyr::filter(var %in% c("clinic_time", "dest")) %>%
					dplyr::mutate(var=ifelse(var=="clinic_time", "clinic_time_f", "dest_f"))

	stratum_table_F <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))

	note("Plotting...\n")
	love_fig <- rbind(glob,
					stratum_M,
					stratum_F) %>%
					dplyr::mutate(
						type=factor(type,
							levels=c("Whole dataset", "Gender strata")),
						var=factor(var,
						levels=rev(c("age16_25","age26_35", "age36_45", "age46_59", "age_60", "sexM", "adult_house", "hsv", "d_child0", "clinic_unable", "clinic_time",
							"clinic_time_m", "clinic_time_f","travelCar", "dest", "dest_m", "dest_f")),
						labels=rev(c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
							"Proportion of men", "Number of adults\nin the household","HSV shedding",
							"Reported at least\none deceased child", "Unable to access a\nhealth care center",
							# "Travel time to a\nhealth care center", "Travel time to a health\ncare center - Men", "Travel time to a health\ncare center - Women",
							"Travel time to a\nhealth care center", "Travel time to a health\ncare center (M/F)", "Travel time to a health\ncare center (M/F)",
							"Able to travel to a health\ncare center with a car",
							"Number of travel\ndestinations", "Number of travel\ndestinations (M/F)", "Number of travel\ndestinations (M/F)"))),
						# fill=ifelse(group=="Men", RColorBrewer::brewer.pal(3, "RdBu")[1],
						# 	ifelse(group=="Women", RColorBrewer::brewer.pal(3, "RdBu")[3], "black")),
						fill=ifelse(p>0.05, NA, ifelse(group=="Men", "red", ifelse(group=="Women", "blue", "black"))),
						fake=ifelse(p>0.05, "p>0.05", "p<=0.05")) %>%
					# dplyr::mutate(fill=ifelse(p2=="p<=0.05", fill, NA)) %>%
					ggplot2::ggplot(., ggplot2::aes(x=t, y=var, color=group)) +
						ggplot2::geom_point(
							# ggplot2::aes(shape=p2, fill=I(fill), stroke=I(stroke)),
							ggplot2::aes(fill=I(fill), group=p2),
							stroke=0.5,
							shape=21,
							# position=ggplot2::position_dodge(width=1),
							size=0.7) +
						ggplot2::geom_point(
							ggplot2::aes(shape=fake, group=fake),
							alpha=0) +
						ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
						ggplot2::scale_shape_manual(
							breaks=c("p<=0.05", "p>0.05"),
							values=c(16,21)) +
						ggplot2::scale_color_manual(
							name="",
							breaks=c("Whole dataset", "Men", "Women"),
							values=c("black", "red", "blue")) +
							# values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
						# ggplot2::scale_fill_manual(
						# 	name="",
						# 	breaks=c("p<=0.05", "p>0.05"),
						# 	values=c("black", NA, NA)) +
						# ggplot2::facet_grid(.~type) +
						ggplot2::guides(
							shape=ggplot2::guide_legend(
								title="p-value",
								override.aes = list(
									size= 0.5,
									alpha=1)),
							color=ggplot2::guide_legend(
								title="Strata",
								override.aes=list(
									size=0.5,
									stroke=0.75))) +
						ggplot2::xlab("Standardized difference between mobile phone owners and non-phone owners") +
						ggplot2::ylab("") +
						ggplot2::theme_bw() +
						ggplot2::theme(
							legend.position = c(0.88, 0.72),
							legend.box.background = ggplot2::element_rect(
								colour = "black",
								fill="white"),
							# legend.spacing.y = unit(0.2, 'cm'),
							legend.key.size = unit(0.5, 'lines'),
							legend.margin = ggplot2::margin(0, 1, 1, 1),
							legend.box.margin = margin(0, 1, 1, 1),
							legend.text=ggplot2::element_text(size=4),
							legend.title=ggplot2::element_text(size=4.3),
							axis.text.y = ggplot2::element_text(size = 3),
							axis.text.x = ggplot2::element_text(size = 3),
							axis.title.x = ggplot2::element_text(size = 6))
							# axis.text.y = ggplot2::element_text(size = 16),
							# axis.text.x = ggplot2::element_text(size = 14),
							# axis.title.x = ggplot2::element_text(size = 19))

	love_fig_light <- rbind(glob,
					stratum_M,
					stratum_F) %>%
					dplyr::filter(
						!(var %in% c("age16_25", "age36_45", "age46_59",
							"adult_house", "hsv", "d_child0","travelCar"))) %>%
					dplyr::mutate(
						type=factor(type,
							levels=c("Whole dataset", "Gender strata")),
						var=factor(ifelse(var %in% c("clinic_time_m", "clinic_time_f"), "clinic_time_m_f",
							ifelse(var %in% c("dest_m", "dest_f"), "dest_m_f", var)),
						levels=rev(c("age26_35", "age_60", "sexM",
							"clinic_unable", "clinic_time", "clinic_time_m_f",
							"dest", "dest_m_f")),
						labels=rev(c("26-35 years", "60+ years",
							"Proportion of men",
							"Unable to access a\nhealth care center",
							"Travel time to a\nhealth care center", "Travel time to a health\ncare center (M/F)",
							"Number of travel\ndestinations", "Number of travel\ndestinations (M/F)"))),
						# fill=ifelse(group=="Men", RColorBrewer::brewer.pal(3, "RdBu")[1],
						# 	ifelse(group=="Women", RColorBrewer::brewer.pal(3, "RdBu")[3], "black")),
						fill=ifelse(p>0.05, NA, ifelse(group=="Men", "red", ifelse(group=="Women", "blue", "black"))),
						fake=ifelse(p>0.05, "p>0.05", "p<=0.05")) %>%
					# dplyr::mutate(fill=ifelse(p2=="p<=0.05", fill, NA)) %>%
					ggplot2::ggplot(., ggplot2::aes(x=t, y=var, color=group)) +
						ggplot2::geom_point(
							# ggplot2::aes(shape=p2, fill=I(fill), stroke=I(stroke)),
							ggplot2::aes(fill=I(fill), group=p2),
							stroke=0.5,
							shape=21,
							# position=ggplot2::position_dodge(width=1),
							size=0.7) +
						ggplot2::geom_point(
							ggplot2::aes(shape=fake, group=fake),
							alpha=0) +
						ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
						ggplot2::scale_shape_manual(
							breaks=c("p<=0.05", "p>0.05"),
							values=c(16,21)) +
						ggplot2::scale_color_manual(
							name="",
							breaks=c("Whole dataset", "Men", "Women"),
							values=c("black", "red", "blue")) +
							# values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
						# ggplot2::scale_fill_manual(
						# 	name="",
						# 	breaks=c("p<=0.05", "p>0.05"),
						# 	values=c("black", NA, NA)) +
						# ggplot2::facet_grid(.~type) +
						ggplot2::guides(
							shape=ggplot2::guide_legend(
								title="p-value",
								override.aes = list(
									size= 0.5,
									alpha=1)),
							color=ggplot2::guide_legend(
								title="Strata",
								override.aes=list(
									size=0.5,
									stroke=0.75))) +
						ggplot2::xlab("Standardized difference between mobile phone owners and non-owners") +
						ggplot2::ylab("") +
						ggplot2::theme_bw() +
						ggplot2::theme(
							legend.position = c(0.88, 0.72),
							legend.box.background = ggplot2::element_rect(
								colour = "black",
								fill="white"),
							# legend.spacing.y = unit(0.2, 'cm'),
							legend.key.size = unit(0.5, 'lines'),
							legend.margin = ggplot2::margin(0, 1, 1, 1),
							legend.box.margin = margin(0, 1, 1, 1),
							legend.text=ggplot2::element_text(size=4),
							legend.title=ggplot2::element_text(size=4.3),
							axis.text.y = ggplot2::element_text(size = 3),
							axis.text.x = ggplot2::element_text(size = 3),
							axis.title.x = ggplot2::element_text(size = 6))
							# axis.text.y = ggplot2::element_text(size = 16),
							# axis.text.x = ggplot2::element_text(size = 14),
							# axis.title.x = ggplot2::element_text(size = 19))

	love_table <- glob[, c(2, 1, 3)]
	love_table[,2] <- round(love_table[,2], digit=3)
	love_table[,3] <- round(love_table[,3], digit=3)
	love_table_sex <- rbind(
					stratum_F[, c(1:3)],
					stratum_M[, c(1:3)])
	empty <- rep(NA, 3)
	love_table <- rbind(
					empty,
					love_table[1:5,],
					empty,
					empty,
					love_table[6,],
					empty,
					empty,
					love_table[7,],
					empty,
					empty,
					love_table[11,],
					empty,
					empty,
					love_table[12,],
					empty,
					empty,
					love_table[13,],
					empty,
					empty,
					love_table[9,],
					empty,
					empty,
					love_table[8,],
					empty,
					empty,
					love_table[10,])
	love_table_sex <- rbind(
						empty,
						love_table_sex[c(1,3),],
						empty,
						love_table_sex[c(2,4),])


	mean_diff_fact_table <- mean_diff_fact_table %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(100*value, 1))
	
	mean_diff_fact_table <- cbind(
								mean_diff_fact_table %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_fact_table %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_num_table <- mean_diff_num_table %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(value, 1))
	mean_diff_num_table <- cbind(
								mean_diff_num_table %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_num_table %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_table <- rbind(
						mean_diff_fact_table,
						mean_diff_num_table) %>%
						dplyr::left_join(.,
							glob_table %>%
								dplyr::select(-t),
							by="var") %>%
						dplyr::mutate(p=round(p,3))

	mean_diff_fact_table_M <- mean_diff_fact_table_M %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(100*value, 1))
	mean_diff_fact_table_M <- cbind(
								mean_diff_fact_table_M %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_fact_table_M %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_num_table_M <- mean_diff_num_table_M %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(value, 1))
	mean_diff_num_table_M <- cbind(
								mean_diff_num_table_M %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_num_table_M %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_table_M <- rbind(
						mean_diff_fact_table_M,
						mean_diff_num_table_M) %>%
						dplyr::left_join(.,
							stratum_table_M %>%
								dplyr::select(-t),
							by="var") %>%
						dplyr::mutate(p=round(p,3))

	mean_diff_fact_table_F <- mean_diff_fact_table_F %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(100*value, 1))
	mean_diff_fact_table_F <- cbind(
								mean_diff_fact_table_F %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_fact_table_F %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_num_table_F <- mean_diff_num_table_F %>%
							tidyr::gather(., var, value, -phone) %>%
							dplyr::mutate(value=round(value, 1))
	mean_diff_num_table_F <- cbind(
								mean_diff_num_table_F %>%
									dplyr::filter(phone==1) %>%
									dplyr::select(-phone) %>%
									dplyr::rename(phone=value),
								mean_diff_num_table_F %>%
									dplyr::filter(phone==0) %>%
									dplyr::select(value) %>%
									dplyr::rename(no_phone=value))

	mean_diff_table_F <- rbind(
						mean_diff_fact_table_F,
						mean_diff_num_table_F) %>%
						dplyr::left_join(.,
							stratum_table_F %>%
								dplyr::select(-t),
							by="var") %>%
						dplyr::mutate(p=round(p,3))

	empty <- rep(NA, 4)

	mean_diff_table_M <- rbind(
							mean_diff_table_M[1:5,],
							empty,
							mean_diff_table_M[6:12,])

	mean_diff_table_F <- rbind(
							mean_diff_table_F[1:5,],
							empty,
							mean_diff_table_F[6:12,])

	note("Table saved...\n")
	res <- love_table
	title <- "love plot"
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "_part1.html"), output_format = rmarkdown::html_document(toc=FALSE))

	res <- love_table_sex
	title <- "love plot gender strata"
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "_part2.html"), output_format = rmarkdown::html_document(toc=FALSE))

	res <- cbind(
			mean_diff_table,
			mean_diff_table_F[,-1],
			mean_diff_table_M[,-1])
	title <- "univariate desc"
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "_part3.html"), output_format = rmarkdown::html_document(toc=FALSE))

	note("Mean reduction...\n")
	note("Pooled estimate based on matching...\n")
	data_imp <- mice::mice(rbind(data2015, data2016), m=15, seed=1234, print=FALSE)
	data_imp_ind <- mice::mice(rbind(data2015, data2016_ind), m=15, seed=1234, print=FALSE)

	trim_iter <- function(imp){
		mod_mat <- stats::model.matrix(phone~age+sex+dest+child_house+adult_house+travel+clinic_time+year+hsv, data=imp)
		imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
		colnames(imp) <- gsub("-", "_", colnames(imp))
		colnames(imp) <- gsub(">=", "_", colnames(imp))
		colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
		colnames(imp) <- gsub("\\.", "_", colnames(imp))
		repeat{
			n <- nrow(imp)
			mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
				data=imp)
			check <- apply(mod_mat, 2, mean) # Initiating a check
			check <- !(check==1 | check==0) & names(check)!="clinic_time"
			form <- paste0("phone~", paste(names(check)[check], collapse="+"))
			mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
			imp$p <- stats::predict(mod, type="response")
			cutoff_low <- min(imp$p[imp$phone==1])
			cutoff_high <- max(imp$p[imp$phone==0])
			imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
			n_trim <- nrow(imp)
			if(n_trim==n){
				break
			}
		}
		return(imp)
	}

	data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	data_imp_ind <- lapply(1:15, function(x){mice::complete(data_imp_ind, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	for(i in 1:length(data_imp)){
		data_imp[[i]]$m <- i
	}
	for(i in 1:length(data_imp_ind)){
		data_imp_ind[[i]]$m <- i
	}

	# Doing the calculation with matching
	match_est <- data_imp %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_combined <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_combined$varest)
	var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

	match_combined <- rbind(match_combined,
							data.frame(m=99,
								est=mean(match_combined$est),
								varest=var_within+(1+(1/nrow(match_combined)))*var_between))

	note("Gender stratification...\n")
	estim_gender <- function(filter="F"){
		if(filter=="F"){
			seed <- 550
		}else{
			seed <- 222
		}
		data_imp <- rbind(data2015, data2016) %>%
						dplyr::filter(sex==filter) %>%
						mice::mice(., m=15, seed=seed, print=FALSE)

		trim_iter <- function(imp){
			mod_mat <- stats::model.matrix(phone~age+dest+child_house+adult_house+travel+clinic_time+year+hsv, data=imp)
			imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
			colnames(imp) <- gsub("-", "_", colnames(imp))
			colnames(imp) <- gsub(">=", "_", colnames(imp))
			colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
			colnames(imp) <- gsub("\\.", "_", colnames(imp))
			repeat{
				n <- nrow(imp)
				mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
					data=imp)
				check <- apply(mod_mat, 2, mean) # Initiating a check
				check <- !(check==1 | check==0) & names(check)!="clinic_time"
				form <- paste0("phone~", paste(names(check)[check], collapse="+"))
				mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
				imp$p <- stats::predict(mod, type="response")
				cutoff_low <- min(imp$p[imp$phone==1])
				cutoff_high <- max(imp$p[imp$phone==0])
				imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
				n_trim <- nrow(imp)
				if(n_trim==n){
					break
				}
			}
			return(imp)
		}

		data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
							lapply(., function(temp){
								mod <- with(data=temp, glm(phone~age+dest+child_house+adult_house+travel+clinic_time+year+hsv, family = binomial(link = "logit")))
								temp$p <- stats::predict(mod, type="response")
								return(temp)
							}) %>%
							lapply(., trim_iter)

		for(i in 1:length(data_imp)){
			data_imp[[i]]$m <- i
		}

		match_est <- data_imp %>%
						lapply(., function(temp){
							temp <- temp %>%
									dplyr::mutate(lp=log(p/(1-p)))
							m_phone <- median(temp$lp[temp$phone==1])
							# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
							temp <- temp %>%
									dplyr::mutate(mark=-abs(lp-m_phone)) %>%
									dplyr::arrange(-phone, mark) %>%
									dplyr::select(-mark)
							return(temp)
							}) %>%
						do.call("rbind", .)

		match_combined <- lapply(1:max(match_est$m), function(x){
								data.frame(m=x,
									est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
									varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
							}) %>%
							do.call("rbind", .)

		var_within <- mean(match_combined$varest)
		var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

		match_combined <- rbind(match_combined,
								data.frame(m=99,
									est=mean(match_combined$est),
									varest=var_within+(1+(1/nrow(match_combined)))*var_between))

		return(match_combined)
	}

	match_combined_F <- estim_gender()
	match_combined_M <- estim_gender("M")

	note("Year stratification...\n")
	estim_year <- function(filter=2015){
		if(filter==2015){
			seed <- 505
		}else{
			seed <- 999
		}
		data_imp <- rbind(data2015, data2016_ind) %>%
						dplyr::filter(year==filter) %>%
						mice::mice(., m=15, seed=seed, print=FALSE)

		trim_iter <- function(imp){
			mod_mat <- stats::model.matrix(phone~age+dest+child_house+adult_house+travel+clinic_time+sex+hsv, data=imp)
			imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
			colnames(imp) <- gsub("-", "_", colnames(imp))
			colnames(imp) <- gsub(">=", "_", colnames(imp))
			colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
			colnames(imp) <- gsub("\\.", "_", colnames(imp))
			repeat{
				n <- nrow(imp)
				mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
					data=imp)
				check <- apply(mod_mat, 2, mean) # Initiating a check
				check <- !(check==1 | check==0) & names(check)!="clinic_time"
				form <- paste0("phone~", paste(names(check)[check], collapse="+"))
				mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
				imp$p <- stats::predict(mod, type="response")
				cutoff_low <- min(imp$p[imp$phone==1])
				cutoff_high <- max(imp$p[imp$phone==0])
				imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
				n_trim <- nrow(imp)
				if(n_trim==n){
					break
				}
			}
			return(imp)
		}

		data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
							lapply(., function(temp){
								mod <- with(data=temp, glm(phone~age+dest+child_house+adult_house+travel+clinic_time+sex+hsv, family = binomial(link = "logit")))
								temp$p <- stats::predict(mod, type="response")
								return(temp)
							}) %>%
							lapply(., trim_iter)

		for(i in 1:length(data_imp)){
			data_imp[[i]]$m <- i
		}

		match_est <- data_imp %>%
						lapply(., function(temp){
							temp <- temp %>%
									dplyr::mutate(lp=log(p/(1-p)))
							m_phone <- median(temp$lp[temp$phone==1])
							# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
							temp <- temp %>%
									dplyr::mutate(mark=-abs(lp-m_phone)) %>%
									dplyr::arrange(-phone, mark) %>%
									dplyr::select(-mark)
							return(temp)
							}) %>%
						do.call("rbind", .)

		match_combined <- lapply(1:max(match_est$m), function(x){
								data.frame(m=x,
									est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
									varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
							}) %>%
							do.call("rbind", .)

		var_within <- mean(match_combined$varest)
		var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

		match_combined <- rbind(match_combined,
								data.frame(m=99,
									est=mean(match_combined$est),
									varest=var_within+(1+(1/nrow(match_combined)))*var_between))

		return(match_combined)
	}

	match_combined_2015 <- estim_year()
	match_combined_2016 <- estim_year(2016)

	note("Plotting...\n")
	redu <- rbind(match_combined[16,] %>% dplyr::mutate(type="Whole\ndataset", col=viridis::viridis(3)[1]),
				match_combined_M[16,] %>% dplyr::mutate(type="Men\nstratum", col=viridis::viridis(3)[2]),
				match_combined_F[16,] %>% dplyr::mutate(type="Women\nstratum", col=viridis::viridis(3)[2]),
				match_combined_2015[16,] %>% dplyr::mutate(type="Rainy season -\n2015 stratum", col=viridis::viridis(3)[3]),
				match_combined_2016[16,] %>% dplyr::mutate(type="Dry season -\n2016 stratum", col=viridis::viridis(3)[3])) %>%
					dplyr::mutate(type=factor(type,
						levels=rev(c("Whole\ndataset", "Men\nstratum", "Women\nstratum", "Rainy season -\n2015 stratum", "Dry season -\n2016 stratum")))) %>%
				ggplot2::ggplot(., ggplot2::aes(x=type, y=est, color=type)) +
					ggplot2::geom_errorbar(
						ggplot2::aes(ymin=est-1.96*sqrt(varest), ymax=est+1.96*sqrt(varest)),
						size=0.25,
						width=0.4) +
					ggplot2::geom_point(
						size=1,
						shape=21,
						fill="white",
						stroke=0.7) +
					ggplot2::geom_hline(yintercept = 0, linetype="dashed") +
					ggplot2::scale_color_manual(
						name="",
						breaks=c("Whole\ndataset", "Men\nstratum", "Women\nstratum",
							"Rainy season -\n2015 stratum", "Dry season -\n2016 stratum"),
						# values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2],
						values=c("black", "red", "blue", "gray65", "gray50")) +
					ggplot2::guides(colour="none") +
					ggplot2::coord_flip() +
					ggplot2::xlab("") +
					ggplot2::ylab("Mean reduction in travel time (hours) to a health care\ncenter for mobile phone owners compared to non-owners") +
					ggplot2::theme_bw() +
					ggplot2::theme(
						axis.text.y = ggplot2::element_text(size = 4),
						axis.text.x = ggplot2::element_text(size = 3),
						legend.text=ggplot2::element_text(size=3),
						axis.title.x = ggplot2::element_text(size = 6)) 					
					# ggplot2::theme(
					# 	axis.text.y = ggplot2::element_text(size = 16),
					# 	axis.text.x = ggplot2::element_text(size = 14),
					# 	axis.title.x = ggplot2::element_text(size = 19)) 					

	# fig1 <- multipanelfigure::multi_panel_figure(
	# 			# rows=2, columns=2,
	# 			width=c(180, 180),
	# 			height=c(140, 260, 140),
	# 			font.label = list(size = 20)) %>%
	# 			multipanelfigure::fill_panel(
	# 				file.path(path.workspace, "schematic.jpg"),
	# 				scaling="fit",
	# 				label="A",
	# 				row=1, column=1) %>%
	# 			multipanelfigure::fill_panel(
	# 				file.path(path.workspace, "map data collection area.jpg"),
	# 				scaling="fit",
	# 				label="B",
	# 				row=1, column=2) %>%
	# 			multipanelfigure::fill_panel(
	# 				love_fig,
	# 				label="C",
	# 				row=2, column=1:2) %>%
	# 			multipanelfigure::fill_panel(
	# 				redu, label="D",
	# 				row=3, column=1:2) %>%
	# 			multipanelfigure::save_multi_panel_figure(paste0(file, ".tiff"), dpi=300)

	fig1B <- multipanelfigure::multi_panel_figure(
				rows=1,
				width=c(12, 10),
				height=12,
				unit="cm") %>%
				multipanelfigure::fill_panel(
					file.path(path.workspace, "schematic.jpg"),
					scaling="fit",
					label="A",
					row=1, column=1) %>%
				multipanelfigure::fill_panel(
					file.path(path.workspace, "map data collection area.jpg"),
					scaling="fit",
					label="B",
					row=1, column=2) %>%
				multipanelfigure::save_multi_panel_figure(paste0(file, "_other.eps"), dpi=300)

	fig1 <- multipanelfigure::multi_panel_figure(
				width=10,
				height=c(6, 4),
				columns=1,
				unit="cm") %>%
				multipanelfigure::fill_panel(
					love_fig,
					label="A",
					row=1, column=1) %>%
				multipanelfigure::fill_panel(
					redu, label="B",
					row=2, column=1) %>%
				multipanelfigure::save_multi_panel_figure(paste0(file, "_suppl.eps"), dpi=300)

	fig1 <- multipanelfigure::multi_panel_figure(
				width=10,
				height=6,
				columns=1,
				rows=1,
				unit="cm") %>%
				multipanelfigure::fill_panel(
					love_fig,
					label="",
					row=1, column=1) %>%
				multipanelfigure::save_multi_panel_figure(paste0(file, "_suppl.tiff"), dpi=300)

	fig1 <- multipanelfigure::multi_panel_figure(
				width=10,
				height=c(6, 4),
				columns=1,
				unit="cm") %>%
				multipanelfigure::fill_panel(
					love_fig_light,
					label="A",
					row=1, column=1) %>%
				multipanelfigure::fill_panel(
					redu, label="B",
					row=2, column=1) %>%
				multipanelfigure::save_multi_panel_figure(paste0(file, ".eps"), dpi=300)
	note("Figure saved...\n")

#width: 4.98 in
#height: 7.47 in
	redu_table <- rbind(
					match_combined[16,] %>%
						dplyr::mutate(
							type="Whole\ndataset"),
					match_combined_M[16,] %>%
						dplyr::mutate(
							type="Men\nstratum"),
					match_combined_F[16,] %>%
						dplyr::mutate(
							type="Women\nstratum"),
					match_combined_2015[16,] %>%
						dplyr::mutate(
							type="Rainy season -\n2015 stratum"),
					match_combined_2016[16,] %>%
						dplyr::mutate(
							type="Dry season -\n2016 stratum")) %>%
					dplyr::mutate(
						type=factor(type,
							levels=rev(c("Whole\ndataset", "Men\nstratum", "Women\nstratum", "Rainy season -\n2015 stratum", "Dry season -\n2016 stratum"))),
						ci=paste(round(est-1.96*sqrt(varest), 2), round(est+1.96*sqrt(varest), 2), sep="-")) %>%
					dplyr::mutate(
						est=round(est, 2)) %>%
					dplyr::select(type, est, ci)

	note("Table saved...\n")
	res <- redu_table
	title <- "mean reduction"
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "_part4.html"), output_format = rmarkdown::html_document(toc=FALSE))

}

#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

fig2_man <- function(boot=1000, file=file.path(path.workspace, "fig2_manuscript")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- as.integer(unique(c(links2015$x, links2015$y)))
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data <- rbind(data2015, data2016)
	data_ind <- rbind(data2015, data2016_ind)

	BS_imput <- function(data=data){
		 temp <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
		 temp_i <- mice::mice(temp, m=1, print=FALSE)
		 return(cbind(mice::complete(temp_i, 1), id=temp$id))
	}

	set.seed(150)
	boot_imput <- replicate(boot, BS_imput(data), simplify=FALSE)
	# boot_ind_2015 <- replicate(boot, BS_imput(data2015), simplify=FALSE)
	# boot_ind_2016 <- replicate(boot, BS_imput(data2016_ind), simplify=FALSE)
	set.seed(150)
	boot_ind <- replicate(boot, BS_imput(data_ind), simplify=FALSE)

	estim <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		data_total_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		ratio <- cbind(
					(data_phone_2[data_phone_2$phone==1, -1]/data_total_2[1,]),
					# (data_phone_2[data_phone_2$phone==1, 4]-data_total_2[1,4]),
					# (data_phone_2[data_phone_2$phone==1, -c(1, 4)]/data_total_2[1,-4]),
					# (data_phone_2[data_phone_2$phone==1, 4]-data_total_2[1,4]),
					(data_phone_1[data_phone_1$phone==1, -1]/data_total_1[1,]))
		return(ratio)
	}

	estim_ratio <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		ratio <- cbind(
					(data_phone_2[data_phone_2$phone==1, -1]/data_phone_2[data_phone_2$phone==0, -1]),
					(data_phone_1[data_phone_1$phone==1, -1]/data_phone_1[data_phone_1$phone==0, -1]))
		return(ratio)
	}

	estim_abs <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest_75=quantile(dest, probs=0.75),
							dest_25=quantile(dest, probs=0.25),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						dplyr::ungroup() %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest_75=quantile(dest, probs=0.75),
							dest_25=quantile(dest, probs=0.25),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						dplyr::mutate(phone=2) %>%
						dplyr::select(
							phone, clinic_time_75, clinic_time_25,
							clinic_time, clinic_unable,
							dest_75, dest_25, dest,
							d_child0, d_child1) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						dplyr::ungroup() %>%
						data.frame()

		data_total_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						data.frame()

		return(
			rbind(
				cbind(data_phone_1, data_phone_2 %>% dplyr::select(-phone)),
				cbind(data_total_1, data_total_2)
				)
			)
	}

	est <- lapply(boot_imput, estim) %>%
			do.call("rbind", .)

	est_M <- lapply(boot_imput, function(X){
				estim(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F <- lapply(boot_imput, function(X){
				estim(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est_phone <- lapply(boot_imput, estim_ratio) %>%
			do.call("rbind", .)

	est_M_phone <- lapply(boot_imput, function(X){
				estim_ratio(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F_phone <- lapply(boot_imput, function(X){
				estim_ratio(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est_abs <- lapply(boot_imput, estim_abs) %>%
			do.call("rbind", .)

	est_M_abs <- lapply(boot_imput, function(X){
				estim_abs(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F_abs <- lapply(boot_imput, function(X){
				estim_abs(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est <- est %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M <- est_M %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F <- est_F %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_phone <- est_phone %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M_phone <- est_M_phone %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F_phone <- est_F_phone %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_abs <- est_abs %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M_abs <- est_M_abs %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F_abs <- est_F_abs %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)

	data_fig <- rbind(
					data.frame(var=colnames(est),
						est=apply(est, 2, mean),
						ci_l=apply(est, 2, quantile, prob=0.025),
						ci_up=apply(est, 2, quantile, prob=0.975),
						type="complete",
						group="Whole dataset"),
					data.frame(var=colnames(est_M),
						est=apply(est_M, 2, mean),
						ci_l=apply(est_M, 2, quantile, prob=0.025),
						ci_up=apply(est_M, 2, quantile, prob=0.975),
						type="Men",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest")),
					data.frame(var=colnames(est_F),
						est=apply(est_F, 2, mean),
						ci_l=apply(est_F, 2, quantile, prob=0.025),
						ci_up=apply(est_F, 2, quantile, prob=0.975),
						type="Women",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest"))
					) %>%
					mutate(
						group=factor(group,
							levels = c("Whole dataset", "Gender strata")),
						var=factor(var,
							levels = rev(c("dest",
								"clinic_time_25","clinic_time", "clinic_time_75",
								"clinic_cost_travel", "clinic_cost_travel_75",
								"clinic_unable",
								"d_child0", "d_child1", "n", "n_adult", "n_child")),
							labels = rev(c(
								"Mean number of\ntravel destinations",
								"25%ile travel time to\na health care center","Mean travel time to\na health care center", "75%ile travel time to\na health care center",
								"Mean travel cost", "75%ile travel cost",
								"Proportion of individuals unable\nto reach a health care center",
								"Proportion of individuals with\nat least 1 deceased child",
								"Proportion of individuals with\nmore than 1 deceased child",
								"Mean number of people\nliving in the household",
								"Mean number of adults\nliving in the household",
								"Mean number of children\nliving in the household")))
						# type=factor(type,
						# 	levels=rev(c("complete", "Men", "Women")))
						)

	data_fig_light <- data_fig %>%
						dplyr::filter(var %in% c(
						"Mean number of\ntravel destinations",
						"Mean travel time to\na health care center", "75%ile travel time to\na health care center",
						"25%ile travel time to\na health care center"
							))

	data_phone_fig <- rbind(
					data.frame(var=colnames(est_phone),
						est=apply(est_phone, 2, mean),
						ci_l=apply(est_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_phone, 2, quantile, prob=0.975),
						type="complete",
						group="Whole dataset"),
					data.frame(var=colnames(est_M_phone),
						est=apply(est_M_phone, 2, mean),
						ci_l=apply(est_M_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_M_phone, 2, quantile, prob=0.975),
						type="Men",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest")),
					data.frame(var=colnames(est_F_phone),
						est=apply(est_F_phone, 2, mean),
						ci_l=apply(est_F_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_F_phone, 2, quantile, prob=0.975),
						type="Women",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest"))
					) %>%
					mutate(
						group=factor(group,
							levels = c("Whole dataset", "Gender strata")),
						var=factor(var,
							levels = rev(c("dest",
								"clinic_time_25","clinic_time", "clinic_time_75",
								"clinic_cost_travel", "clinic_cost_travel_75",
								"clinic_unable",
								"d_child0", "d_child1", "n", "n_adult", "n_child")),
							labels = rev(c(
								"Mean number of\ntravel destinations",
								"25%ile travel time to\na health care center","Mean travel time to\na health care center", "75%ile travel time to\na health care center",
								"Mean travel cost", "75%ile travel cost",
								"Proportion of individuals unable\nto reach a health care center",
								"Proportion of individuals with\nat least 1 deceased child",
								"Proportion of individuals with\nmore than 1 deceased child",
								"Mean number of people\nliving in the household",
								"Mean number of adults\nliving in the household",
								"Mean number of children\nliving in the household"))))

	data_phone_fig_light <- data_phone_fig %>%
								dplyr::filter(var %in% c(
								"Mean number of\ntravel destinations",
								"Mean travel time to\na health care center", "75%ile travel time to\na health care center",
								"25%ile travel time to\na health care center"
									))

	var_digest <- function(df, ref, comp, direction=-1){
		data_fig <- df %>%
						dplyr::mutate(sim=rep(1:boot, each=3)) %>%
						dplyr::filter(phone %in% c(ref, comp)) %>%
						dplyr::group_by(sim)

		if(direction==-1){
			data_fig <- data_fig %>%
							dplyr::arrange(sim, -phone)
		}else{
			data_fig <- data_fig %>%
							dplyr::arrange(sim, phone)
		}
	
		data_fig <- data_fig %>%
						dplyr::summarise(
							clinic_cost_travel=diff(clinic_cost_travel),
							clinic_cost_travel_75=diff(clinic_cost_travel_75),
							clinic_unable=diff(clinic_unable),
							clinic_time=diff(clinic_time),
							clinic_time_25=diff(clinic_time_25),
							clinic_time_75=diff(clinic_time_75), 
							d_child0=diff(d_child0),
							# d_child1=diff(d_child1),
							dest=diff(dest),
							n=diff(n),
							n_adult=diff(n_adult),
							n_child=diff(n_child)) %>%
						dplyr::ungroup() %>%
						dplyr::select(-sim)

		data_fig <- data.frame(
						var=colnames(data_fig),
						est=apply(data_fig, 2, mean),
						ci_l=apply(data_fig, 2, quantile, prob=0.025),
						ci_up=apply(data_fig, 2, quantile, prob=0.975))

		return(data_fig)
	}

	data_abs_fig <- rbind(
			var_digest(est_abs, 2, 1, -1) %>%
				dplyr::mutate(
					type="complete",
					group="Whole dataset",
					ref="All participants"),
			var_digest(est_abs, 0, 1, 1) %>%
				dplyr::mutate(
					type="complete",
					group="Whole dataset",
					ref="Non-phone owners")) %>%
			dplyr::bind_rows(.,
				rbind(
					var_digest(est_M_abs, 2, 1, -1) %>%
						dplyr::mutate(
							type="Men",
							group="Gender strata",
							ref="All participants"),
					var_digest(est_M_abs, 0, 1, 1) %>%
						dplyr::mutate(
							type="Men",
							group="Gender strata",
							ref="Non-phone owners")) %>%
				dplyr::filter(
					var %in% c("clinic_cost_travel", "clinic_cost_travel_75", "clinic_unable",
					"clinic_time", "clinic_time_25", "clinic_time_75", "dest"))) %>%
			dplyr::bind_rows(.,
				rbind(
					var_digest(est_F_abs, 2, 1, -1) %>%
						dplyr::mutate(
							type="Women",
							group="Gender strata",
							ref="All participants"),
					var_digest(est_F_abs, 0, 1, 1) %>%
						dplyr::mutate(
							type="Women",
							group="Gender strata",
							ref="Non-phone owners")) %>%
				dplyr::filter(var %in% c("clinic_cost_travel", "clinic_cost_travel_75", "clinic_unable",
					"clinic_time", "clinic_time_25", "clinic_time_75", "dest"))) %>%
			dplyr::mutate(
				group_var=ifelse(var %in% c("clinic_time", "clinic_time_25", "clinic_time_75"),
					"Time to reach a\nhealth care center",
					ifelse(var %in% c("clinic_cost_travel", "clinic_cost_travel_75"),
						"Travel cost",
						ifelse(var %in% c("clinic_unable"),
							"Being unable to access a\nhealth care center",
							ifelse(var %in% c("d_child0"),
								"At least 1 deceased child",
								ifelse(var %in% c("dest", "dest_75", "dest_25"),
									"Mean number of travel\ndestinations",
									"Household characteristics")))))) %>%
			dplyr::filter(var %in% c(
				"clinic_time", "clinic_time_25", "clinic_time_75",
				"dest", "dest_25", "dest_75")) %>%
			dplyr::mutate(
				group_var=ifelse(var %in% c("clinic_time", "clinic_time_25", "clinic_time_75"), "Travel time to\na health care center",
						"Mean number of travel\ndestinations"),
				# var=factor(var,
				# 	levels = rev(c(
				# 		"clinic_time_25", "clinic_time", "clinic_time_75", 
				# 		"dest_25","dest", "dest_75")),
				# 	# labels = rev(
				# 	# 	c("Mean travel cost", "75p travel cost",
				# 	# 	"Mean travel time to\na health care centre", "25p travel time to\na health care centre", "75p travel time to\na health care centre",
				# 	# 	"Mean number of\ntravel destinations", "25p number of\ntravel destinations", "75p number of\ntravel destinations"))))
				# 	labels = rev(
				# 		c("Mean", "75%ile",
				# 		"25%ile", "Mean", "75%ile",
				# 		"25%ile","", "75%ile"))),
				var=factor(var,
					levels = rev(c("dest",
						"clinic_time_25","clinic_time", "clinic_time_75")),
					labels = rev(c(
						"Mean number of\ntravel destinations",
						"25%ile travel time to\na health care center","Mean travel time to\na health care center", "75%ile travel time to\na health care center"))),
				type=factor(
					type,
					levels=rev(c("complete", "Men", "Women")),
					labels=rev(c("Whole dataset", "Men", "Women"))))
			# dplyr::mutate(
			# 	group_var=factor(
			# 		group_var,
			# 		levels=c("Mean number of travel\ndestinations", "Travel time to\na health care center", "Travel cost")))

	note("Plotting...\n")
	# figB1 <- ggplot2::ggplot(
	# 			data_abs_fig %>%
	# 				dplyr::filter(
	# 					ref=="All participants") %>%
	# 				dplyr::mutate(ref=paste0("Reference: ", ref)),
	# 			ggplot2::aes(y=est, x=var, color=type, fill=type)) +
	# 			ggplot2::geom_bar(
	# 				stat="identity",
	# 				width=0.5,
	# 				alpha=0.4,
	# 				position=ggplot2::position_dodge(width=0.5)) +
	# 			ggplot2::geom_errorbar(
	# 				ggplot2::aes(
	# 					ymin=ci_l,
	# 					ymax=ci_up,
	# 					x=var,
	# 					color=type),
	# 				position=ggplot2::position_dodge(width=0.5),
	# 				width=0.25) +
	# 			ggplot2::coord_flip() +
	# 			ggplot2::facet_wrap(.~group_var, scales="free", ncol=1) +
	# 			ggplot2::geom_hline(yintercept = 0, linetype="dashed") +
	# 			ggplot2::scale_color_manual(
	# 				name="",
	# 				breaks=c("complete", "Men", "Women"),
	# 				labels=c("All participants", "Men", "Women"),
	# 				values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
	# 			ggplot2::scale_fill_manual(
	# 				name="",
	# 				breaks=c("complete", "Men", "Women"),
	# 				labels=c("All participants", "Men", "Women"),
	# 				values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
	# 			ggplot2::ylab("Absolute difference between mobile phone owners and the reference group") +
	# 			ggplot2::xlab("") +
	# 			ggplot2::theme_bw() +
	# 			ggplot2::theme(legend.position="none")

	# figB2 <- ggplot2::ggplot(
	# 			data_abs_fig %>%
	# 				dplyr::filter(
	# 					ref=="Non owners") %>%
	# 				dplyr::mutate(ref=paste0("Reference: ", ref)),
	# 			ggplot2::aes(y=est, x=var, color=type, fill=type)) +
	# 			ggplot2::geom_bar(
	# 				stat="identity",
	# 				width=0.5,
	# 				alpha=0.4,
	# 				position=ggplot2::position_dodge(width=0.5)) +
	# 			ggplot2::geom_errorbar(
	# 				ggplot2::aes(
	# 					ymin=ci_l,
	# 					ymax=ci_up,
	# 					x=var,
	# 					color=type),
	# 				position=ggplot2::position_dodge(width=0.5),
	# 				width=0.25) +
	# 			ggplot2::coord_flip() +
	# 			ggplot2::facet_wrap(.~group_var, scales="free", ncol=1) +
	# 			ggplot2::geom_hline(yintercept = 0, linetype="dashed") +
	# 			ggplot2::scale_color_manual(
	# 				name="",
	# 				breaks=c("complete", "Men", "Women"),
	# 				labels=c("All participants", "Men", "Women"),
	# 				values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
	# 			ggplot2::scale_fill_manual(
	# 				name="",
	# 				breaks=c("complete", "Men", "Women"),
	# 				labels=c("All participants", "Men", "Women"),
	# 				values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
	# 			ggplot2::ylab("Absolute difference between mobile phone owners and the reference group") +
	# 			ggplot2::xlab("") +
	# 			ggplot2::theme_bw() +
	# 			ggplot2::theme(
	# 				legend.position="none",
	# 				axis.title.y=ggplot2::element_blank(),
	# 				axis.text.y=ggplot2::element_blank(),
	# 				axis.ticks.y=ggplot2::element_blank())

	figB <- ggplot2::ggplot(
				data_abs_fig %>%
					dplyr::mutate(ref=paste0("Reference: ", ref)),
				ggplot2::aes(x=est, y=var, color=type, fill=type)) +
				ggplot2::geom_bar(
					stat="identity",
					width=0.5,
					alpha=1,
					position=ggplot2::position_dodge(width=0.5)) +
				ggplot2::geom_errorbar(
					ggplot2::aes(
						xmin=ci_l,
						xmax=ci_up,
						y=var,
						color=type),
					position=ggplot2::position_dodge(width=0.5),
					width=0.25) +
				# ggplot2::coord_flip() +
				# ggplot2::facet_wrap(
				# 	ref~.,
				# 	scales="free",
				# 	ncol=2,
				# 	dir="v") +
				# ggh4x::facet_grid2(
				# 	ref~group_var,
				# 	switch="y",
				# 	scales="free",
				# 	independent="y") +
				ggplot2::facet_wrap(.~ref, ncol=2, scale="free_x") +
				# ggplot2::scale_y_discrete(
				# 	breaks=c("25%ile", "Mean", "75%ile")) +
				ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
				ggplot2::scale_color_manual(
					name="",
					breaks=rev(c("Whole dataset", "Men", "Women")),
					labels=rev(c("Whole dataset", "Men", "Women")),
					# values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
					values=rev(c("black", "red", "blue"))) +
				ggplot2::scale_fill_manual(
					name="",
					breaks=rev(c("Whole dataset", "Men", "Women")),
					labels=rev(c("Whole dataset", "Men", "Women")),
					# values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
					values=rev(c("black", "red", "blue"))) +
				ggplot2::xlab("Absolute difference between mobile phone owners\nand the reference group") +
				ggplot2::ylab("") +
				ggplot2::theme_bw() +
				ggplot2::theme(
					legend.position="none",
					# axis.text.y = ggplot2::element_text(size = 8),
					axis.text.y = ggplot2::element_blank(),
					axis.ticks.y = ggplot2::element_blank(),
					axis.text.x = ggplot2::element_text(size = 8),
					axis.title.x = ggplot2::element_text(size = 10))

# gtstrip <- ggplot2::ggplotGrob(figB)
# stripText_g <- gtable::gtable_filter(gtstrip, "strip-t")

# gt1 <- ggplot2::ggplotGrob(figB1)
# panels <- c(subset(gt1$layout, grepl("panel", gt1$layout$name), se=t:r))
# strips <- c(subset(gt1$layout, grepl("strip-t", gt1$layout$name), se=t:r))
# stripText <- gtable::gtable_filter(gt1, "strip-t")
# striph <- gt1$heights[7]
# gt1 <- gtable::gtable_add_rows(gt1, unit(5.5, "points"), 6)
# gt1 <- gtable::gtable_add_grob(gt1, stripText_g$grobs[[1]]$grobs[[1]], 6, l=5)
# gt1$heights[6] = unit(as.numeric(gsub("cm", "", striph)), "cm")
# gt1$heights[7] = unit(0, "cm")

# gt2 <- ggplot2::ggplotGrob(figB2)
# panels <- c(subset(gt2$layout, grepl("panel", gt2$layout$name), se=t:r))
# strips <- c(subset(gt2$layout, grepl("strip-t", gt2$layout$name), se=t:r))
# stripText <- gtable::gtable_filter(gt2, "strip-t")
# striph <- gt2$heights[7]
# gt2 <- gtable::gtable_add_rows(gt2, unit(5.5, "points"), 6)
# gt2 <- gtable::gtable_add_grob(gt2, stripText_g$grobs[[2]]$grobs[[1]], 6, l=5)
# gt2$heights[6] = unit(as.numeric(gsub("cm", "", striph)), "cm")
# gt2$heights[7] = unit(0, "cm")

# grid::grid.draw(gt2)

	figA <- ggplot2::ggplot(
				data_fig %>%
					dplyr::mutate(ref="Reference: All participants") %>%
					dplyr::bind_rows(.,
						data_phone_fig %>%
							dplyr::mutate(ref="Reference: Non-phone owners")) %>%
					dplyr::mutate(
						shape=ifelse(round(ci_l,2)<1 & round(ci_up,2)>1, 21, 16),
						type=factor(
							type,
							levels=rev(c("complete", "Men", "Women")),
							labels=rev(c("Whole dataset", "Men", "Women")))),
				ggplot2::aes(x=est, y=var, color=type)) +
				ggplot2::geom_errorbarh(
					ggplot2::aes(xmin=ci_l,
						xmax=ci_up, y=var, color=type),
					position=ggplot2::position_dodge(width=0.5),
					height=0.25) +
				ggplot2::geom_point(
					ggplot2::aes(color=type, fill=I(ifelse(shape==21, NA, ifelse(type=="Whole dataset", "black", ifelse(type=="Men", "red", "blue"))))),
					shape=21,
					stroke=2,
					size=2.2,
					position=ggplot2::position_dodge(width=0.5)) +
				ggplot2::geom_vline(xintercept = 1, linetype="dashed") +
				ggplot2::scale_color_manual(
					name="",
					breaks=rev(c("Whole dataset", "Men", "Women")),
					labels=rev(c("All participants", "Men", "Women")),
					# values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
					values=rev(c("black", "red", "blue"))) +
				ggplot2::scale_x_continuous(
					trans="log2") +
				ggplot2::facet_wrap(.~ref, ncol=2, scale="free_x") +
				# ggplot2::xlim(c(0, 6.6)) +
				# ggplot2::guides(colour="none") +
				ggplot2::xlab("Ratio of the value for mobile phone owners over the value of the reference group") +
				ggplot2::ylab("") +
				ggplot2::theme_bw() +
				ggplot2::theme(
					legend.position="none",
					axis.text.y = ggplot2::element_text(size = 8),
					axis.text.x = ggplot2::element_text(size = 8),
					axis.title.x = ggplot2::element_text(size = 10))

	figA_light <- ggplot2::ggplot(
				data_fig_light %>%
					dplyr::mutate(ref="Reference: All participants") %>%
					dplyr::bind_rows(.,
						data_phone_fig_light %>%
							dplyr::mutate(ref="Reference: Non-phone owners")) %>%
					dplyr::mutate(
						shape=ifelse(round(ci_l,2)<1 & round(ci_up,2)>1, 21, 16),
						type=factor(
							type,
							levels=rev(c("complete", "Men", "Women")),
							labels=rev(c("Whole dataset", "Men", "Women")))),
				ggplot2::aes(x=est, y=var, color=type)) +
				ggplot2::geom_errorbarh(
					ggplot2::aes(xmin=ci_l,
						xmax=ci_up, y=var, color=type),
					position=ggplot2::position_dodge(width=0.5),
					height=0.25) +
				ggplot2::geom_point(
					ggplot2::aes(color=type, fill=I(ifelse(shape==21, NA, ifelse(type=="Whole dataset", "black", ifelse(type=="Men", "red", "blue"))))),
					shape=21,
					stroke=2,
					size=2.2,
					position=ggplot2::position_dodge(width=0.5)) +
				ggplot2::geom_vline(xintercept = 1, linetype="dashed") +
				ggplot2::scale_color_manual(
					name="",
					breaks=rev(c("Whole dataset", "Men", "Women")),
					labels=rev(c("All participants", "Men", "Women")),
					# values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
					values=rev(c("black", "red", "blue"))) +
				ggplot2::scale_x_continuous(
					trans="log2") +
				ggplot2::facet_wrap(.~ref, ncol=2, scale="free_x") +
				# ggplot2::xlim(c(0, 6.6)) +
				# ggplot2::guides(colour="none") +
				ggplot2::xlab("Ratio of the value for mobile phone owners over\nthe value of the reference group") +
				ggplot2::ylab("") +
				ggplot2::theme_bw() +
				ggplot2::theme(
					legend.position="none",
					# axis.text.y = ggplot2::element_text(size = 8),
					axis.text.y = ggplot2::element_blank(),
					axis.text.x = ggplot2::element_text(size = 8),
					axis.title.x = ggplot2::element_text(size = 10))

#72.27 grid units/in
	# fig2 <- multipanelfigure::multi_panel_figure(
	# 			width=c(4, 6),
	# 			height=11,
	# 			unit="cm",
	# 			rows=1) %>%
	# 			multipanelfigure::fill_panel(
	# 				figA,
	# 				label="A",
	# 				row=1, column=1) %>%
	# 			multipanelfigure::fill_panel(
	# 				figB, label="B",
	# 				row=1, column=2) %>%
	# 			multipanelfigure::save_multi_panel_figure(paste0(file, ".eps"), dpi=300)

	# fig2 <- multipanelfigure::multi_panel_figure(
	# 			width=c(4, 6),
	# 			height=11,
	# 			unit="cm",
	# 			rows=1) %>%
	# 			multipanelfigure::fill_panel(
	# 				figA,
	# 				label="A",
	# 				row=1, column=1) %>%
	# 			multipanelfigure::fill_panel(
	# 				figB, label="B",
	# 				row=1, column=2)

	ggsave(
		paste0(file, "A.eps"),
		figA_light,
		width=20,
		height=22,
		unit="cm")

	ggsave(
		paste0(file, "B.eps"),
		figB,
		width=24,
		height=22,
		unit="cm")

	fig2 <- multipanelfigure::multi_panel_figure(
				rows=1,
				width=c(10, 10),
				height=20,
				unit="cm") %>%
				multipanelfigure::fill_panel(
					figA_light,
					label="A",
					row=1, column=1) %>%
				multipanelfigure::fill_panel(
					figB,
					label="B",
					row=1, column=2) %>%
				multipanelfigure::save_multi_panel_figure(paste0(file, ".eps"), dpi=300)

	# ggsave(
	# 	paste0(file, ".eps"),
	# 	grid.arrange(
	# 		figA,
	# 		figB,
	# 		ncol=2,
	# 		widths=c(4,6)),
	# 	width=20,
	# 	height=22,
	# 	unit="cm",
	# 	device = cairo_eps
	# 	)

	# cairo_ps(
	# 	paste0(file, ".eps"),
	# 	width=18,
	# 	height=9,
	# 	fallback_resolution = 300)
	# grid.arrange(
	# 	figA,
	# 	figB,
	# 	ncol=2,
	# 	widths=c(4,6))
	# dev.off()

	# Cairo::Cairo(
	# 	paste0(file, ".eps"),
	# 	type="ps",
	# 	width=20,
	# 	height=22,
	# 	units="cm")
	# fig2
	# dev.off()

	note("Figure saved...\n")
# width: 5.67 in
# height: 3.32 in
	note("Table saved...\n")
	abs_table <- cbind(
					data_abs_fig %>%
						dplyr::mutate(group=factor(group, levels=c("complete", "Women", "Men"))) %>%
						dplyr::arrange(group_var, var, group) %>%
						dplyr::filter(ref=="All participants") %>%
						dplyr::mutate(
							est=round(est, 3),
							ci=paste(round(ci_l, 3), round(ci_up, 3), sep="-")) %>% 
						dplyr::select(var, type, est, ci),
					data_abs_fig %>%
						dplyr::mutate(group=factor(group, levels=c("complete", "Women", "Men"))) %>%
						dplyr::arrange(group_var, var, group) %>%
						dplyr::filter(ref=="Non-phone owners") %>%
						dplyr::mutate(
							est=round(est, 3),
							ci=paste(round(ci_l, 3), round(ci_up, 3), sep="-")) %>% 
						dplyr::select(est, ci))

	ratio_table <- cbind(
					data_fig %>%
						dplyr::mutate(group=factor(group, levels=c("complete", "Women", "Men"))) %>%
						dplyr::arrange(var, type) %>%
						dplyr::mutate(
							est=round(est, 3),
							ci=paste(round(ci_l, 3), round(ci_up, 3), sep="-")) %>% 
						dplyr::select(var, type, est, ci),
					data_phone_fig %>%
						dplyr::mutate(group=factor(group, levels=c("complete", "Women", "Men"))) %>%
						dplyr::arrange(var, type) %>%
						dplyr::mutate(
							est=round(est, 3),
							ci=paste(round(ci_l, 3), round(ci_up, 3), sep="-")) %>% 
						dplyr::select(est, ci))

	res <- ratio_table
	title <- "bias ratio"
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "_ratio.html"), output_format = rmarkdown::html_document(toc=FALSE))

	res <- abs_table
	title <- "bias abs"
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "_abs.html"), output_format = rmarkdown::html_document(toc=FALSE))

}


#' Creating a figure on the network
#'
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

suppl_net_degree <- function(file=file.path(path.workspace, "net_degree_supplementary.tiff")){
	# 2015
	net <- net2015
	data <- net_d_manage(net, verbose=FALSE)
	nodes <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration

	edges <- edge2015
	nodes <- nodes %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links$min <- apply(links[,1:2],1,min)
	links$max <- apply(links[,1:2],1,max)
	links <- links[links$min!=links$max,]
	links$comb <- paste(links$min, links$max,sep="-")
	links <- links[!duplicated(links$comb),c("x","y")]
	l_list <- unique(c(links$x, links$y))
	temp <- net[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=as.integer(l_list[!(l_list %in% as.character(nodes$id))])) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes <- nodes %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes[match(l_list$id, nodes$id), c("age", "sex")] <- l_list[na.omit(match(nodes$id, l_list$id)), c("age", "sex")]
	nodes <- nodes %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
					phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l <- links
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
						"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
						(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")

	edges <- setNames(edges, c("x", "y"))
	edges$link <- unlist(lapply(1:nrow(edges), function(x){
		paste(sort(t(edges)[,x]), collapse="-")
	}))
	links$link <- unlist(lapply(1:nrow(links), function(x){
		paste(sort(t(links)[,x]), collapse="-")
	}))
	links_df <- rbind(edges, links) %>%
					dplyr::group_by(link) %>%
					dplyr::mutate(n=dplyr::row_number()) %>%
					dplyr::ungroup() %>%
					dplyr::filter(n==1) %>%
					dplyr::select(x, y, link)
	links_df$x <- ifelse(substr(links_df$x, 1, 1)=="0", substr(links_df$x, 2, nchar(links_df$x)), links_df$x)
	links_df$y <- ifelse(substr(links_df$y, 1, 1)=="0", substr(links_df$y, 2, nchar(links_df$y)), links_df$y)

	added_id <- unique(c(edges$x, edges$y, as.character(nodes$id)))

	net <- igraph::graph_from_data_frame(
				d=unique(links_df[,c("x", "y")]),
				vertices=data.frame(id=added_id),
				directed=FALSE)

	d_dist_2015 <- data.frame(
				id=added_id,
				d=igraph::degree(net)) %>%
				dplyr::left_join(
					.,
					nodes %>%
						dplyr::mutate(
							id=as.character(id)) %>%
						dplyr::select(id, phone, sex),
					by="id")

	# 2016
	net <- net2016
	data <- net_d_manage(net, verbose=FALSE)
	nodes <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	edges <- edge2016
	nodes <- nodes[!(nodes$id==216 & nodes$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links$min <- apply(links[,1:2],1,min)
	links$max <- apply(links[,1:2],1,max)
	links <- links[links$min!=links$max,]
	links$comb <- paste(links$min, links$max,sep="-")
	links <- links[!duplicated(links$comb),c("x","y")]
	links_l <- links
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
						"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
						(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")
	edges <- setNames(edges, c("x", "y"))
	edges$link <- unlist(lapply(1:nrow(edges), function(x){
		paste(sort(t(edges)[,x]), collapse="-")
	}))
	links$link <- unlist(lapply(1:nrow(links), function(x){
		paste(sort(t(links)[,x]), collapse="-")
	}))
	links_df <- rbind(edges, links) %>%
					dplyr::group_by(link) %>%
					dplyr::mutate(n=dplyr::row_number()) %>%
					dplyr::ungroup() %>%
					dplyr::filter(n==1) %>%
					dplyr::select(x, y, link)
	links_df$x <- ifelse(substr(links_df$x, 1, 1)=="0", substr(links_df$x, 2, nchar(links_df$x)), links_df$x)
	links_df$y <- ifelse(substr(links_df$y, 1, 1)=="0", substr(links_df$y, 2, nchar(links_df$y)), links_df$y)

	added_id <- unique(c(edges$x, edges$y, as.character(nodes$id)))

	net <- igraph::graph_from_data_frame(
				d=unique(links_df[,c("x", "y")]),
				vertices=data.frame(id=added_id),
				directed=FALSE)

	d_dist_2016 <- data.frame(
				id=added_id,
				d=igraph::degree(net)) %>%
				dplyr::left_join(
					.,
					nodes %>%
						dplyr::mutate(
							id=as.character(id)) %>%
						dplyr::select(id, phone, sex),
					by="id")

	gg_color_hue <- function(n) {
	  hues = seq(15, 375, length = n + 1)
	  hcl(h = hues, l = 65, c = 100)[1:n]
	}

	figA <- rbind(
			d_dist_2015 %>%
				dplyr::mutate(
					year="Rainy season - 2015"),
			d_dist_2016 %>%
				dplyr::mutate(
					year="Dry season - 2016")) %>%
			dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
			dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
			ggplot2::ggplot(., ggplot2::aes(x=d)) +
				ggplot2::geom_histogram(
					ggplot2::aes(fill=phone),
					color="black",
					binwidth=1) +
				ggplot2::scale_fill_manual(
					name="",
					values=rev(gg_color_hue(4))) +
				ggplot2::facet_grid(.~year) +
				ggplot2::xlab("Number of edges") +
				ggplot2::ylab("Count") +
				ggplot2::theme_bw()

	figB <- rbind(
			d_dist_2015 %>%
				dplyr::mutate(
					year="Rainy season - 2015"),
			d_dist_2016 %>%
				dplyr::mutate(
					year="Dry season - 2016")) %>%
			dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
			dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
			dplyr::filter(phone %in% c("No phone", "Phone")) %>%
			ggplot2::ggplot(., ggplot2::aes(x=d)) +
				ggplot2::geom_histogram(
					ggplot2::aes(fill=phone),
					color="black",
					binwidth=1) +
				ggplot2::scale_fill_manual(
					name="",
					values=rev(gg_color_hue(4)[1:2])) +
				ggplot2::facet_grid(.~year) +
				ggplot2::xlab("Number of edges") +
				ggplot2::ylab("Count") +
				ggplot2::theme_bw()

	fig <- multipanelfigure::multi_panel_figure(
				width=220,
				height=c(120, 120),
				columns=1) %>%
				multipanelfigure::fill_panel(
					figA,
					label="A",
					row=1, column=1) %>%
				multipanelfigure::fill_panel(
					figB, label="B",
					row=2, column=1) %>%
				multipanelfigure::save_multi_panel_figure(paste0(file, ".tiff"), dpi=300)
	note("Figure saved...\n")

	data_table <- rbind(
					d_dist_2015 %>%
						dplyr::mutate(
							year="Rainy season - 2015"),
					d_dist_2016 %>%
						dplyr::mutate(
							year="Dry season - 2016")) %>%
					dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
					dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
					dplyr::filter(phone %in% c("No phone", "Phone")) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(
						d_mean=round(mean(d), 2),
						d_med=round(median(d), 2),
						IQR=paste0(round(quantile(d, 0.25), 2), "-", round(quantile(d, 0.75), 2))) %>%
					dplyr::ungroup() %>%
					dplyr::mutate(
						p=c(wilcox.test(
							d~phone,
							data=rbind(
								d_dist_2015 %>%
									dplyr::mutate(
										year="Rainy season - 2015"),
								d_dist_2016 %>%
									dplyr::mutate(
										year="Dry season - 2016")) %>%
								dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
								dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
								dplyr::filter(phone %in% c("No phone", "Phone")))$p.value %>% round(., 3), "")) %>%
					dplyr::mutate(data="Complete") %>%
					dplyr::bind_rows(.,
						rbind(
							d_dist_2015 %>%
								dplyr::mutate(
									year="Rainy season - 2015"),
							d_dist_2016 %>%
								dplyr::mutate(
									year="Dry season - 2016")) %>%
							dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
							dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
							dplyr::filter(phone %in% c("No phone", "Phone") & sex=="F") %>%
							dplyr::group_by(phone) %>%
							dplyr::summarise(
								d_mean=round(mean(d), 2),
								d_med=round(median(d), 2),
								IQR=paste0(round(quantile(d, 0.25), 2), "-", round(quantile(d, 0.75), 2))) %>%
							dplyr::ungroup() %>%
							dplyr::mutate(
								p=c(wilcox.test(
									d~phone,
									data=rbind(
										d_dist_2015 %>%
											dplyr::mutate(
												year="Rainy season - 2015"),
										d_dist_2016 %>%
											dplyr::mutate(
												year="Dry season - 2016")) %>%
										dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
										dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
										dplyr::filter(phone %in% c("No phone", "Phone") & sex=="F"))$p.value %>% round(., 3), "")) %>%
							dplyr::mutate(data="Women")) %>%
					dplyr::bind_rows(.,
						rbind(
							d_dist_2015 %>%
								dplyr::mutate(
									year="Rainy season - 2015"),
							d_dist_2016 %>%
								dplyr::mutate(
									year="Dry season - 2016")) %>%
							dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
							dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
							dplyr::filter(phone %in% c("No phone", "Phone") & sex=="M") %>%
							dplyr::group_by(phone) %>%
							dplyr::summarise(
								d_mean=round(mean(d), 2),
								d_med=round(median(d), 2),
								IQR=paste0(round(quantile(d, 0.25), 2), "-", round(quantile(d, 0.75), 2))) %>%
							dplyr::ungroup() %>%
							dplyr::mutate(
								p=c(wilcox.test(
									d~phone,
									data=rbind(
										d_dist_2015 %>%
											dplyr::mutate(
												year="Rainy season - 2015"),
										d_dist_2016 %>%
											dplyr::mutate(
												year="Dry season - 2016")) %>%
										dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
										dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
										dplyr::filter(phone %in% c("No phone", "Phone") & sex=="M"))$p.value %>% round(., 3), "")) %>%
							dplyr::mutate(data="Men")) %>%
					dplyr::bind_rows(.,
						rbind(
							d_dist_2015 %>%
								dplyr::mutate(
									year="Rainy season - 2015"),
							d_dist_2016 %>%
								dplyr::mutate(
									year="Dry season - 2016")) %>%
							dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
							dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
							dplyr::filter(phone %in% c("No phone", "Phone") & year=="Rainy season - 2015") %>%
							dplyr::group_by(phone) %>%
							dplyr::summarise(
								d_mean=round(mean(d), 2),
								d_med=round(median(d), 2),
								IQR=paste0(round(quantile(d, 0.25), 2), "-", round(quantile(d, 0.75), 2))) %>%
							dplyr::ungroup() %>%
							dplyr::mutate(
								p=c(wilcox.test(
									d~phone,
									data=rbind(
										d_dist_2015 %>%
											dplyr::mutate(
												year="Rainy season - 2015"),
										d_dist_2016 %>%
											dplyr::mutate(
												year="Dry season - 2016")) %>%
										dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
										dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
										dplyr::filter(phone %in% c("No phone", "Phone") & year=="Rainy season - 2015"))$p.value %>% round(., 3), "")) %>%
							dplyr::mutate(data="2015")) %>%
					dplyr::bind_rows(.,
						rbind(
							d_dist_2015 %>%
								dplyr::mutate(
									year="Rainy season - 2015"),
							d_dist_2016 %>%
								dplyr::mutate(
									year="Dry season - 2016")) %>%
							dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
							dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
							dplyr::filter(phone %in% c("No phone", "Phone") & year=="Dry season - 2016") %>%
							dplyr::group_by(phone) %>%
							dplyr::summarise(
								d_mean=round(mean(d), 2),
								d_med=round(median(d), 2),
								IQR=paste0(round(quantile(d, 0.25), 2), "-", round(quantile(d, 0.75), 2))) %>%
							dplyr::ungroup() %>%
							dplyr::mutate(
								p=c(wilcox.test(
									d~phone,
									data=rbind(
										d_dist_2015 %>%
											dplyr::mutate(
												year="Rainy season - 2015"),
										d_dist_2016 %>%
											dplyr::mutate(
												year="Dry season - 2016")) %>%
										dplyr::mutate(phone=ifelse(is.na(as.character(phone)), "No data", as.character(phone))) %>%
										dplyr::mutate(year=factor(year, levels=c("Rainy season - 2015", "Dry season - 2016"))) %>%
										dplyr::filter(phone %in% c("No phone", "Phone") & year=="Dry season - 2016"))$p.value %>% round(., 3), "")) %>%
							dplyr::mutate(data="2016"))

	note("Table saved...\n")
	title <- "Network - Degree and phone"
	res <- data_table
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=file.path(path.workspace, "net_phone_table.html"), output_format = rmarkdown::html_document(toc=FALSE))

}

#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

suppl_bias <- function(boot=1000, file=file.path(path.workspace, "bias_supplementary_")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- as.integer(unique(c(links2015$x, links2015$y)))
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data <- rbind(data2015, data2016)
	data_ind <- rbind(data2015, data2016_ind)

	BS_imput <- function(data=data){
		 temp <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
		 temp_i <- mice::mice(temp, m=1, print=FALSE)
		 return(cbind(mice::complete(temp_i, 1), id=temp$id))
	}

	boot_imput <- replicate(boot, BS_imput(data), simplify=FALSE)
	# boot_ind_2015 <- replicate(boot, BS_imput(data2015), simplify=FALSE)
	# boot_ind_2016 <- replicate(boot, BS_imput(data2016_ind), simplify=FALSE)
	boot_ind <- replicate(boot, BS_imput(data_ind), simplify=FALSE)

	# temp_i <- list()
	# for(i in 1:boot){
	# 	 temp_i[[i]] <- mice::mice(
	# 	 			data2016_ind[sample(1:nrow(data2016_ind), nrow(data2016_ind), replace=TRUE),],
	# 	 			m=1, print=FALSE)
	# }

	estim <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		data_total_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		ratio <- cbind(
					(data_phone_2[data_phone_2$phone==1, -1]/data_total_2[1,]),
					# (data_phone_2[data_phone_2$phone==1, 4]-data_total_2[1,4]),
					# (data_phone_2[data_phone_2$phone==1, -c(1, 4)]/data_total_2[1,-4]),
					# (data_phone_2[data_phone_2$phone==1, 4]-data_total_2[1,4]),
					(data_phone_1[data_phone_1$phone==1, -1]/data_total_1[1,]))
		return(ratio)
	}

	estim_ratio <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		ratio <- cbind(
					(data_phone_2[data_phone_2$phone==1, -1]/data_phone_2[data_phone_2$phone==0, -1]),
					(data_phone_1[data_phone_1$phone==1, -1]/data_phone_1[data_phone_1$phone==0, -1]))
		return(ratio)
	}

	estim_abs <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest_75=quantile(dest, probs=0.75),
							dest_25=quantile(dest, probs=0.25),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						dplyr::ungroup() %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest_75=quantile(dest, probs=0.75),
							dest_25=quantile(dest, probs=0.25),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						dplyr::mutate(phone=2) %>%
						dplyr::select(
							phone, clinic_time_75, clinic_time_25,
							clinic_time, clinic_unable,
							dest_75, dest_25, dest,
							d_child0, d_child1) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						dplyr::ungroup() %>%
						data.frame()

		data_total_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						data.frame()

		return(
			rbind(
				cbind(data_phone_1, data_phone_2 %>% dplyr::select(-phone)),
				cbind(data_total_1, data_total_2)
				)
			)
	}

	est <- lapply(boot_imput, estim) %>%
			do.call("rbind", .)

	est_M <- lapply(boot_imput, function(X){
				estim(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F <- lapply(boot_imput, function(X){
				estim(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est_phone <- lapply(boot_imput, estim_ratio) %>%
			do.call("rbind", .)

	est_M_phone <- lapply(boot_imput, function(X){
				estim_ratio(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F_phone <- lapply(boot_imput, function(X){
				estim_ratio(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est_abs <- lapply(boot_imput, estim_abs) %>%
			do.call("rbind", .)

	est_M_abs <- lapply(boot_imput, function(X){
				estim_abs(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F_abs <- lapply(boot_imput, function(X){
				estim_abs(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est <- est %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M <- est_M %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F <- est_F %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_phone <- est_phone %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M_phone <- est_M_phone %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F_phone <- est_F_phone %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_abs <- est_abs %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M_abs <- est_M_abs %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F_abs <- est_F_abs %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)

	data_fig <- rbind(
					data.frame(var=colnames(est),
						est=apply(est, 2, mean),
						ci_l=apply(est, 2, quantile, prob=0.025),
						ci_up=apply(est, 2, quantile, prob=0.975),
						type="complete",
						group="Whole dataset"),
					data.frame(var=colnames(est_M),
						est=apply(est_M, 2, mean),
						ci_l=apply(est_M, 2, quantile, prob=0.025),
						ci_up=apply(est_M, 2, quantile, prob=0.975),
						type="Men",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest")),
					data.frame(var=colnames(est_F),
						est=apply(est_F, 2, mean),
						ci_l=apply(est_F, 2, quantile, prob=0.025),
						ci_up=apply(est_F, 2, quantile, prob=0.975),
						type="Women",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest"))
					) %>%
					mutate(
						group=factor(group,
							levels = c("Whole dataset", "Gender strata")),
						var=factor(var,
							levels = rev(c("clinic_time","clinic_cost_travel", "clinic_cost_travel_75",
								"clinic_unable", "clinic_time", "clinic_time_25", "clinic_time_75", 
								"d_child0", "d_child1", "dest", "n", "n_adult", "n_child")),
							labels = rev(c("Time to reach a\nhealth care center",
								"Mean travel cost", "75p travel cost",
								"Being unable to access a\nhealth care center",
								"Mean time to reach\na health care centre", "25p time to reach\na health care centre", "75p time to reach\na health care centre",
								"At least 1 deceased child",
								"More than 1 deceased child",
								"Number of travel\ndestinations",
								"People living\nin the household",
								"Adults living\nin the household",
								"Children living\nin the household")))) %>%
					dplyr::mutate(
						est=round(est, 3),
						ci=paste0(round(ci_l, 3), "-", round(ci_up, 3)))

	data_table <- cbind(
					data_fig[data_fig$type=="complete", c("var", "est", "ci")][c(1:3, 5, 4, 8, 7, 6, 9:11),],
					rbind(
						rep("", 2),
						rep("", 2),
						rep("", 2),
						data_fig[data_fig$type=="Women", c("est", "ci")][c(2, 1, 5:3),],
						rep("", 2),
						data_fig[data_fig$type=="Women", c("est", "ci")][6,],
						rep("", 2)),
					rbind(
						rep("", 2),
						rep("", 2),
						rep("", 2),
						data_fig[data_fig$type=="Men", c("est", "ci")][c(2, 1, 5:3),],
						rep("", 2),
						data_fig[data_fig$type=="Men", c("est", "ci")][6,],
						rep("", 2)))

	data_phone_fig <- rbind(
					data.frame(var=colnames(est_phone),
						est=apply(est_phone, 2, mean),
						ci_l=apply(est_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_phone, 2, quantile, prob=0.975),
						type="complete",
						group="Whole dataset"),
					data.frame(var=colnames(est_M_phone),
						est=apply(est_M_phone, 2, mean),
						ci_l=apply(est_M_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_M_phone, 2, quantile, prob=0.975),
						type="Men",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest")),
					data.frame(var=colnames(est_F_phone),
						est=apply(est_F_phone, 2, mean),
						ci_l=apply(est_F_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_F_phone, 2, quantile, prob=0.975),
						type="Women",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest"))
					) %>%
					mutate(
						group=factor(group,
							levels = c("Whole dataset", "Gender strata")),
						var=factor(var,
							levels = rev(c("clinic_time","clinic_cost_travel", "clinic_cost_travel_75",
								"clinic_unable", "clinic_time", "clinic_time_25", "clinic_time_75", 
								"d_child0", "d_child1", "dest", "n", "n_adult", "n_child")),
							labels = rev(c("Time to reach a\nhealth care center",
								"Mean travel cost", "75p travel cost",
								"Being unable to access a\nhealth care center",
								"Mean time to reach\na health care centre", "25p time to reach\na health care centre", "75p time to reach\na health care centre",
								"At least 1 deceased child",
								"More than 1 deceased child",
								"Number of travel\ndestinations",
								"People living\nin the household",
								"Adults living\nin the household",
								"Children living\nin the household")))) %>%
					dplyr::mutate(
						est=round(est, 3),
						ci=paste0(round(ci_l, 3), "-", round(ci_up, 3)))

	data_phone_table <- cbind(
					data_phone_fig[data_phone_fig$type=="complete", c("var", "est", "ci")][c(1:3, 5, 4, 8, 7, 6, 9:11),],
					rbind(
						rep("", 2),
						rep("", 2),
						rep("", 2),
						data_phone_fig[data_phone_fig$type=="Women", c("est", "ci")][c(2, 1, 5:3),],
						rep("", 2),
						data_phone_fig[data_phone_fig$type=="Women", c("est", "ci")][6,],
						rep("", 2)),
					rbind(
						rep("", 2),
						rep("", 2),
						rep("", 2),
						data_phone_fig[data_phone_fig$type=="Men", c("est", "ci")][c(2, 1, 5:3),],
						rep("", 2),
						data_phone_fig[data_phone_fig$type=="Men", c("est", "ci")][6,],
						rep("", 2)))

	var_digest <- function(df, ref, comp, direction=-1){
		data_fig <- df %>%
						dplyr::mutate(sim=rep(1:boot, each=3)) %>%
						dplyr::filter(phone %in% c(ref, comp)) %>%
						dplyr::group_by(sim)

		if(direction==-1){
			data_fig <- data_fig %>%
							dplyr::arrange(sim, -phone)
		}else{
			data_fig <- data_fig %>%
							dplyr::arrange(sim, phone)
		}
	
		data_fig <- data_fig %>%
						dplyr::summarise(
							clinic_cost_travel=diff(clinic_cost_travel),
							clinic_cost_travel_75=diff(clinic_cost_travel_75),
							clinic_unable=diff(clinic_unable),
							clinic_time=diff(clinic_time),
							clinic_time_25=diff(clinic_time_25),
							clinic_time_75=diff(clinic_time_75), 
							d_child0=diff(d_child0),
							# d_child1=diff(d_child1),
							dest=diff(dest),
							n=diff(n),
							n_adult=diff(n_adult),
							n_child=diff(n_child)) %>%
						dplyr::ungroup() %>%
						dplyr::select(-sim)

		data_fig <- data.frame(
						var=colnames(data_fig),
						est=apply(data_fig, 2, mean),
						ci_l=apply(data_fig, 2, quantile, prob=0.025),
						ci_up=apply(data_fig, 2, quantile, prob=0.975))

		return(data_fig)
	}

	data_abs_fig <- rbind(
			var_digest(est_abs, 2, 1, 1) %>%
				dplyr::mutate(
					type="complete",
					group="Whole dataset",
					ref="All participants"),
			var_digest(est_abs, 0, 1, -1) %>%
				dplyr::mutate(
					type="complete",
					group="Whole dataset",
					ref="Non owners")) %>%
			dplyr::bind_rows(.,
				rbind(
					var_digest(est_M_abs, 2, 1, 1) %>%
						dplyr::mutate(
							type="Men",
							group="Gender strata",
							ref="All participants"),
					var_digest(est_M_abs, 0, 1, -1) %>%
						dplyr::mutate(
							type="Men",
							group="Gender strata",
							ref="Non owners")) %>%
				dplyr::filter(
					var %in% c("clinic_cost_travel", "clinic_cost_travel_75", "clinic_unable",
					"clinic_time", "clinic_time_25", "clinic_time_75", "dest"))) %>%
			dplyr::bind_rows(.,
				rbind(
					var_digest(est_F_abs, 2, 1, 1) %>%
						dplyr::mutate(
							type="Women",
							group="Gender strata",
							ref="All participants"),
					var_digest(est_F_abs, 0, 1, -1) %>%
						dplyr::mutate(
							type="Women",
							group="Gender strata",
							ref="Non owners")) %>%
				dplyr::filter(var %in% c("clinic_cost_travel", "clinic_cost_travel_75", "clinic_unable",
					"clinic_time", "clinic_time_25", "clinic_time_75", "dest"))) %>%
			dplyr::mutate(
				group_var=ifelse(var %in% c("clinic_time", "clinic_time_25", "clinic_time_75"),
					"Time to reach a\nhealth care center",
					ifelse(var %in% c("clinic_cost_travel", "clinic_cost_travel_75"),
						"Travel cost",
						ifelse(var %in% c("clinic_unable"),
							"Being unable to access a\nhealth care center",
							ifelse(var %in% c("d_child0"),
								"At least 1 deceased child",
								ifelse(var %in% c("dest", "dest_75", "dest_25"),
									"Number of travel\ndestinations",
									"Household characteristics")))))) %>%
			dplyr::filter(var %in% c("clinic_cost_travel", "clinic_cost_travel_75",
				"clinic_time", "clinic_time_25", "clinic_time_75",
				"dest", "dest_25", "dest_75")) %>%
			dplyr::mutate(
				group_var=ifelse(var %in% c("clinic_cost_travel", "clinic_cost_travel_75"), "Travel cost",
					ifelse(var %in% c("clinic_time", "clinic_time_25", "clinic_time_75"), "Time to reach a\nhealth care center",
						"Number of travel\ndestinations")),
				var=factor(var,
					levels = rev(c("clinic_cost_travel", "clinic_cost_travel_75",
						"clinic_time", "clinic_time_25", "clinic_time_75", 
						"dest", "dest_25", "dest_75")),
					labels = rev(
						c("Mean travel cost", "75p travel cost",
						"Mean time to reach\na health care centre", "25p time to reach\na health care centre", "75p time to reach\na health care centre",
						"Mean number of travel\ndestinations", "25p number of\ntravel destinations", "75p number of\ntravel destinations")))) %>%
			dplyr::mutate(
				est=round(est, 3),
				ci=paste0(round(ci_l, 3), "-", round(ci_up, 3)))

	data_abs_table <- cbind(
					data_abs_fig[data_abs_fig$type=="complete" & data_abs_fig$ref=="All participants", c("var", "est", "ci")],
					data_abs_fig[data_abs_fig$type=="Women" & data_abs_fig$ref=="All participants", c("est", "ci")],
					data_abs_fig[data_abs_fig$type=="Men" & data_abs_fig$ref=="All participants", c("est", "ci")])

	data_abs_phone_table <- cbind(
					data_abs_fig[data_abs_fig$type=="complete" & data_abs_fig$ref=="Non owners", c("var", "est", "ci")],
					data_abs_fig[data_abs_fig$type=="Women" & data_abs_fig$ref=="Non owners", c("est", "ci")],
					data_abs_fig[data_abs_fig$type=="Men" & data_abs_fig$ref=="Non owners", c("est", "ci")])

	title <- "Ratio - all participants"
	res <- data_table
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "ratio_participants.html"), output_format = rmarkdown::html_document(toc=FALSE))

	title <- "Ratio - non owners"
	res <- data_phone_table
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "ratio_non_owners.html"), output_format = rmarkdown::html_document(toc=FALSE))

	title <- "Difference - all participants"
	res <- data_abs_table
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "diff_participants.html"), output_format = rmarkdown::html_document(toc=FALSE))

	title <- "Difference - non owners"
	res <- data_abs_phone_table
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, "diff_non_owners.html"), output_format = rmarkdown::html_document(toc=FALSE))

	note("Tables saved...\n")
}

#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

bias_fig <- function(boot=1000, file=file.path(path.workspace, "bias_manuscript")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- as.integer(unique(c(links2015$x, links2015$y)))
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data <- rbind(data2015, data2016)
	data_ind <- rbind(data2015, data2016_ind)

	BS_imput <- function(data=data){
		 temp <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
		 temp_i <- mice::mice(temp, m=1, print=FALSE)
		 return(cbind(mice::complete(temp_i, 1), id=temp$id))
	}

	boot_imput <- replicate(boot, BS_imput(data), simplify=FALSE)
	# boot_ind_2015 <- replicate(boot, BS_imput(data2015), simplify=FALSE)
	# boot_ind_2016 <- replicate(boot, BS_imput(data2016_ind), simplify=FALSE)
	boot_ind <- replicate(boot, BS_imput(data_ind), simplify=FALSE)

	estim <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		data_total_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		ratio <- cbind(
					(data_phone_2[data_phone_2$phone==1, -1]/data_total_2[1,]),
					# (data_phone_2[data_phone_2$phone==1, 4]-data_total_2[1,4]),
					# (data_phone_2[data_phone_2$phone==1, -c(1, 4)]/data_total_2[1,-4]),
					# (data_phone_2[data_phone_2$phone==1, 4]-data_total_2[1,4]),
					(data_phone_1[data_phone_1$phone==1, -1]/data_total_1[1,]))
		return(ratio)
	}

	est <- lapply(boot_imput, estim) %>%
			do.call("rbind", .)

	est_M <- lapply(boot_imput, function(X){
				estim(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F <- lapply(boot_imput, function(X){
				estim(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est_2015 <- lapply(boot_ind, function(X){
					estim(X[X$year==2015,])
				}) %>%
				do.call("rbind", .)

	est_2016 <- lapply(boot_ind, function(X){
					estim(X[X$year==2016,])
				}) %>%
				do.call("rbind", .)

	est <- est %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M <- est_M %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F <- est_F %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_2015 <- est_2015 %>%
					dplyr::select(-clinic_cost_travel_25) %>%
					dplyr::select(-d_child1)
	est_2016 <- est_2016 %>%
					dplyr::select(-clinic_cost_travel_25) %>%
					dplyr::select(-d_child1)

	data_fig <- rbind(
					data.frame(var=colnames(est),
						est=apply(est, 2, mean),
						ci_l=apply(est, 2, quantile, prob=0.025),
						ci_up=apply(est, 2, quantile, prob=0.975),
						type="Whole dataset"),
					data.frame(var=colnames(est_M),
						est=apply(est_M, 2, mean),
						ci_l=apply(est_M, 2, quantile, prob=0.025),
						ci_up=apply(est_M, 2, quantile, prob=0.975),
						type="Men stratum"),
					data.frame(var=colnames(est_F),
						est=apply(est_F, 2, mean),
						ci_l=apply(est_F, 2, quantile, prob=0.025),
						ci_up=apply(est_F, 2, quantile, prob=0.975),
						type="Women stratum"),
					data.frame(var=colnames(est_2015),
						est=apply(est_2015, 2, mean),
						ci_l=apply(est_2015, 2, quantile, prob=0.025),
						ci_up=apply(est_2015, 2, quantile, prob=0.975),
						type="2015 stratum"),
					data.frame(var=colnames(est_2016),
						est=apply(est_2016, 2, mean),
						ci_l=apply(est_2016, 2, quantile, prob=0.025),
						ci_up=apply(est_2016, 2, quantile, prob=0.975),
						type="2016 stratum")) %>%
					mutate(
						type=factor(type,
							levels = c("Men stratum", "Women stratum", "2015 stratum", "2016 stratum", "Whole dataset")),
						var=factor(var,
							levels = rev(c("clinic_time","clinic_cost_travel", "clinic_cost_travel_75",
								"clinic_unable", "clinic_time", "clinic_time_25", "clinic_time_75", 
								"d_child0", "d_child1", "dest", "n", "n_adult", "n_child")),
							labels = rev(c("Time to reach a\nhealth care center",
								"Mean travel cost", "75p travel cost",
								"Being unable to access a\nhealth care center",
								"Mean time to reach\na health care centre", "25p time to reach\na health care centre", "75p time to reach\na health care centre",
								"At least 1 deceased child",
								"More than 1 deceased child",
								"Number of travel\ndestinations",
								"People living\nin the household",
								"Adults living\nin the household",
								"Children living\nin the household"))))

	note("Plotting...\n")
	fig <- ggplot2::ggplot(data_fig, ggplot2::aes(x=est, y=var, color=est)) +
				ggplot2::geom_errorbarh(ggplot2::aes(xmin=ci_l,
					xmax=ci_up, y=var, color=est), height=0.25) +
				ggplot2::geom_point(size=3) +
				ggplot2::geom_vline(xintercept = 1, linetype="dashed") +
				ggplot2::scale_color_gradient2(low="blue", mid="grey", high="red",
					midpoint = 1) +
				ggplot2::facet_wrap(.~type, ncol=2) +
				ggplot2::guides(colour="none") +
				ggplot2::xlab("Ratio phone/total") +
				ggplot2::ylab("") +
				ggplot2::theme_bw()

	note("Plot saved...\n")
	ggplot2::ggsave(paste0(file, ".tiff"), fig, width=8, height=10, unit="in")

	note("Table saved...\n")
	title <- "ratio"
	res <- data_fig
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, ".html"), output_format = rmarkdown::html_document(toc=FALSE))

}

#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

bias_fig2 <- function(boot=1000, file=file.path(path.workspace, "bias_manuscript2")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- as.integer(unique(c(links2015$x, links2015$y)))
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data <- rbind(data2015, data2016)
	data_ind <- rbind(data2015, data2016_ind)

	BS_imput <- function(data=data){
		 temp <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
		 temp_i <- mice::mice(temp, m=1, print=FALSE)
		 return(cbind(mice::complete(temp_i, 1), id=temp$id))
	}

	boot_imput <- replicate(boot, BS_imput(data), simplify=FALSE)
	boot_ind <- replicate(boot, BS_imput(data_ind), simplify=FALSE)

	estim <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest_75=quantile(dest, probs=0.75),
							dest_25=quantile(dest, probs=0.25),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						dplyr::ungroup() %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest_75=quantile(dest, probs=0.75),
							dest_25=quantile(dest, probs=0.25),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						dplyr::mutate(phone=2) %>%
						dplyr::select(
							phone, clinic_time_75, clinic_time_25,
							clinic_time, clinic_unable,
							dest_75, dest_25, dest,
							d_child0, d_child1) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						dplyr::ungroup() %>%
						data.frame()

		data_total_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							) %>%
						data.frame()

		return(
			rbind(
				cbind(data_phone_1, data_phone_2 %>% dplyr::select(-phone)),
				cbind(data_total_1, data_total_2)
				)
			)
	}

	est <- lapply(boot_imput, estim) %>%
			do.call("rbind", .)

	est_M <- lapply(boot_imput, function(X){
				estim(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F <- lapply(boot_imput, function(X){
				estim(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est_2015 <- lapply(boot_ind, function(X){
					estim(X[X$year==2015,])
				}) %>%
				do.call("rbind", .)

	est_2016 <- lapply(boot_ind, function(X){
					estim(X[X$year==2016,])
				}) %>%
				do.call("rbind", .)

	est <- est %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M <- est_M %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F <- est_F %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_2015 <- est_2015 %>%
					dplyr::select(-clinic_cost_travel_25) %>%
					dplyr::select(-d_child1)
	est_2016 <- est_2016 %>%
					dplyr::select(-clinic_cost_travel_25) %>%
					dplyr::select(-d_child1)

	var_digest <- function(df){
		data_fig <- data.frame(var=colnames(df)[-1],
						est=apply(df[,-1], 2, mean),
						ci_l=apply(df[,-1], 2, quantile, prob=0.025),
						ci_up=apply(df[,-1], 2, quantile, prob=0.975))
		return(data_fig)
	}

	data_fig <- rbind(
			var_digest(est[est$phone==0,]) %>%
				dplyr::mutate(phone=0),
			var_digest(est[est$phone==1,]) %>%
				dplyr::mutate(phone=1),
			var_digest(est[est$phone==2,]) %>%
				dplyr::mutate(phone=2)) %>%
			dplyr::mutate(type="Whole dataset") %>%
			dplyr::bind_rows(.,
				rbind(
					var_digest(est_M[est_M$phone==0,]) %>%
						dplyr::mutate(phone=0),
					var_digest(est_M[est_M$phone==1,]) %>%
						dplyr::mutate(phone=1),
					var_digest(est_M[est_M$phone==2,]) %>%
						dplyr::mutate(phone=2)) %>%
					dplyr::mutate(type="Men stratum")) %>%
			dplyr::bind_rows(.,
				rbind(
					var_digest(est_F[est_F$phone==0,]) %>%
						dplyr::mutate(phone=0),
					var_digest(est_F[est_F$phone==1,]) %>%
						dplyr::mutate(phone=1),
					var_digest(est_F[est_F$phone==2,]) %>%
						dplyr::mutate(phone=2)) %>%
					dplyr::mutate(type="Women stratum")) %>%
			dplyr::bind_rows(.,
				rbind(
					var_digest(est_2015[est_2015$phone==0,]) %>%
						dplyr::mutate(phone=0),
					var_digest(est_2015[est_2015$phone==1,]) %>%
						dplyr::mutate(phone=1),
					var_digest(est_2015[est_2015$phone==2,]) %>%
						dplyr::mutate(phone=2)) %>%
					dplyr::mutate(type="2015 stratum")) %>%
			dplyr::bind_rows(.,
				rbind(
					var_digest(est_2016[est_2016$phone==0,]) %>%
						dplyr::mutate(phone=0),
					var_digest(est_2016[est_2016$phone==1,]) %>%
						dplyr::mutate(phone=1),
					var_digest(est_2016[est_2016$phone==2,]) %>%
						dplyr::mutate(phone=2)) %>%
					dplyr::mutate(type="2016 stratum")) %>%
			dplyr::filter(var!="d_child1") %>%
			dplyr::mutate(group=ifelse(var %in% c("clinic_time", "clinic_time_25", "clinic_time_75"),
					"Time to reach a\nhealth care center",
					ifelse(var %in% c("clinic_cost_travel", "clinic_cost_travel_75"),
						"Travel cost",
						ifelse(var %in% c("clinic_unable"),
							"Being unable to access a\nhealth care center",
							ifelse(var %in% c("d_child0"),
								"At least 1 deceased child",
								ifelse(var %in% c("dest", "dest_75", "dest_25"),
									"Number of travel\ndestinations",
									"Household characteristics")))))) %>%
			mutate(
				type=factor(type,
					levels = c("Men stratum", "Women stratum", "2015 stratum", "2016 stratum", "Whole dataset")),
				phone=factor(phone,
					levels=0:2,
					labels=c("Not a mobile phone owner", "Mobile phone owner", "Owners and non owners")),
				var=factor(var,
					levels = rev(c("clinic_cost_travel", "clinic_cost_travel_75",
						"clinic_unable", "clinic_time", "clinic_time_25", "clinic_time_75", 
						"d_child0", "dest", "dest_25", "dest_75", "n", "n_adult", "n_child")),
					labels = rev(
						c("Mean travel cost", "75p travel cost",
						"Being unable to access a\nhealth care center",
						"Mean time to reach\na health care centre", "25p time to reach\na health care centre", "75p time to reach\na health care centre",
						"Poportion",
						"Mean number of\ntravel destinations", "25p number of\ntravel destinations", "75p number of\ntravel destinations",
						"Mean number of people\nliving in the household",
						"Mean number of adults\nliving in the household",
						"Mean number of children\nliving in the household"))))

	note("Plotting...\n")
	group_level <- rev(sort(unique(data_fig$group)))
	label_axis <- c("Cost in dollars",
					"Time in hours",
					"",
					"",
					"Proportion",
					"Proportion")
	for(i in 1:length(group_level)){
		assign(
			paste0("temp_", i),
			if(i==1){
				ggplot2::ggplot(
					data_fig %>%
						dplyr::filter(group==group_level[i]),
					ggplot2::aes(x=est, y=var, color=phone)) +
						ggplot2::geom_errorbarh(
							ggplot2::aes(xmin=ci_l,
							xmax=ci_up, y=var, color=phone),
							height=0.25,
							position=ggplot2::position_dodge(width=0.5)) +
						ggplot2::geom_point(
							size=3,
							position=ggplot2::position_dodge(width=0.5)) +
						viridis::scale_color_viridis(
							name="",
							option="D",
							discrete=TRUE) +
						ggplot2::coord_flip() +
						ggplot2::facet_grid(type~group) +
						ggplot2::xlab(label_axis[i]) +
						ggplot2::ylab("") +
						ggplot2::theme_bw()
			}else{
				ggplot2::ggplot(
					data_fig %>%
						dplyr::filter(group==group_level[i]),
					ggplot2::aes(x=est, y=var, color=phone)) +
						ggplot2::geom_errorbarh(
							ggplot2::aes(xmin=ci_l,
							xmax=ci_up, y=var, color=phone),
							height=0.25,
							position=ggplot2::position_dodge(width=0.5)) +
						ggplot2::geom_point(
							size=3,
							position=ggplot2::position_dodge(width=0.5)) +
						viridis::scale_color_viridis(
							name="",
							option="D",
							discrete=TRUE) +
						ggplot2::coord_flip() +
						ggplot2::facet_grid(type~group) +
						ggplot2::xlab(label_axis[i]) +
						ggplot2::ylab("") +
						ggplot2::theme_bw() +
						ggplot2::theme(
							legend.position="none",
							strip.background.y = ggplot2::element_blank(),
							strip.text.y = ggplot2::element_blank())
			}
		)
	}

	# fig <- grid::grid.draw(
	# 		cbind(
	# 			ggplot2::ggplotGrob(temp_6),
	# 			ggplot2::ggplotGrob(temp_5),
	# 			ggplot2::ggplotGrob(temp_4),
	# 			ggplot2::ggplotGrob(temp_3),
	# 			ggplot2::ggplotGrob(temp_2),
	# 			ggplot2::ggplotGrob(temp_1),
	# 			size = "last"))

	fig <- gridExtra::arrangeGrob(
				ggplot2::ggplotGrob(temp_6),
				ggplot2::ggplotGrob(temp_5),
				ggplot2::ggplotGrob(temp_4),
				ggplot2::ggplotGrob(temp_3),
				ggplot2::ggplotGrob(temp_2),
				ggplot2::ggplotGrob(temp_1),
				nrow=1,
				layout_matrix=matrix(c(1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6), nrow=1))

	note("Plot saved...\n")
	ggplot2::ggsave(
		paste0(file, ".tiff"),
		fig, width=24, height=8, unit="in")

	note("Table saved...\n")
	title <- "ratio"
	res <- data_fig
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, ".html"), output_format = rmarkdown::html_document(toc=FALSE))

}

#' Creating a figure on the difference in mean time to reach a health care center
#'
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

access_fig <- function(file=file.path(path.workspace, "access.tiff")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y))
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	note("Pooled estimate based on matching...")
	data_imp <- mice::mice(rbind(data2015, data2016), m=15, seed=150, print=FALSE)
	# data_imp_ind <- mice::mice(rbind(data2015, data2016_ind), m=15, seed=150, print=FALSE)

	trim_iter <- function(imp){
		mod_mat <- stats::model.matrix(phone~age+sex+dest+child_house+adult_house+travel+clinic_time+year+hsv, data=imp)
		imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
		colnames(imp) <- gsub("-", "_", colnames(imp))
		colnames(imp) <- gsub(">=", "_", colnames(imp))
		colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
		colnames(imp) <- gsub("\\.", "_", colnames(imp))
		repeat{
			n <- nrow(imp)
			mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
				data=imp)
			check <- apply(mod_mat, 2, mean) # Initiating a check
			check <- !(check==1 | check==0) & names(check)!="clinic_time"
			form <- paste0("phone~", paste(names(check)[check], collapse="+"))
			mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
			imp$p <- stats::predict(mod, type="response")
			cutoff_low <- min(imp$p[imp$phone==1])
			cutoff_high <- max(imp$p[imp$phone==0])
			imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
			n_trim <- nrow(imp)
			if(n_trim==n){
				break
			}
		}
		return(imp)
	}

	data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	data_imp_ind <- lapply(1:15, function(x){mice::complete(data_imp_ind, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	for(i in 1:length(data_imp)){
		data_imp[[i]]$m <- i
	}
	for(i in 1:length(data_imp_ind)){
		data_imp_ind[[i]]$m <- i
	}

	# Doing the calculation with matching
	match_est <- data_imp %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_combined <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_combined$varest)
	var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

	match_combined <- rbind(match_combined,
							data.frame(m=99,
								est=mean(match_combined$est),
								varest=var_within+(1+(1/nrow(match_combined)))*var_between))

	note("Gender stratification...\n")
	estim_gender <- function(filter="F"){
		if(filter=="F"){
			seed <- 550
		}else{
			seed <- 222
		}
		data_imp <- rbind(data2015, data2016) %>%
						dplyr::filter(sex==filter) %>%
						mice::mice(., m=15, seed=seed, print=FALSE)

		trim_iter <- function(imp){
			mod_mat <- stats::model.matrix(phone~age+dest+child_house+adult_house+travel+clinic_time+year+hsv, data=imp)
			imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
			colnames(imp) <- gsub("-", "_", colnames(imp))
			colnames(imp) <- gsub(">=", "_", colnames(imp))
			colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
			colnames(imp) <- gsub("\\.", "_", colnames(imp))
			repeat{
				n <- nrow(imp)
				mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
					data=imp)
				check <- apply(mod_mat, 2, mean) # Initiating a check
				check <- !(check==1 | check==0) & names(check)!="clinic_time"
				form <- paste0("phone~", paste(names(check)[check], collapse="+"))
				mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
				imp$p <- stats::predict(mod, type="response")
				cutoff_low <- min(imp$p[imp$phone==1])
				cutoff_high <- max(imp$p[imp$phone==0])
				imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
				n_trim <- nrow(imp)
				if(n_trim==n){
					break
				}
			}
			return(imp)
		}

		data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
							lapply(., function(temp){
								mod <- with(data=temp, glm(phone~age+dest+child_house+adult_house+travel+clinic_time+year+hsv, family = binomial(link = "logit")))
								temp$p <- stats::predict(mod, type="response")
								return(temp)
							}) %>%
							lapply(., trim_iter)

		for(i in 1:length(data_imp)){
			data_imp[[i]]$m <- i
		}

		match_est <- data_imp %>%
						lapply(., function(temp){
							temp <- temp %>%
									dplyr::mutate(lp=log(p/(1-p)))
							m_phone <- median(temp$lp[temp$phone==1])
							# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
							temp <- temp %>%
									dplyr::mutate(mark=-abs(lp-m_phone)) %>%
									dplyr::arrange(-phone, mark) %>%
									dplyr::select(-mark)
							return(temp)
							}) %>%
						do.call("rbind", .)

		match_combined <- lapply(1:max(match_est$m), function(x){
								data.frame(m=x,
									est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
									varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
							}) %>%
							do.call("rbind", .)

		var_within <- mean(match_combined$varest)
		var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

		match_combined <- rbind(match_combined,
								data.frame(m=99,
									est=mean(match_combined$est),
									varest=var_within+(1+(1/nrow(match_combined)))*var_between))

		return(match_combined)
	}

	match_combined_F <- estim_gender()
	match_combined_M <- estim_gender("M")

	note("Year stratification...\n")
	estim_year <- function(filter=2015){
		if(filter==2015){
			seed <- 505
		}else{
			seed <- 999
		}
		data_imp <- rbind(data2015, data2016_ind) %>%
						dplyr::filter(year==filter) %>%
						mice::mice(., m=15, seed=seed, print=FALSE)

		trim_iter <- function(imp){
			mod_mat <- stats::model.matrix(phone~age+dest+child_house+adult_house+travel+clinic_time+sex+hsv, data=imp)
			imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
			colnames(imp) <- gsub("-", "_", colnames(imp))
			colnames(imp) <- gsub(">=", "_", colnames(imp))
			colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
			colnames(imp) <- gsub("\\.", "_", colnames(imp))
			repeat{
				n <- nrow(imp)
				mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
					data=imp)
				check <- apply(mod_mat, 2, mean) # Initiating a check
				check <- !(check==1 | check==0) & names(check)!="clinic_time"
				form <- paste0("phone~", paste(names(check)[check], collapse="+"))
				mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
				imp$p <- stats::predict(mod, type="response")
				cutoff_low <- min(imp$p[imp$phone==1])
				cutoff_high <- max(imp$p[imp$phone==0])
				imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
				n_trim <- nrow(imp)
				if(n_trim==n){
					break
				}
			}
			return(imp)
		}

		data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
							lapply(., function(temp){
								mod <- with(data=temp, glm(phone~age+dest+child_house+adult_house+travel+clinic_time+sex+hsv, family = binomial(link = "logit")))
								temp$p <- stats::predict(mod, type="response")
								return(temp)
							}) %>%
							lapply(., trim_iter)

		for(i in 1:length(data_imp)){
			data_imp[[i]]$m <- i
		}

		match_est <- data_imp %>%
						lapply(., function(temp){
							temp <- temp %>%
									dplyr::mutate(lp=log(p/(1-p)))
							m_phone <- median(temp$lp[temp$phone==1])
							# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
							temp <- temp %>%
									dplyr::mutate(mark=-abs(lp-m_phone)) %>%
									dplyr::arrange(-phone, mark) %>%
									dplyr::select(-mark)
							return(temp)
							}) %>%
						do.call("rbind", .)

		match_combined <- lapply(1:max(match_est$m), function(x){
								data.frame(m=x,
									est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
									varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
							}) %>%
							do.call("rbind", .)

		var_within <- mean(match_combined$varest)
		var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

		match_combined <- rbind(match_combined,
								data.frame(m=99,
									est=mean(match_combined$est),
									varest=var_within+(1+(1/nrow(match_combined)))*var_between))

		return(match_combined)
	}

	match_combined_2015 <- estim_year()
	match_combined_2016 <- estim_year(2016)

	note("Plotting...\n")

	fig <- rbind(match_combined[16,] %>% dplyr::mutate(type="Complete\ndataset", col=viridis::viridis(3)[1]),
			match_combined_M[16,] %>% dplyr::mutate(type="Men\nstratum", col=viridis::viridis(3)[2]),
			match_combined_F[16,] %>% dplyr::mutate(type="Women\nstratum", col=viridis::viridis(3)[2]),
			match_combined_2015[16,] %>% dplyr::mutate(type="2015\nstratum", col=viridis::viridis(3)[3]),
			match_combined_2016[16,] %>% dplyr::mutate(type="2016\nstratum", col=viridis::viridis(3)[3])) %>%
				dplyr::mutate(type=factor(type,
					levels=rev(c("Complete\ndataset", "Men\nstratum", "Women\nstratum", "2015\nstratum", "2016\nstratum")))) %>%
			ggplot2::ggplot(., ggplot2::aes(x=type, y=est, color=I(col))) +
				ggplot2::geom_point(size=3) +
				ggplot2::geom_errorbar(ggplot2::aes(ymin=est-1.96*sqrt(varest), ymax=est+1.96*sqrt(varest), color=I(col)), width=0.1) +
				ggplot2::geom_hline(yintercept = 0, linetype="dashed") +
				ggplot2::coord_flip() +
				ggplot2::xlab("") +
				ggplot2::ylab("Mean reduction in travel time\nto a health care center") +
				ggplot2::theme_bw()

	ggplot2::ggsave(file, fig, width=8, height=8, unit="in")
	note("Plot saved...\n")

	return(fig)
}


#' Creating a descriptive table
#'
#' @param save It is a TRUE/FALSE option. If TRUE (the default) figure is save as a tiff.
#' @param title Title of the table on the word document.
#' @param file The output_file pathway. The file will be a docx file.
#' @return A ggplot2 figure.

desc_table <- function(save=TRUE, title="description", file=file.path(path.workspace, "desc_table.html")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	nodes2016$phone_use_ever <- ifelse(nodes2016$phone_use_ever=="No data", NA,
									ifelse(nodes2016$phone_use_ever=="Already used a phone", "Yes", "No"))

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015$phone[nodes2015$id %in% c(96:98)] <- "Children"
	nodes2015 <- dplyr::filter(nodes2015, !(is.na(age) & id %in% c(96:98)))

	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	nodes2015$phone_use_ever <- ifelse(nodes2015$phone_use_ever=="No data", NA,
									ifelse(nodes2015$phone_use_ever=="Already used a phone", "Yes", "No"))

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					dplyr::mutate(year=2015)
					# dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
					# 	hsv=ifelse(hsv=="Positive", 1, 0),
					# 	sex=factor(sex, levels = c("F", "M")),
					# 	age=factor(as.character(age),
					# 		levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
					# 	year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					dplyr::mutate(year=2015)
					# dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
					# 	hsv=ifelse(hsv=="Positive", 1, 0),
					# 	sex=factor(sex, levels = c("F", "M")),
					# 	age=factor(as.character(age),
					# 		levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
					# 	year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children" & nodes2016$age_c==0,] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					dplyr::mutate(year=2016)
					# dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
					# 	hsv=ifelse(hsv=="Positive", 1, 0),
					# 	sex=factor(sex, levels = c("F", "M")),
					# 	age=factor(as.character(age),
					# 		levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
					# 	year=2016)

	data2016_red <- nodes2016[nodes2016$phone!="Children" & nodes2016$age_c==0,] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					dplyr::mutate(year=2016)
					# dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
					# 	hsv=ifelse(hsv=="Positive", 1, 0),
					# 	sex=factor(sex, levels = c("F", "M")),
					# 	age=factor(as.character(age),
					# 		levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
					# 	year=2016)

	data <- rbind(data2015, data2016_red)
	data_red <- rbind(data2015, data2016)

	desc <- function(X){
		phone <- X %>%
					dplyr::select(phone) %>%
					dplyr::group_by(phone) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1))
					
		if(nrow(phone)==2){
			phone <- rbind(phone, rep(NA, 3))
			phone[3, 2:3] <- 0
		}

		phone <- phone %>%
					dplyr::mutate(phone=c("Yes", "No", "Missing value"))
		phone[nrow(phone)+1, 1] <- "Phone ownership"
		phone <- data.frame(phone[c(nrow(phone), 1:(nrow(phone)-1)),])
		colnames(phone)[1] <- "var"

		phone_use <- X %>%
					dplyr::select(phone_use_ever) %>%
					dplyr::group_by(phone_use_ever) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1))
					
		if(nrow(phone_use)==2){
			phone_use <- rbind(phone_use, rep(NA, 3))
			phone_use[3, 2:3] <- 0
		}

		phone_use <- phone_use %>%
						dplyr::mutate(phone_use_ever=c("No", "Yes", "Missing value"))
		phone_use[nrow(phone_use)+1, 1] <- "Ever used a phone"
		phone_use <- data.frame(phone_use[c(nrow(phone_use), 1:(nrow(phone_use)-1)),])
		colnames(phone_use)[1] <- "var"

		if(length(unique(na.omit(X$sex)))==1){
			sex <- data.frame(var=rep(NA, 3),
						n=rep(NA, 3),
						p=rep(NA, 3))
		}else{
			sex <- X %>%
					dplyr::select(sex) %>%
					dplyr::group_by(sex) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1),
						sex=c("Women", "Men"))
			sex[nrow(sex)+1, 1] <- "Gender"
			sex <- data.frame(sex[c(nrow(sex), 1:(nrow(sex)-1)),])
			colnames(sex)[1] <- "var"
		}

		if(any(na.omit(X$age)==">=60")){
			age <- X %>%
					dplyr::select(age) %>%
					dplyr::group_by(age) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1),
						age=as.character(age))
			if(any(is.na(age$age))){
				age$age[is.na(age$age)] <- "Missing value"
			}else{
				age[nrow(age)+1, 1] <- "Missing value"
				age[nrow(age), 2:3] <- 0
			}
			age[nrow(age)+1, 1] <- "Age group"
			age <- data.frame(age[c(nrow(age), 1:(nrow(age)-1)),])
			colnames(age)[1] <- "var"
		}else{
			age <- X %>%
					dplyr::select(age) %>%
					dplyr::group_by(age) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1),
						age=as.character(age))
			age[nrow(age),1] <- "Missing value"
			age[nrow(age)+1, 1] <- "Age group"
			age <- data.frame(age[c(nrow(age), 1:(nrow(age)-1)),])
			colnames(age)[1] <- "var"
		}

		if(length(unique(na.omit(X$year)))==1){
			year <- data.frame(var=rep(NA, 3),
						n=rep(NA, 3),
						p=rep(NA, 3))
		}else{
			year <- X %>%
						dplyr::select(year) %>%
						dplyr::group_by(year) %>%
						dplyr::summarise(n=dplyr::n()) %>%
						dplyr::mutate(p=round(100*n/nrow(X), digits=1),
							year=as.character(year))
			year[nrow(year)+1, 1] <- "Recruitment year"
			year <- data.frame(year[c(nrow(year), 1:(nrow(year)-1)),])
			colnames(year)[1] <- "var"
		}
	
		time <- rbind(
					X %>%
						dplyr::select(clinic_time) %>%
						dplyr::filter(!is.na(clinic_time)) %>%
						dplyr::summarise(n=dplyr::n(),
							p=round(mean(clinic_time), digits=1)),
					X %>%
						dplyr::select(clinic_time) %>%
						dplyr::filter(is.na(clinic_time)) %>%
						dplyr::summarise(n=dplyr::n(),
							p=NA)) %>%
					dplyr::mutate(var=c("Time to reach\na health care center",
						"Missing values"))
		time <- time[,c(3,1,2)]

		travel <- X %>%
					dplyr::select(travel) %>%
					dplyr::group_by(travel) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1),
						travel=as.character(travel))
		travel[nrow(travel),1] <- "Missing value"
		travel[nrow(travel)+1, 1] <- "Mean to reach a\nhealth care center"
		travel <- data.frame(travel[c(nrow(travel), 2, 1, 3),])
		colnames(travel)[1] <- "var"

		unable <- X %>%
					dplyr::select(clinic_unable) %>%
					dplyr::group_by(clinic_unable) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1),
						clinic_unable=c("No", "Yes", "Missing values"))
		unable[nrow(unable)+1, 1] <- "Ever been unable to access a\nhealth care center"
		unable <- data.frame(unable[c(nrow(unable), 2, 1, 3),])
		colnames(unable)[1] <- "var"

		# travel_cost <- rbind(
		# 				X %>%
		# 					dplyr::select(clinic_cost_travel) %>%
		# 					dplyr::filter(!is.na(clinic_cost_travel)) %>%
		# 					dplyr::summarise(n=dplyr::n(),
		# 						p=round(mean(clinic_cost_travel), digits=1)),
		# 				X %>%
		# 					dplyr::select(clinic_cost_travel) %>%
		# 					dplyr::filter(is.na(clinic_cost_travel)) %>%
		# 					dplyr::summarise(n=dplyr::n(),
		# 						p=NA)) %>%
		# 				dplyr::mutate(var=c("Travel cost",
		# 					"Missing values"))
		# travel_cost <- travel_cost[,c(3,1,2)]

		if(any(is.na(X$d_child0))==TRUE){
			d_child0 <- X %>%
						dplyr::select(d_child0) %>%
						dplyr::group_by(d_child0) %>%
						dplyr::summarise(n=dplyr::n()) %>%
						dplyr::mutate(p=round(100*n/nrow(X), digits=1),
							d_child0=c("No", "Yes", "Missing values"))
			d_child0[nrow(d_child0)+1, 1] <- "Reported at least\n1 diceased child"
			d_child0 <- data.frame(d_child0[c(nrow(d_child0), 2, 1, 3),])
			colnames(d_child0)[1] <- "var"
		}else{
			d_child0 <- X %>%
						dplyr::select(d_child0) %>%
						dplyr::group_by(d_child0) %>%
						dplyr::summarise(n=dplyr::n()) %>%
						dplyr::mutate(p=round(100*n/nrow(X), digits=1),
							d_child0=c("No", "Yes"))
			d_child0 <- rbind(
							d_child0,
							c(NA, 0))
			d_child0[nrow(d_child0)+1, 1] <- "Reported at least\n1 diceased child"
			d_child0 <- data.frame(d_child0[c(nrow(d_child0), 2, 1, 3),])
			colnames(d_child0)[1] <- "var"
		}

		if(any(is.na(X$hsv))==TRUE){
			hsv <- X %>%
					dplyr::select(hsv) %>%
					dplyr::group_by(hsv) %>%
					dplyr::summarise(n=dplyr::n()) %>%
					dplyr::mutate(p=round(100*n/nrow(X), digits=1),
							hsv=c("Negative", "Positive", "Missing values"))
			hsv[nrow(hsv)+1, 1] <- "HSV shedding"
			hsv <- data.frame(hsv[c(nrow(hsv), 2, 1, 3),])
			colnames(hsv)[1] <- "var"
		}else{
			hsv <- X %>%
						dplyr::select(hsv) %>%
						dplyr::group_by(hsv) %>%
						dplyr::summarise(n=dplyr::n()) %>%
						dplyr::mutate(p=round(100*n/nrow(X), digits=1),
							hsv=c("Negative", "Positive"))
			hsv <- rbind(
							hsv,
							c(NA, 0))
			hsv[nrow(hsv)+1, 1] <- "HSV shedding"
			hsv <- data.frame(hsv[c(nrow(hsv), 2, 1, 3),])
			colnames(hsv)[1] <- "var"
		}

		dest <- X %>%
					dplyr::select(dest) %>%
					dplyr::filter(!is.na(dest)) %>%
					dplyr::summarise(n=dplyr::n(),
						p=round(mean(dest), digits=1)) %>%
					dplyr::mutate(var="Number of destination\ntraveled to recently")
		dest <- dest[,c(3,1,2)]

		travel_cost <- X %>%
					dplyr::select(clinic_cost_travel) %>%
					dplyr::filter(!is.na(clinic_cost_travel)) %>%
					dplyr::summarise(n=dplyr::n(),
						p=round(mean(clinic_cost_travel), digits=1)) %>%
					dplyr::mutate(var="Travel cost to reach\na health care center")
		travel_cost <- travel_cost[,c(3,1,2)]

		empty <- rep("", 3)

		res <- rbind(year,
					empty,
					age,
					empty,
					sex,
					empty,
					phone,
					empty,
					phone_use,
					empty,
					dest,
					empty,
					time,
					travel_cost,
					empty,
					travel,
					empty,
					unable,
					d_child0,
					hsv)
		return(res)
	}
	global <- desc(data_red)
	global_alt <- desc(data)
	stratum_2015 <- desc(data2015)
	stratum_2016 <- desc(data2016_red)
	stratum_M <- desc(data_red[data_red$sex=="M",])
	stratum_F <- desc(data_red[data_red$sex=="F",])

	res <- cbind(
			global,
			global_alt[,-1],
			stratum_2015[,-1],
			stratum_2016[,-1],
			stratum_F[,-1],
			stratum_M[,-1])

	if(save){
		rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
			output_file=file, output_format = rmarkdown::html_document(toc=FALSE))
		note("\n....report generated\n")
	}
	# res

}

#' Creating a Love plot to compare phone owners and non-owners
#'
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

love_fig <- function(file=file.path(path.workspace, "love_phone")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y))
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_mis <- data2016_ind %>%
						dplyr::mutate(
							phone=ifelse((other_id %in% na.omit(as.character(nodes2015$id)))==TRUE, NA, phone)) %>%
						dplyr::select(-other_id)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	data2015_mis <- data2015 %>%
						dplyr::mutate(
							phone=ifelse((as.character(id) %in% na.omit(nodes2016$other_id))==TRUE, NA, phone))

	data_imp <- mice::mice(rbind(data2015, data2016) %>% select(-id), m=15, seed=150)
	data_ind_imp <- mice::mice(rbind(data2015, data2016_ind) %>% select(-id), m=15, seed=150)
	# data_red_imp <- mice::mice(rbind(data2015_red, data2016) %>% select(-id), m=15, seed=150)
	# data_mis_imp <- mice::mice(rbind(data2015_mis, data2016_mis) %>% select(-id), m=15, seed=150)

	data_imp_2015 <- lapply(1:15, function(x){
						temp <- mice::complete(data_ind_imp, x) %>%
									filter(year==2015) %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_2016 <- lapply(1:15, function(x){
						temp <- mice::complete(data_ind_imp, x) %>%
									filter(year==2016) %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_M <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								filter(sex=="M") %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_F <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									filter(sex=="F") %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	mean_diff_fact <- data_imp[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp[, c(1, 8:9, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	var_num <- data_imp[, c(1, 8:9, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])

	## Pooling difference testing
	z_fact <- data_imp[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:9] <- z_fact[,2:9]/sqrt(se_fact[,2:9])

	t_num <- data_imp[, c(1, 8:9, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp[, c(1, 8:9, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	glob <- data.frame(t=apply(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num))) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Whole dataset")

	# 2015
	mean_diff_fact <- data_imp_2015[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp_2015[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp_2015[, c(1, 8:9, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	var_num <- data_imp_2015[, c(1, 8:9, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])

	## Pooling difference testing
	z_fact <- data_imp_2015[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_2015[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:9] <- z_fact[,2:9]/sqrt(se_fact[,2:9])

	t_num <- data_imp_2015[, c(1, 8:9, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_2015[, c(1, 8:9, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_2015 <- data.frame(var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1]),
						t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1], 2, mean), digits=3),
						p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="2015")

	# 2016
	mean_diff_fact <- data_imp_2016[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp_2016[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp_2016[, c(1, 8:9, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	var_num <- data_imp_2016[, c(1, 8:9, 13:14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])

	## Pooling difference testing
	z_fact <- data_imp_2016[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_2016[, c(1:7,10:12,14)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),,
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:9] <- z_fact[,2:9]/sqrt(se_fact[,2:9])

	t_num <- data_imp_2016[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_2016[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_2016 <- data.frame(var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1]),
						t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1], 2, mean), digits=3),
						p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="2016")

	# Men
	mean_diff_fact <- data_imp_M[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp_M[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:9] <- mean_diff_fact[,2:9]/sqrt(var_fact[,2:9])

	mean_diff_num <- data_imp_M[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	var_num <- data_imp_M[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])

	## Pooling difference testing
	z_fact <- data_imp_M[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_M[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:8] <- z_fact[,2:8]/sqrt(se_fact[,2:8])

	t_num <- data_imp_M[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_M[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_M <- data.frame(var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1]),
						t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1], 2, mean), digits=3),
						p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="M")

	# Women
	mean_diff_fact <- data_imp_F[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp_F[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:9] <- mean_diff_fact[,2:9]/sqrt(var_fact[,2:9])

	mean_diff_num <- data_imp_F[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	var_num <- data_imp_F[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])

	## Pooling difference testing
	z_fact <- data_imp_F[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_F[, c(1:6,9:12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:8] <- z_fact[,2:8]/sqrt(se_fact[,2:8])

	t_num <- data_imp_F[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_F[, c(1, 7:8, 12)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_F <- data.frame(var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1]),
						t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1], 2, mean), digits=3),
						p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="F")

	note("Plotting...\n")
	fig <- rbind(glob,
			stratum_2015,
			stratum_2016,
			stratum_M,
			stratum_F) %>%
			dplyr::mutate(var=factor(var,
				levels=c("age16_25","age26_35", "age36_45", "age46_59", "age_60", "sexM", "hsv", "clinic_unable", "clinic_time", "travelCar", "dest"),
				labels=c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
					"Men", "HSV shedding", "Unable to access a\nhealth care center", "Time to reach a\nhealth care center",
					"Able to reach a\nhealth care using a car", "Number of travel\ndestinations"))) %>%
			ggplot2::ggplot(., ggplot2::aes(x=t, y=var, color=t)) +
				ggplot2::geom_point(ggplot2::aes(shape=p2), size=2) +
				ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
				ggplot2::scale_color_gradient2(low="blue", mid="grey", high="red",
					midpoint = 0) +
				ggplot2::facet_wrap(.~type, ncol=2) +
				ggplot2::guides(colour="none", shape=ggplot2::guide_legend(title="")) +
				ggplot2::xlab("Standardized difference") +
				ggplot2::ylab("") +
				ggplot2::theme_bw() +
				ggplot2::theme(legend.position = "bottom")

	fig

	note("Plot saved...\n")
	ggplot2::ggsave(paste0(file, ".tiff"), fig, width=8, height=10, unit="in")

	res <- cbind(glob[,c(2, 1, 3)],
					stratum_2015[,c(2, 3)],
					stratum_2016[,c(2, 3)],
					rbind(stratum_F[1:5, c(2, 3)],
						c(NA, NA),
						stratum_F[6:10, c(2, 3)]),
					rbind(stratum_M[1:5, c(2, 3)],
						c(NA, NA),
						stratum_M[6:10, c(2, 3)]))
	res[,2:11] <- apply(res[,2:11], 2, function(x){round(x, digits=3)})
	title <- "Love plot"

	note("Table saved...\n")
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, ".html"), output_format = rmarkdown::html_document(toc=FALSE))
	note("\n....report generated\n")
}

#' Creating a figure on the hsv tests with the network
#'
#' @param year Year of the data used.
#' @param participant_only TRUE/FALSE parameter to choose if only survey participants will be included.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

hsv_net <- function(participant_only=FALSE, palette="C", file=file.path(path.workspace, "hsv_net_upd3.tiff")){
	graph_net <- function(year=2015, legend=FALSE, participant_only=participant_only){
		if(year==2016){
			net <- net2016
		}else{
			net <- net2015
		}
		data <- net_d_manage(net, verbose=FALSE)
		nodes <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	
		if(year==2016){
			edges <- edge2016
			nodes <- nodes[!(nodes$id==216 & nodes$phone_own_duration==24),] %>%
						dplyr::filter(!(id==214 & adult_house==4)) %>%
						dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
								ifelse(phone_own>0, "Phone", "No phone")),
							phone_compound=ifelse(is.na(other_phone_own), "No data",
								ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
						dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
								ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
								phone_c2=ifelse(phone=="Phone", "Phone owned",
								ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
									ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
						dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
								phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
									ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
						dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
						dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
						dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
						dplyr::mutate(sex=ifelse(is.na(sex), NA,
							ifelse(sex==2, "F", "M")),
							hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))
	
			links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
			links$min <- apply(links[,1:2],1,min)
			links$max <- apply(links[,1:2],1,max)
			links <- links[links$min!=links$max,]
			links$comb <- paste(links$min, links$max,sep="-")
			links <- links[!duplicated(links$comb),c("x","y")]
			links_l <- links
			links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
								"Phone", "No Phone")
			links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
								(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
									"Phone", "No Phone")
			nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
									nodes$id %in% nodes$id[nodes$phone=="Phone"],
									"Phone", "No Phone")
			links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
								(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
									"Phone", "No Phone")
			links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
								(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
									"Phone", "No Phone")
			nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
									nodes$id %in% nodes$id[nodes$phone=="Phone"],
									"Phone", "No Phone")
		}else{
			edges <- edge2015
			nodes <- nodes %>%
						dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
								ifelse(phone_own>0, "Phone", "No phone")),
							phone_compound=ifelse(is.na(other_phone_own), "No data",
								ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
						dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
								ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
								phone_c2=ifelse(phone=="Phone", "Phone owned",
								ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
									ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
						dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
						dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))
	
			links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
			links$min <- apply(links[,1:2],1,min)
			links$max <- apply(links[,1:2],1,max)
			links <- links[links$min!=links$max,]
			links$comb <- paste(links$min, links$max,sep="-")
			links <- links[!duplicated(links$comb),c("x","y")]
			l_list <- unique(c(links$x, links$y))
			temp <- net[[2]][, c("id", "age", "sex")] %>%
						dplyr::mutate(id=as.integer(id))
			l_list <- data.frame(id=as.integer(l_list[!(l_list %in% as.character(nodes$id))])) %>% # List of ID in the file for children but not in the other ones
						dplyr::left_join(., temp, by="id")
			nodes <- nodes %>%
							dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
							dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
			nodes[match(l_list$id, nodes$id), c("age", "sex")] <- l_list[na.omit(match(nodes$id, l_list$id)), c("age", "sex")]
			nodes <- nodes %>%
						dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
						dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
						dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
							phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
									ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
						# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
						dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
						dplyr::mutate(sex=ifelse(is.na(sex), NA,
							ifelse(sex==2, "F", "M")),
							hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))
	
			links_l <- links
			links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
								"Phone", "No Phone")
			links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
								(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
									"Phone", "No Phone")
			nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
									nodes$id %in% nodes$id[nodes$phone=="Phone"],
									"Phone", "No Phone")
			links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
								(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
									"Phone", "No Phone")
			links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
								(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
									"Phone", "No Phone")
			nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
									nodes$id %in% nodes$id[nodes$phone=="Phone"],
									"Phone", "No Phone")
		}
	
		if(participant_only){
			edges <- edges[grepl("sp", edges$id)==FALSE & grepl("sp", edges$p_id)==FALSE,]
		}
		edges <- setNames(edges, c("x", "y"))
		edges$link <- unlist(lapply(1:nrow(edges), function(x){
			paste(sort(t(edges)[,x]), collapse="-")
		}))
		links$link <- unlist(lapply(1:nrow(links), function(x){
			paste(sort(t(links)[,x]), collapse="-")
		}))
		links_df <- rbind(edges, links) %>%
						dplyr::group_by(link) %>%
						dplyr::mutate(n=dplyr::row_number()) %>%
						dplyr::ungroup() %>%
						dplyr::filter(n==1) %>%
						dplyr::select(x, y, link)
		links_df$x <- ifelse(substr(links_df$x, 1, 1)=="0", substr(links_df$x, 2, nchar(links_df$x)), links_df$x)
		links_df$y <- ifelse(substr(links_df$y, 1, 1)=="0", substr(links_df$y, 2, nchar(links_df$y)), links_df$y)

		added_id <- unique(c(edges$x, edges$y, as.character(nodes$id)))
	
		net <- igraph::graph_from_data_frame(
					d=unique(links_df[,c("x", "y")]),
					vertices=data.frame(id=added_id),
					directed=FALSE)
		gnet <- intergraph::asNetwork(igraph::simplify(net))
	
		d_dist <- data.frame(
					id=added_id,
					d=igraph::degree(net)) %>%
					dplyr::left_join(
						.,
						nodes %>%
							dplyr::mutate(
								id=as.character(id)) %>%
							dplyr::select(id, hsv),
						by="id") %>%
					dplyr::mutate(hsv=ifelse(is.na(hsv), "Not tested", hsv)) %>%
					dplyr::mutate(hsv=factor(hsv,
									levels=c("Not tested", "Negative", "Positive"),
									labels=c("Not tested", "No HSV shedding", "HSV shedding")))

		color_code <- c("lightgrey", viridis::viridis(2, option="B"))
		degree_dist <- ggplot2::ggplot(
						d_dist,
						ggplot2::aes(x=d, fill=hsv)) +
						ggplot2::geom_bar(stat="count") +
						ggplot2::scale_fill_manual(
							name="",
							values=color_code) +
						ggplot2::xlab("Number of edges") +
						ggplot2::ylab("Count") +
						ggplot2::guides(
							fill = ggplot2::guide_legend(override.aes = list(size = 4))) +
						ggplot2::theme_bw() +
						ggplot2::theme(
							legend.position = c(0.63, 0.75),
							legend.box.background = element_rect(colour = "black"),
							legend.title = ggplot2::element_text(size = 6),
							legend.text  = ggplot2::element_text(size = 9),
							legend.key.size = ggplot2::unit(0.5, "lines"),
							axis.title=ggplot2::element_text(size=8, face="bold"))

		links_df$part <- ifelse(links_df$link %in% edges$link, "solid", "dotted")
		links_df$part_col <- ifelse(links_df$link %in% edges$link, "grey85", "grey40")
	
		set.seed(20)
		fig <- GGally::ggnet2(gnet, mode = "fruchtermanreingold",
					alpha=0.8, size=3)
	
		coord <- fig$data[,c("label", "x", "y")]
					# dplyr::mutate(
					# 	x=ifelse(x<0.5, x-(1.4*(abs(x-0.5)+abs(x-0.5)^0.5)), x+(1.4*((x-0.5)+(x-0.5)^0.5))),
					# 	y=ifelse(y<0.5, y-(1.25*(abs(y-0.5)+abs(y-0.5)^0.5)), y+(1.25*((y-0.5)+(y-0.5)^0.5))))
	
		if(participant_only){
			hsv <- dplyr::left_join(
						hsv_test %>%
							dplyr::mutate(id=as.character(id)) %>%
							dplyr::filter(year==year) %>%
							dplyr::select(-year),
						coord,
						by=c("id"="label"))
		}else{
			hsv <- dplyr::full_join(
						hsv_test %>%
							dplyr::mutate(id=as.character(id)) %>%
							dplyr::filter(year==year) %>%
							dplyr::select(-year),
						coord,
						by=c("id"="label"))
		}
	
		col_code <- function(x, legend=FALSE){
			cols <- viridis::viridis(3, option=palette)
			data_prep <- function(x){
				# col <- ifelse(unique(hsv_test$cat[hsv_test$id==x])=="F", "dodgerblue3",
				# 			ifelse(unique(hsv_test$cat[hsv_test$id==x])=="M", "red", "green"))
				col <- ifelse(unique(hsv_test$cat[hsv_test$id==x])=="F", cols[1],
							ifelse(unique(hsv_test$cat[hsv_test$id==x])=="M", cols[2], cols[3]))
				temp <- hsv_test[hsv_test$id==x,] %>%
							dplyr::mutate(sample=factor(sample,
									levels = rev(c(2, 4, 3, 1))),
								val=0.25,
								col=ifelse(genome==1, col, "white")) %>%
							dplyr::select(-dna) %>%
							dplyr::select(-cat) %>%
							dplyr::select(-year)
				if(unique(hsv_test$cat[hsv_test$id==x])=="M"){
					fill <- data.frame(
								sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
								genome=0,
								id=x,
								val=0.25,
								col="grey") %>%
							dplyr::filter(sample != "4")
					miss <- !(fill$sample %in% temp$sample)
					temp <- rbind(temp, fill[miss,])
				}else{
					if(unique(hsv_test$cat[hsv_test$id==x])=="young"){
						fill <- data.frame(
									sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
									genome=0,
									id=x,
									val=0.25,
									col="grey") %>%
								dplyr::filter(sample == "1")
						miss <- !(fill$sample %in% temp$sample)
						temp <- rbind(temp, fill[miss,])
					}else{
						fill <- data.frame(
									sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
									genome=0,
									id=x,
									val=0.25,
									col="grey")
						miss <- !(fill$sample %in% temp$sample)
						temp <- rbind(temp, fill[miss,])
					}
				}
		
				cols <- temp$col[order(as.integer(temp$sample))]
				return(list(temp, cols))
			}
			if(!legend){
				temp_d <- data_prep(x)
				temp <- temp_d[[1]]
				cols <- temp_d[[2]]
	
				pie <- ggplot2::ggplot(temp) +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col="black", size=0.75) +
							ggplot2::coord_polar("y", start=0) +
							ggplot2::scale_fill_manual(values=cols) +
							ggplot2::theme_void() +
							ggplot2::theme(legend.position = "none")
				if(unique(hsv_test$cat[hsv_test$id==x])=="young"){
					pie <- pie +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col=NA)
				}else{
					pie <- pie +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col="black", size=0.5)
				}
				return(pie)
			}else{
				legend_1 <- data_prep("118")
				legend_2 <- data_prep("21")
				legend_3 <- data_prep("170")
	
				legend_t <- rbind(
								legend_1[[1]] %>%
									dplyr::mutate(
										title="F",
										col="black"),
								legend_2[[1]] %>%
									dplyr::mutate(
										title="M",
										col="black"),
								legend_3[[1]] %>%
									dplyr::mutate(
										title="Child",
										col=NA)) %>%
							dplyr::mutate(
								fill=c(legend_1[[2]], legend_2[[2]], legend_3[[2]]),
								order=factor(c(2, 3, 4, 1, 7, 8, 6, 9), levels=c(2, 3, 4, 1, 7, 8, 6, 9))) %>%
							dplyr::mutate(samp_pos=sample)
	
				# Tweaking the legend for M
				legend_t$sample <- as.integer(as.character(legend_t$sample))
				legend_t$sample[legend_t$title=="M"] <- c(1,3,2)
				legend_t <- legend_t %>%
							dplyr::mutate(sample=factor(sample, levels = rev(c(2, 4, 3, 1)))) %>%
							dplyr::group_by(title) %>%
							dplyr::arrange(title, as.character(sample)) %>%
							dplyr::mutate(val=1/dplyr::n()) %>%
							dplyr::mutate(pos=val/2 + c(0, cumsum(val)[-length(val)])) %>%
							dplyr::ungroup() %>%
							data.frame()
				legend_t$samp_pos <- as.character(legend_t$samp_pos)
				legend_t$samp_pos[legend_t$title=="M"] <- c("2", "3", "1")
				legend_t$samp_pos[legend_t$title=="F"] <- c("2", "4", "3", "1")
	
				legend_t$title <- factor(
									legend_t$title,
									levels = c("F", "M", "Child"))
	
				pie_l <- ggplot2::ggplot(legend_t) +
							ggplot2::coord_polar("y") +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=order),
								stat="identity", col="black", size=0.75) +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=order, color=I(col)),
								stat="identity", size=0.5) +
							ggplot2::scale_fill_manual(values=legend_t$fill[order(legend_t$order)]) +
							ggplot2::geom_text(aes(x=0, y = pos, 
								label = samp_pos), size=4) +
							ggplot2::facet_wrap(.~title, ncol=1) +
							ggplot2::theme_void() +
							ggplot2::theme(
								legend.position = "none",
								strip.text.x = element_text(size = 16, face = "bold", hjust=0.5))
	
				return(pie_l)
			}
		}
	
		links_net <- setNames(data.frame(links_df), c("from", "to", "link", "part", "part_col")) %>%
						dplyr::left_join(., coord, by=c(c("to"="label"))) %>%
						dplyr::rename(xmax=x) %>%
						dplyr::rename(ymax=y) %>%
						dplyr::left_join(., coord, by=c(c("from"="label")))
	
		if(participant_only){
			subplots <- lapply(coord$label, col_code)
		}else{
			subplots <- lapply(coord$label[coord$label %in% hsv$id[!is.na(hsv$sample)]], col_code)
		}
		
		frame <- ggplot2::ggplot() +
					ggplot2::geom_point(data=coord,
						ggplot2::aes(x, y),
						color=NA) +
					# ggplot2::geom_segment(data=links_net[links_net$part=="solid",],
					# 	ggplot2::aes(x=x, xend=xmax, y=y, yend=ymax, linetype=I(part)),
					# 	color="darkgrey", size=0.8) +
					ggplot2::geom_segment(data=links_net[links_net$part=="solid",],
						ggplot2::aes(x=x*1.5, xend=xmax*1.5, y=y*1.2, yend=ymax*1.2, color=I(part_col)),
						linetype="solid", size=0.8) +
					# ggplot2::geom_segment(data=links_net[links_net$part=="dotted",],
					# 	ggplot2::aes(x=x, xend=xmax, y=y, yend=ymax, linetype=I(part)),
					# 	color="darkgrey", size=0.8, lty="11") +
					ggplot2::geom_segment(data=links_net[links_net$part=="dotted",],
						ggplot2::aes(x=x*1.5, xend=xmax*1.5, y=y*1.2, yend=ymax*1.2, color=I(part_col)),
						linetype="solid", size=0.8) +
					ggplot2::theme_void()
	
		if(participant_only){
			for(i in 1:length(subplots)){
				frame <- frame +
							ggplot2::annotation_custom(
								ggplot2::ggplotGrob(subplots[[i]]), 
									x = (coord$x[i]-0.015)*1.5, y = (coord$y[i]-0.015)*1.2, 
									xmax = (coord$x[i]+0.015)*1.5, ymax = (coord$y[i]+0.015)*1.2)
							# 		 +
							# ggplot2::geom_text(data=coord,
							# 	ggplot2::aes(x=x, y=y-0.015, label=label),
							# 	size=2.5, fontface = "bold")
			}
		}else{
			for(i in 1:length(subplots)){
				frame <- frame +
							ggplot2::annotation_custom(
								ggplot2::ggplotGrob(subplots[[i]]), 
									x = (coord$x[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]-0.015)*1.5,
									y = (coord$y[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]-0.015)*1.2, 
									xmax = (coord$x[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]+0.015)*1.5,
									ymax = (coord$y[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]+0.015)*1.2)
							# 		 +
							# ggplot2::geom_text(data=coord[coord$label %in% hsv$id[!is.na(hsv$sample)],],
							# 	ggplot2::aes(x=x, y=y-0.02, label=label),
							# 	size=2.5, fontface = "bold")
			}
			frame <- frame +
						ggplot2::geom_point(data=coord[!(coord$label %in% hsv$id[!is.na(hsv$sample)]),],
							ggplot2::aes(x=x*1.5, y=y*1.2), shape=21, color="black", fill="grey80", size=3)
		}
		if(legend){
			frame <- frame +
						ggplot2::annotation_custom(
							ggplot2::ggplotGrob(degree_dist), 
								x = -0.045,
								y = -0.05, 
								xmax = 0.2,
								ymax = 0.24) +
						ggplot2::annotation_custom(
							ggplot2::ggplotGrob(col_code(legend=TRUE)), 
								x = -0.08,
								y = 0.9, 
								xmax = 0.12,
								ymax = 1.2)
	
		}else{
			frame <- frame +
						ggplot2::annotation_custom(
							ggplot2::ggplotGrob(degree_dist), 
								x = -0.045,
								y = -0.05, 
								xmax = 0.2,
								ymax = 0.24)
		}
		return(frame)
	}
	# fig <- multipanelfigure::multi_panel_figure(rows=1, width=c(12.5, 12.5), height=10, unit="in") %>%
	# 		multipanelfigure::fill_panel(graph_net(year=2015, legend=TRUE, participant_only=participant_only), label="a") %>%
	# 		multipanelfigure::fill_panel(graph_net(year=2016, legend=FALSE, participant_only=participant_only), label="b") %>%
	# 		multipanelfigure::save_multi_panel_figure(file, dpi=300)

	fig <- multipanelfigure::multi_panel_figure(rows=2, width=17.5, height=20, columns=1, unit="in") %>%
			multipanelfigure::fill_panel(graph_net(year=2015, legend=TRUE, participant_only=participant_only), label="A") %>%
			multipanelfigure::fill_panel(graph_net(year=2016, legend=FALSE, participant_only=participant_only), label="B") %>%
			multipanelfigure::save_multi_panel_figure(file, dpi=300)
	note("Figure saved...\n")
}

#' Creating a figure on the hsv tests with the network
#'
#' @param year Year of the data used.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

hsv_net_ind <- function(palette="C", legend_pos=c(0.75, 0.85)){
	graph_net <- function(year=2015, palette_v=palette, show_legend=FALSE, legend_position=legend_pos){
		if(!show_legend){
			if(year==2016){
				net <- net2016
			}else{
				net <- net2015
			}
			data <- net_d_manage(net, verbose=FALSE)
			nodes <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
		
			if(year==2016){
				edges <- edge2016
				nodes <- nodes[!(nodes$id==216 & nodes$phone_own_duration==24),] %>%
							dplyr::filter(!(id==214 & adult_house==4)) %>%
							dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
									ifelse(phone_own>0, "Phone", "No phone")),
								phone_compound=ifelse(is.na(other_phone_own), "No data",
									ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
							dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
									ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
									phone_c2=ifelse(phone=="Phone", "Phone owned",
									ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
										ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
							dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
									phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
										ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
							dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
							dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
							dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
							dplyr::mutate(sex=ifelse(is.na(sex), NA,
								ifelse(sex==2, "F", "M")),
								hsv=ifelse(id %in% hsv_test$id[hsv_test$year==2016 & hsv_test$genome==1], "Positive", "Negative"))
		
				links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
				links$min <- apply(links[,1:2],1,min)
				links$max <- apply(links[,1:2],1,max)
				links <- links[links$min!=links$max,]
				links$comb <- paste(links$min, links$max,sep="-")
				links <- links[!duplicated(links$comb),c("x","y")]
				links_l <- links
				links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
									"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
									(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
										"Phone", "No Phone")
				nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
										nodes$id %in% nodes$id[nodes$phone=="Phone"],
										"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
									(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
										"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
									(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
										"Phone", "No Phone")
				nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
										nodes$id %in% nodes$id[nodes$phone=="Phone"],
										"Phone", "No Phone")
			}else{
				edges <- edge2015
				nodes <- nodes %>%
							dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
									ifelse(phone_own>0, "Phone", "No phone")),
								phone_compound=ifelse(is.na(other_phone_own), "No data",
									ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
							dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
									ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
									phone_c2=ifelse(phone=="Phone", "Phone owned",
									ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
										ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
							dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
							dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))
		
				links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
				links$min <- apply(links[,1:2],1,min)
				links$max <- apply(links[,1:2],1,max)
				links <- links[links$min!=links$max,]
				links$comb <- paste(links$min, links$max,sep="-")
				links <- links[!duplicated(links$comb),c("x","y")]
				l_list <- unique(c(links$x, links$y))
				temp <- net[[2]][, c("id", "age", "sex")] %>%
							dplyr::mutate(id=as.integer(id))
				l_list <- data.frame(id=as.integer(l_list[!(l_list %in% as.character(nodes$id))])) %>% # List of ID in the file for children but not in the other ones
							dplyr::left_join(., temp, by="id")
				nodes <- nodes %>%
								dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
								dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
				nodes[match(l_list$id, nodes$id), c("age", "sex")] <- l_list[na.omit(match(nodes$id, l_list$id)), c("age", "sex")]
				nodes <- nodes %>%
							dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
							dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
							dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
								phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
										ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
							# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
							dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
							dplyr::mutate(sex=ifelse(is.na(sex), NA,
								ifelse(sex==2, "F", "M")),
								hsv=ifelse(id %in% hsv_test$id[hsv_test$year==2015 & hsv_test$genome==1], "Positive", "Negative"))
		
				links_l <- links
				links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
									"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
									(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
										"Phone", "No Phone")
				nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
										nodes$id %in% nodes$id[nodes$phone=="Phone"],
										"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
									(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
										"Phone", "No Phone")
				links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
									(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
										"Phone", "No Phone")
				nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
										nodes$id %in% nodes$id[nodes$phone=="Phone"],
										"Phone", "No Phone")
			}
		
			edges <- setNames(edges, c("x", "y"))
			edges$link <- unlist(lapply(1:nrow(edges), function(x){
				paste(sort(t(edges)[,x]), collapse="-")
			}))
			links$link <- unlist(lapply(1:nrow(links), function(x){
				paste(sort(t(links)[,x]), collapse="-")
			}))
			links_df <- rbind(edges, links) %>%
							dplyr::group_by(link) %>%
							dplyr::mutate(n=dplyr::row_number()) %>%
							dplyr::ungroup() %>%
							dplyr::filter(n==1) %>%
							dplyr::select(x, y, link)
			links_df$x <- ifelse(substr(links_df$x, 1, 1)=="0", substr(links_df$x, 2, nchar(links_df$x)), links_df$x)
			links_df$y <- ifelse(substr(links_df$y, 1, 1)=="0", substr(links_df$y, 2, nchar(links_df$y)), links_df$y)
	
			added_id <- unique(c(edges$x, edges$y, as.character(nodes$id)))
		
			net <- igraph::graph_from_data_frame(
						d=unique(links_df[,c("x", "y")]),
						vertices=data.frame(id=added_id),
						directed=FALSE)
			gnet <- intergraph::asNetwork(igraph::simplify(net))
		
			d_dist <- data.frame(
						id=added_id,
						d=igraph::degree(net)) %>%
						dplyr::left_join(
							.,
							nodes %>%
								dplyr::mutate(
									id=as.character(id)) %>%
								dplyr::select(id, hsv),
							by="id") %>%
						# dplyr::mutate(hsv=ifelse(is.na(hsv), "Not tested", hsv)) %>%
						# dplyr::mutate(hsv=factor(hsv,
						# 				levels=c("Not tested", "Negative", "Positive"),
						# 				labels=c("Not tested", "No HSV shedding", "HSV shedding")))
						dplyr::mutate(hsv=ifelse(is.na(hsv), "Sample not collected", hsv)) %>%
						dplyr::mutate(hsv=factor(hsv,
										levels=c("Sample not collected", "Negative", "Positive"),
										labels=c("Sample not collected", "HSV not detected", "HSV detected")))
	
			# color_code <- c("lightgrey", viridis::viridis(2, option="F"))
			color_code <- viridis::viridis(3, option="G")
			degree_dist <- ggplot2::ggplot(
							d_dist,
							ggplot2::aes(x=d, fill=hsv)) +
							ggplot2::geom_bar(stat="count") +
							# ggplot2::scale_fill_manual(
							# 	name="",
							# 	values=color_code) +
							ggplot2::scale_fill_manual(
								name="",
								values=c("grey", "black", "steelblue")) +
							ggplot2::xlab("Number of edges") +
							ggplot2::ylab("Count") +
							ggplot2::guides(
								fill = ggplot2::guide_legend(override.aes = list(size = 4))) +
							ggplot2::theme_bw() +
							ggplot2::xlim(c(-0.5, 12.5)) +
							ggplot2::ggtitle(paste0(nrow(d_dist), " individuals in the network in ", year, " (", nrow(d_dist[d_dist$hsv!="Sample not collected",]), " participants)")) +
							ggplot2::theme(
								legend.position = legend_position,
								legend.box.background = element_rect(colour = "black"),
								legend.title = ggplot2::element_text(size = 6),
								legend.text  = ggplot2::element_text(size = 9),
								legend.key.size = ggplot2::unit(0.5, "lines"),
								axis.title=ggplot2::element_text(size=8, face="bold"),
								plot.title = element_text(size=9))
		
			links_df$part <- ifelse(links_df$link %in% edges$link, "solid", "dotted")
			links_df$part_col <- ifelse(links_df$link %in% edges$link, "grey85", "grey40")
		
			set.seed(20)
			fig <- GGally::ggnet2(gnet, mode = "fruchtermanreingold",
						alpha=0.8, size=3)
		
			coord <- fig$data[,c("label", "x", "y")]
		
			hsv <- dplyr::full_join(
						hsv_test %>%
							dplyr::mutate(id=as.character(id)) %>%
							dplyr::filter(year==year) %>%
							dplyr::select(-year),
						coord,
						by=c("id"="label"))
		}
		col_code <- function(x, legend=FALSE){
			cols <- viridis::viridis(3, option=palette)
			data_prep <- function(x){
				col <- ifelse(unique(hsv_test$cat[hsv_test$id==x])=="F", cols[2],
							ifelse(unique(hsv_test$cat[hsv_test$id==x])=="M", cols[1], cols[3]))
				temp <- hsv_test[hsv_test$id==x,] %>%
							dplyr::mutate(
								sample=factor(sample,
									levels = rev(c(2, 4, 3, 1))),
								val=0.25,
								col=ifelse(genome==1, col, "white")) %>%
							dplyr::select(-dna) %>%
							dplyr::select(-cat) %>%
							dplyr::select(-year)
				if(unique(hsv_test$cat[hsv_test$id==x])=="M"){
					fill <- data.frame(
								sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
								genome=0,
								id=x,
								val=0.25,
								col="grey") %>%
							dplyr::filter(sample != "4")
					miss <- !(fill$sample %in% temp$sample)
					temp <- rbind(temp, fill[miss,])
				}else{
					if(unique(hsv_test$cat[hsv_test$id==x])=="young"){
						fill <- data.frame(
									sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
									genome=0,
									id=x,
									val=0.25,
									col="grey") %>%
								dplyr::filter(sample == "1")
						miss <- !(fill$sample %in% temp$sample)
						temp <- rbind(temp, fill[miss,])
					}else{
						fill <- data.frame(
									sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
									genome=0,
									id=x,
									val=0.25,
									col="grey")
						miss <- !(fill$sample %in% temp$sample)
						temp <- rbind(temp, fill[miss,])
					}
				}
		
				cols <- temp$col[order(as.integer(temp$sample))]
				return(list(temp, cols))
			}
			if(!legend){
				temp_d <- data_prep(x)
				temp <- temp_d[[1]]
				cols <- temp_d[[2]]
	
				pie <- ggplot2::ggplot(temp) +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col="black", size=0.75) +
							ggplot2::coord_polar("y", start=0) +
							ggplot2::scale_fill_manual(values=cols) +
							ggplot2::theme_void() +
							ggplot2::theme(legend.position = "none")
				if(unique(hsv_test$cat[hsv_test$id==x])=="young"){
					pie <- pie +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col=NA)
				}else{
					pie <- pie +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col="black", size=0.5)
				}
				return(pie)
			}else{
				legend_1 <- data_prep("118")
				legend_2 <- data_prep("21")
				legend_3 <- data_prep("170")
	
				legend_t <- rbind(
								legend_1[[1]] %>%
									dplyr::mutate(
										title="F",
										col="black"),
								legend_2[[1]] %>%
									dplyr::mutate(
										title="M",
										col="black"),
								legend_3[[1]] %>%
									dplyr::mutate(
										title="Child",
										col=NA),
									data.frame(
										sample=1,
										genome=0,
										id=NA,
										val=0.25,
										col=NA,
										title="Named contact")) %>%
							dplyr::mutate(
								fill=c(legend_1[[2]], legend_2[[2]], legend_3[[2]], "grey80"),
								order=factor(c(2, 3, 4, 1, 7, 8, 6, 9, 10), levels=c(2, 3, 4, 1, 7, 8, 6, 9, 10))) %>%
							dplyr::mutate(samp_pos=sample)
	
				# Tweaking the legend for M
				legend_t$sample <- as.integer(as.character(legend_t$sample))
				legend_t$sample[legend_t$title=="M"] <- c(1,3,2)
				legend_t <- legend_t %>%
							dplyr::mutate(sample=factor(sample, levels = rev(c(2, 4, 3, 1)))) %>%
							dplyr::group_by(title) %>%
							dplyr::arrange(title, as.character(sample)) %>%
							dplyr::mutate(val=1/dplyr::n()) %>%
							dplyr::mutate(pos=val/2 + c(0, cumsum(val)[-length(val)])) %>%
							dplyr::ungroup() %>%
							data.frame()
				legend_t$samp_pos <- as.character(legend_t$samp_pos)
				legend_t$samp_pos[legend_t$title=="M"] <- c("2", "3", "1")
				legend_t$samp_pos[legend_t$title=="F"] <- c("2", "4", "3", "1")
				legend_t$samp_pos[legend_t$title=="Named contact"] <- ""
	
				legend_t$title <- factor(
									legend_t$title,
									levels = c("F", "M", "Child", "Named contact"))
	
				pie_l_1 <- ggplot2::ggplot(
							legend_t %>%
								dplyr::filter(title!="Named contact")) +
							ggplot2::coord_polar("y") +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=order),
								stat="identity", col="black", size=0.75) +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=order, color=I(col)),
								stat="identity", size=0.5) +
							ggplot2::scale_fill_manual(values=legend_t$fill[order(legend_t$order)]) +
							ggplot2::geom_text(aes(x=0, y = pos, 
								label = samp_pos), size=4) +
							ggplot2::facet_wrap(.~title, ncol=1) +
							ggplot2::theme_void() +
							ggplot2::theme(
								legend.position = "none",
								strip.text.x = element_text(size = 18, face = "bold", hjust=0.5),
								plot.margin=ggplot2::margin(0, 10, 0, 0, unit="cm"))

				pie_l_2 <- ggplot2::ggplot(
							legend_t %>%
								dplyr::filter(title=="Named contact")) +
							ggplot2::coord_polar("y") +
							ggplot2::geom_bar(ggplot2::aes(x=0.5, y=val, fill=order),
								stat="identity", col="black", size=0.75) +
							ggplot2::geom_bar(ggplot2::aes(x=0.5, y=val, fill=order, color=I(col)),
								stat="identity", size=0.5) +
							ggplot2::scale_fill_manual(values="grey80") +
							# ggplot2::geom_text(aes(x=0, y = pos, 
							# 	label = samp_pos), size=4) +
							ggplot2::facet_wrap(.~title, ncol=1) +
							# ggplot2::scale_x_discrete(limits=c(0, 2)) +
							ggplot2::theme_void() +
							ggplot2::theme(
								legend.position = "none",
								strip.text.x = element_text(size = 18, face = "bold", hjust=0.5),
								plot.margin=ggplot2::margin(0, 10, 0, 0, unit="cm"))

				plot_build <- ggplot2::ggplot_build(pie_l_2)
				# plot_build[["layout"]][["panel_params"]][[1]][["r.range"]]
				plot_build[["layout"]][["panel_params"]][[1]][["r.range"]][2] <- 1.45
				pie_l_2 <- ggplotify::as.ggplot(ggplot2::ggplot_gtable(plot_build))

				# legend_df <- data.frame(
				# 				y=c(1.5, 5.5, 9.5),
				# 				label=c("Not tested", "No HSV", "HSV")) %>%
				# 				dplyr::mutate(x=2)
				# legend_rect <- data.frame(
				# 				ymin=c(0, 4, 8:10),
				# 				ymax=c(3, 7, 9:11),
				# 				fill=c("grey80", "white", rev(c(legend_1[[1]]$col[1], legend_2[[1]]$col[2], legend_3[[1]]$col[1])))) %>%
				# 				dplyr::mutate(
				# 					xmin=rep(0, 5),
				# 					xmax=rep(1.5, 5))

				# col_legend <- ggplot2::ggplot() +
				# 				ggplot2::geom_rect(
				# 					data=legend_rect,
				# 					ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=I(fill)),
				# 					color="black") +
				# 				ggplot2::geom_text(
				# 					data=legend_df,
				# 					ggplot2::aes(x=x, y=y, label=label),
				# 					hjust=0,
				# 					size=5) +
				# 				ggplot2::xlim(0, 5) +
				# 				ggplot2::theme_void()

####################
				legend_circle <- sampSurf::spCircle(
									radius=1,
									centerPoint=c(x=0, y=1.2))[[1]]
				# legend_circle_small <- sampSurf::spCircle(
				# 						radius=0.7,
				# 						centerPoint=c(x=0, y=1.2))[[1]]

				poly1 <- sp::SpatialPolygons(
							list(sp::Polygons(
								list(sp::Polygon(
									matrix(
										c(-1.2, (0.2+2/3), 1.2, (0.2+2/3),
											1.2, 0, -1.2, 0,
											-1.2, (0.2+2/3)),
										ncol=2, byrow=TRUE))), ID="a")))

				poly2 <- sp::SpatialPolygons(
							list(sp::Polygons(
								list(sp::Polygon(
									matrix(
										c(-1.2, (0.2+4/3), 1.2, (0.2+4/3),
											1.2, 2.6, -1.2, 2.6,
											-1.2, (0.2+4/3)),
										ncol=2, byrow=TRUE))), ID="a")))

				legend_circle_s <- raster::bind(
									rgeos::gIntersection(legend_circle, poly2),
									rgeos::gDifference(legend_circle, raster::bind(poly1, poly2)),
									rgeos::gIntersection(legend_circle, poly1)) %>%
									as(., "SpatialPolygonsDataFrame")
				legend_circle_s@data$id <- 1:3
				legend_circle_s <- ggplot2::fortify(legend_circle_s, region="id") %>%
									dplyr::mutate(fill=ifelse(id=="1", "#0D0887FF",
										ifelse(id=="2", "#CC4678FF", "#F0F921FF")))
				legend_circle <- ggplot2::fortify(legend_circle)
				# legend_circle_small <- ggplot2::fortify(legend_circle_small)

				pie <- cowplot::ggdraw(
						gridExtra::grid.arrange(
						pie_l_1,
						pie_l_2,
						ncol=1,
						heights=c(2, 0.7))) +
						cowplot::draw_grob(
							ggplot2::ggplotGrob(
								ggplot2::ggplot() +
									ggplot2::geom_polygon(
										data=legend_circle_s,
										ggplot2::aes(x=long, y=lat, group=group, fill=I(fill)),
										color="black") +
									ggplot2::geom_text(
										ggplot2::aes(x=1.5, y=1.2),
										label="HSV detected",
										hjust=0,
										size=6) +
									ggplot2::xlim(-1.2, 8) +
									ggplot2::theme_void()), 
								0.45,
								0.6,
								0.65, 
								0.15) +
						cowplot::draw_grob(
							ggplot2::ggplotGrob(
								ggplot2::ggplot() +
									ggplot2::geom_polygon(
										data=legend_circle,
										ggplot2::aes(x=long, y=lat, group=group),
										fill="white",
										color="black") +
									ggplot2::geom_text(
										ggplot2::aes(x=1.5, y=1.2),
										label="HSV not detected",
										hjust=0,
										size=6) +
									ggplot2::xlim(-1.2, 8) +
									ggplot2::theme_void()), 
								0.45,
								0.4,
								0.65, 
								0.15)  +
						cowplot::draw_grob(
							ggplot2::ggplotGrob(
								ggplot2::ggplot() +
									ggplot2::geom_polygon(
										data=legend_circle,
										ggplot2::aes(x=long, y=lat, group=group),
										fill="grey",
										color="black") +
									ggplot2::geom_text(
										ggplot2::aes(x=1.5, y=1.2),
										label="Sample not collected",
										hjust=0,
										size=6) +
									ggplot2::xlim(-1.2, 8) +
									ggplot2::theme_void()), 
								0.45,
								0.2,
								0.65, 
								0.15)
##################

				# ggplot2::ggsave(
				# 	file.path(path.workspace, "hsv_net_legend.tiff"),
				# 	gridExtra::grid.arrange(
				# 		pie_l_1,
				# 		pie_l_2,
				# 		col_legend,
				# 		heights=c(1, 2, 1),
				# 		widths=c(1.3, 1),
				# 		layout_matrix =
				# 			rbind(c(1, NA),
				# 				c(1, 3),
				# 				c(2, NA))
				# 	)
				# )
				return(pie)
				# )
			}
		}

		if(show_legend){
			col_code(legend=TRUE)
			# note("Figure saved...\n")
		}else{
			links_net <- setNames(data.frame(links_df), c("from", "to", "link", "part", "part_col")) %>%
							dplyr::left_join(., coord, by=c(c("to"="label"))) %>%
							dplyr::rename(xmax=x) %>%
							dplyr::rename(ymax=y) %>%
							dplyr::left_join(., coord, by=c(c("from"="label")))
		
			subplots <- lapply(coord$label[coord$label %in% hsv$id[!is.na(hsv$sample)]], col_code, legend=FALSE)
			
			frame <- ggplot2::ggplot() +
						ggplot2::geom_point(
							data=coord,
							ggplot2::aes(x, y),
							color=NA) +
						# ggplot2::geom_segment(data=links_net[links_net$part=="solid",],
						# 	ggplot2::aes(x=x*1.25, xend=xmax*1.25, y=y*1.25, yend=ymax*1.25),
						# 	color="grey40",
						# 	linetype="solid", size=0.8) +
						# ggplot2::geom_segment(data=links_net[links_net$part=="dotted",],
						# 	ggplot2::aes(x=x*1.25, xend=xmax*1.25, y=y*1.25, yend=ymax*1.25),
						# 	color="grey40",
						# 	linetype="solid", size=0.8) +
						ggplot2::geom_segment(data=links_net,
							ggplot2::aes(x=x*1.25, xend=xmax*1.25, y=y*1.25, yend=ymax*1.25),
							color="grey80",
							linetype="solid", size=0.8) +
						ggplot2::theme_void()
		
			for(i in 1:length(subplots)){
				frame <- frame +
							ggplot2::annotation_custom(
								ggplot2::ggplotGrob(subplots[[i]]), 
									x = (coord$x[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]-0.016)*1.25,
									y = (coord$y[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]-0.016)*1.25, 
									xmax = (coord$x[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]+0.016)*1.25,
									ymax = (coord$y[coord$label %in% hsv$id[!is.na(hsv$sample)]][i]+0.016)*1.25)
			}
			frame <- frame +
						ggplot2::geom_point(data=coord[!(coord$label %in% hsv$id[!is.na(hsv$sample)]),],
							ggplot2::aes(x=x*1.25, y=y*1.25), shape=21, color="black", fill="grey80", size=3)

			# ggplot2::ggsave(
			# 	file.path(path.workspace, paste0("hsv_net_degree_", year, ".tiff")),
			# 	degree_dist,
			# 	height=4,
			# 	width=4,
			# 	unit="in")
			# ggplot2::ggsave(
			# 	file.path(path.workspace, paste0("hsv_net_", year, ".tiff")),
			# 	frame,
			# 	height=15,
			# 	width=15,
			# 	unit="in")
			# note("Figure saved...\n")
			return(list(frame, degree_dist))
		}
	}

	ggplot2::ggsave(
		file.path(path.workspace, "hsv_net_legend.tiff"),
		graph_net(show_legend=TRUE),
		width=9,
		height=9,
		unit="in")

	# fig_leg <- multipanelfigure::multi_panel_figure(rows=1, width=8, height=8, columns=1, unit="in") %>%
	# 			multipanelfigure::fill_panel(graph_net(show_legend=TRUE), label="") %>%
	# 			multipanelfigure::save_multi_panel_figure(file.path(path.workspace, "hsv_net_legend.tiff"), dpi=300)

	fig_deg1 <- multipanelfigure::multi_panel_figure(rows=1, width=4, height=4, columns=1, unit="in") %>%
				multipanelfigure::fill_panel(graph_net(year=2015, palette_v=palette, show_legend=FALSE, legend_position=legend_pos)[[2]], label="") %>%
				multipanelfigure::save_multi_panel_figure(file.path(path.workspace, "hsv_net_degree_2015.tiff"), dpi=300)

	fig_deg2 <- multipanelfigure::multi_panel_figure(rows=1, width=4, height=4, columns=1, unit="in") %>%
				multipanelfigure::fill_panel(graph_net(year=2016, palette_v=palette, show_legend=FALSE, legend_position=legend_pos)[[2]], label="") %>%
				multipanelfigure::save_multi_panel_figure(file.path(path.workspace, "hsv_net_degree_2016.tiff"), dpi=300)

	fig1 <- multipanelfigure::multi_panel_figure(
				rows=1, width=8, height=8, columns=1, unit="in",
				font.label = list(size = 30)) %>%
				multipanelfigure::fill_panel(graph_net(year=2015, palette_v=palette, show_legend=FALSE, legend_position=legend_pos)[[1]], label="A") %>%
				multipanelfigure::save_multi_panel_figure(file.path(path.workspace, "hsv_net_2015.tiff"), dpi=300)

	fig2 <- multipanelfigure::multi_panel_figure(
				rows=1, width=8, height=8, columns=1, unit="in",
				font.label = list(size = 30)) %>%
				multipanelfigure::fill_panel(graph_net(year=2016, palette_v=palette, show_legend=FALSE, legend_position=legend_pos)[[1]], label="B") %>%
				multipanelfigure::save_multi_panel_figure(file.path(path.workspace, "hsv_net_2016.tiff"), dpi=300)

	note("Figure saved...\n")
	# graph_net(show_legend=TRUE)
	# graph_net(year=2015, palette_v=palette, show_legend=FALSE, legend_position=legend_pos)
	# graph_net(year=2016, palette_v=palette, show_legend=FALSE, legend_position=legend_pos)
}

#' Creating a figure on the hsv tests with the network
#'
#' @param year Year of the data used.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

hsv_appendix <- function(palette="C", legend_pos=c(0.75, 0.15), num_col=10){
	graph_net <- function(year_s=2015, palette_v=palette, show_legend=FALSE, legend_position=legend_pos){
		hsv_test$cat[hsv_test$id %in% c(99, 111)] <- "F"

		num_row_F_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="F"])) %/% num_col
		rem_F_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="F"])) %% num_col
		num_row_M_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="M"])) %/% num_col
		rem_M_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="M"])) %% num_col
		num_row_young_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="young"])) %/% num_col
		rem_young_2015 <- length(unique(hsv_test$id[hsv_test$year==2015 & hsv_test$cat=="young"])) %% num_col

		num_row_F_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="F"])) %/% num_col
		rem_F_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="F"])) %% num_col
		num_row_M_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="M"])) %/% num_col
		rem_M_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="M"])) %% num_col
		num_row_young_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="young"])) %/% num_col
		rem_young_2016 <- length(unique(hsv_test$id[hsv_test$year==2016 & hsv_test$cat=="young"])) %% num_col

		num_row_2015 <- num_row_F_2015+ifelse(rem_F_2015>0, 1, 0)+num_row_M_2015+ifelse(rem_M_2015>0, 1, 0)+num_row_young_2015+ifelse(rem_young_2015>0, 1, 0)
		num_row_2016 <- num_row_F_2016+ifelse(rem_F_2016>0, 1, 0)+num_row_M_2016+ifelse(rem_M_2016>0, 1, 0)+num_row_young_2016+ifelse(rem_young_2016>0, 1, 0)
		row_max <- max(c(num_row_2015, num_row_2016))

		if(year_s==2015){
			rem_F <- rem_F_2015
			rem_M <- rem_M_2015
			rem_young <- rem_young_2015
			num_row_F <- num_row_F_2015
			num_row_M <- num_row_M_2015
			num_row_young <- num_row_young_2015
		}else{
			rem_F <- rem_F_2016
			rem_M <- rem_M_2016
			rem_young <- rem_young_2016
			num_row_F <- num_row_F_2016
			num_row_M <- num_row_M_2016
			num_row_young <- num_row_young_2016
		}
		coord <- rbind(
					hsv_test %>%
						dplyr::filter(year==year_s & cat=="F") %>%
						dplyr::select(id, cat) %>%
						unique() %>%
						dplyr::arrange(id) %>%
						dplyr::mutate(
							x=c(rep(1:num_col, num_row_F), 1:rem_F),
							y=c(rep(1:num_row_F, each=num_col), rep((num_row_F+1), rem_F))),
					hsv_test %>%
						dplyr::filter(year==year_s & cat=="M") %>%
						dplyr::select(id, cat) %>%
						unique() %>%
						dplyr::arrange(id) %>%
						dplyr::mutate(
							x=c(rep(1:num_col, num_row_M), 1:rem_M),
							y=c(rep(1:num_row_M, each=num_col), rep((num_row_M+1), rem_M))),
					hsv_test %>%
						dplyr::filter(year==year_s & cat=="young") %>%
						dplyr::select(id, cat) %>%
						unique() %>%
						dplyr::arrange(id) %>%
						dplyr::mutate(
							x=c(rep(1:num_col, num_row_young), 1:rem_young),
							y=c(rep(1:num_row_young, each=num_col), rep((num_row_young+1), rem_young)))
				)
		coord$y[coord$cat=="M"] <- coord$y[coord$cat=="M"]+max(coord$y[coord$cat=="F"])
		coord$y[coord$cat=="young"] <- coord$y[coord$cat=="young"]+max(coord$y[coord$cat=="M"])
		coord$y <- (row_max+1)-coord$y

		col_code <- function(x, legend=FALSE){
			cols <- viridis::viridis(3, option=palette_v)
			data_prep <- function(x){
				col <- ifelse(unique(hsv_test$cat[hsv_test$id==x])=="F", cols[2],
							ifelse(unique(hsv_test$cat[hsv_test$id==x])=="M", cols[1], cols[3]))
				temp <- hsv_test[hsv_test$id==x,] %>%
							dplyr::mutate(sample=factor(sample,
									levels = rev(c(2, 4, 3, 1))),
								val=0.25,
								col=ifelse(genome==1, col, "white")) %>%
							dplyr::select(-dna) %>%
							dplyr::select(-cat) %>%
							dplyr::select(-year)
				if(unique(hsv_test$cat[hsv_test$id==x])=="M"){
					fill <- data.frame(
								sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
								genome=0,
								id=x,
								val=0.25,
								col="grey") %>%
							dplyr::filter(sample != "4")
					miss <- !(fill$sample %in% temp$sample)
					temp <- rbind(temp, fill[miss,])
				}else{
					if(unique(hsv_test$cat[hsv_test$id==x])=="young"){
						fill <- data.frame(
									sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
									genome=0,
									id=x,
									val=0.25,
									col="grey") %>%
								dplyr::filter(sample == "1")
						miss <- !(fill$sample %in% temp$sample)
						temp <- rbind(temp, fill[miss,])
					}else{
						fill <- data.frame(
									sample=factor(1:4, levels = rev(c(2, 4, 3, 1))),
									genome=0,
									id=x,
									val=0.25,
									col="grey")
						miss <- !(fill$sample %in% temp$sample)
						temp <- rbind(temp, fill[miss,])
					}
				}
		
				cols <- temp$col[order(as.integer(temp$sample))]
				return(list(temp, cols))
			}
			if(!legend){
				temp_d <- data_prep(x)
				temp <- temp_d[[1]]
				cols <- temp_d[[2]]
	
				pie <- ggplot2::ggplot(temp) +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col="black", size=0.75) +
							ggplot2::coord_polar("y", start=0) +
							ggplot2::scale_fill_manual(values=cols) +
							ggplot2::theme_void() +
							ggplot2::theme(legend.position = "none")
				if(unique(hsv_test$cat[hsv_test$id==x])=="young"){
					pie <- pie +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col=NA)
				}else{
					pie <- pie +
							ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=sample),
								stat="identity", col="black", size=0.5)
				}
				return(pie)
			}else{
				legend_1 <- data_prep("118")
				legend_2 <- data_prep("21")
				legend_3 <- data_prep("170")
	
				legend_t <- rbind(
								legend_1[[1]] %>%
									dplyr::mutate(
										title="F",
										col="black"),
								legend_2[[1]] %>%
									dplyr::mutate(
										title="M",
										col="black"),
								legend_3[[1]] %>%
									dplyr::mutate(
										title="Child",
										col=NA)) %>%
							dplyr::mutate(
								fill=c(legend_1[[2]], legend_2[[2]], legend_3[[2]]),
								order=factor(c(2, 3, 4, 1, 7, 8, 6, 9), levels=c(2, 3, 4, 1, 7, 8, 6, 9))) %>%
							dplyr::mutate(samp_pos=sample)
	
				# Tweaking the legend for M
				legend_t$sample <- as.integer(as.character(legend_t$sample))
				legend_t$sample[legend_t$title=="M"] <- c(1,3,2)
				legend_t <- legend_t %>%
							dplyr::mutate(sample=factor(sample, levels = rev(c(2, 4, 3, 1)))) %>%
							dplyr::group_by(title) %>%
							dplyr::arrange(title, as.character(sample)) %>%
							dplyr::mutate(val=1/dplyr::n()) %>%
							dplyr::mutate(pos=val/2 + c(0, cumsum(val)[-length(val)])) %>%
							dplyr::ungroup() %>%
							data.frame()
				legend_t$samp_pos <- as.character(legend_t$samp_pos)
				legend_t$samp_pos[legend_t$title=="M"] <- c("2", "3", "1")
				legend_t$samp_pos[legend_t$title=="F"] <- c("2", "4", "3", "1")
	
				legend_t$title <- factor(
									legend_t$title,
									levels = c("F", "M", "Child"))
	
				pie <- ggplot2::ggplot(legend_t) +
						ggplot2::coord_polar("y") +
						ggplot2::facet_wrap(.~title, ncol=1) +
						ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=I(fill)),
							# fill="white",
							stat="identity",
							color="black",
							size=0.75) +
						ggplot2::geom_bar(ggplot2::aes(x=0, y=val, fill=I(fill), color=I(col)),
							# fill="white",
							stat="identity",
							size=0.5) +
						ggplot2::geom_text(
							ggplot2::aes(x=0, y = pos, label = samp_pos),
							size=8) +
						ggplot2::theme_void() +
						ggplot2::theme(
							legend.position = "none",
							strip.text.x = element_text(size = 22, face = "bold", hjust=0.5))
			
				legend_df <- data.frame(
								y=c(1.5, 5.5, 9.5),
								label=c("Not tested", "No HSV", "HSV")) %>%
								dplyr::mutate(x=2)
				legend_circle <- sampSurf::spCircle(
									radius=1,
									centerPoint=c(x=0, y=1.2))[[1]]

				poly1 <- sp::SpatialPolygons(
							list(sp::Polygons(
								list(sp::Polygon(
									matrix(
										c(-1.2, (0.2+2/3), 1.2, (0.2+2/3),
											1.2, 0, -1.2, 0,
											-1.2, (0.2+2/3)),
										ncol=2, byrow=TRUE))), ID="a")))

				poly2 <- sp::SpatialPolygons(
							list(sp::Polygons(
								list(sp::Polygon(
									matrix(
										c(-1.2, (0.2+4/3), 1.2, (0.2+4/3),
											1.2, 2.6, -1.2, 2.6,
											-1.2, (0.2+4/3)),
										ncol=2, byrow=TRUE))), ID="a")))

				legend_circle_s <- raster::bind(
									rgeos::gIntersection(legend_circle, poly2),
									rgeos::gDifference(legend_circle, raster::bind(poly1, poly2)),
									rgeos::gIntersection(legend_circle, poly1)) %>%
									as(., "SpatialPolygonsDataFrame")
				legend_circle_s@data$id <- 1:3
				legend_circle_s <- ggplot2::fortify(legend_circle_s, region="id") %>%
									dplyr::mutate(fill=ifelse(id=="1", "#0D0887FF",
										ifelse(id=="2", "#CC4678FF", "#F0F921FF")))
				legend_circle <- ggplot2::fortify(legend_circle)

				pie <- cowplot::ggdraw(
							pie +
								ggplot2::theme(
									plot.margin=ggplot2::margin(0, 14, 0, 0, unit="cm"))) +
						cowplot::draw_grob(
							ggplot2::ggplotGrob(
								ggplot2::ggplot() +
									ggplot2::geom_polygon(
										data=legend_circle_s,
										ggplot2::aes(x=long, y=lat, group=group, fill=I(fill)),
										color="black") +
									ggplot2::geom_text(
										ggplot2::aes(x=1.5, y=1.2),
										label="HSV detected",
										hjust=0,
										size=10) +
									ggplot2::xlim(-1.2, 8) +
									ggplot2::theme_void()), 
								0.35,
								0.6,
								0.65, 
								0.15) +
						cowplot::draw_grob(
							ggplot2::ggplotGrob(
								ggplot2::ggplot() +
									ggplot2::geom_polygon(
										data=legend_circle,
										ggplot2::aes(x=long, y=lat, group=group),
										fill="white",
										color="black") +
									ggplot2::geom_text(
										ggplot2::aes(x=1.5, y=1.2),
										label="HSV not detected",
										hjust=0,
										size=10) +
									ggplot2::xlim(-1.2, 8) +
									ggplot2::theme_void()), 
								0.35,
								0.4,
								0.65, 
								0.15)  +
						cowplot::draw_grob(
							ggplot2::ggplotGrob(
								ggplot2::ggplot() +
									ggplot2::geom_polygon(
										data=legend_circle,
										ggplot2::aes(x=long, y=lat, group=group),
										fill="grey",
										color="black") +
									ggplot2::geom_text(
										ggplot2::aes(x=1.5, y=1.2),
										label="Sample not collected",
										hjust=0,
										size=10) +
									ggplot2::xlim(-1.2, 8) +
									ggplot2::theme_void()), 
								0.35,
								0.2,
								0.65, 
								0.15)
				return(pie)
			}
		}

		if(show_legend){
			legend_fig <- col_code(legend=TRUE)
			return(legend_fig)
		}else{
			subplots <- lapply(coord$id, col_code, legend=FALSE)
			
			frame <- ggplot2::ggplot() +
						ggplot2::geom_point(
							data=coord,
							ggplot2::aes(x, y),
							color=NA) +
						ggplot2::ylim(-2, row_max) +
						ggplot2::ggtitle(paste0("Samples collected in ", year_s)) +
						ggplot2::theme_void() +
						ggplot2::theme(
							plot.title = element_text(size=16, hjust=0.5))
		
			for(i in 1:length(subplots)){
				frame <- frame +
							ggplot2::annotation_custom(
								ggplot2::ggplotGrob(subplots[[i]]), 
									x = (coord$x[i]-0.45),
									y = (coord$y[i]-0.45), 
									xmax = (coord$x[i]+0.45),
									ymax = (coord$y[i]+0.45))
			}
			frame <- frame +
						ggplot2::geom_text(
							data=coord,
							ggplot2::aes(
								x=x,
								y=y-0.48,
								label=id),
							size=2,
							fontface="bold")
			return(frame)
		}
	}

	ggplot2::ggsave(
		file.path(path.workspace, "appendix_legend.tiff"),
		graph_net(show_legend=TRUE),
		width=9,
		height=9,
		unit="in")

	fig <- multipanelfigure::multi_panel_figure(rows=1, width=15, height=8, columns=2, unit="in") %>%
			multipanelfigure::fill_panel(graph_net(year=2015), label="A") %>%
			multipanelfigure::fill_panel(graph_net(year=2016), label="B") %>%
			multipanelfigure::save_multi_panel_figure(file.path(path.workspace, "appendix_hsv.tiff"), dpi=300)

	note("Figure saved...\n")
	# gridExtra::grid.arrange(
	# 	graph_net(year_s=2015, show_legend=TRUE),
	# 	graph_net(year_s=2016, show_legend=FALSE),
	# 	ncol=2)
}

#' Creating a figure on the hsv tests with the network
#'
#' @param year Year of the data used.
#' @param phone TRUE/FALSE parameter to choose if colors will be based on phone ownershipe or access to care.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

care_phone_net <- function(year=2015, phone=TRUE, file=file.path(path.workspace, "phone_net.tiff")){
	if(year==2016){
		net <- net2016
	}else{
		net <- net2015
	}
	data <- net_d_manage(net, verbose=FALSE)
	nodes <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration

	if(year==2016){
		edges <- edge2016
		nodes <- nodes[!(nodes$id==216 & nodes$phone_own_duration==24),] %>%
					dplyr::filter(!(id==214 & adult_house==4)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
							phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

		links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
		links$min <- apply(links[,1:2],1,min)
		links$max <- apply(links[,1:2],1,max)
		links <- links[links$min!=links$max,]
		links$comb <- paste(links$min, links$max,sep="-")
		links <- links[!duplicated(links$comb),c("x","y")]
		links_l <- links
		links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
							"Phone", "No Phone")
		links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
							(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
								"Phone", "No Phone")
		nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
								nodes$id %in% nodes$id[nodes$phone=="Phone"],
								"Phone", "No Phone")
		links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
							(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
								"Phone", "No Phone")
		links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
							(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
								"Phone", "No Phone")
		nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
								nodes$id %in% nodes$id[nodes$phone=="Phone"],
								"Phone", "No Phone")
	}else{
		edges <- edge2015
		nodes <- nodes %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

		links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
		links$min <- apply(links[,1:2],1,min)
		links$max <- apply(links[,1:2],1,max)
		links <- links[links$min!=links$max,]
		links$comb <- paste(links$min, links$max,sep="-")
		links <- links[!duplicated(links$comb),c("x","y")]
		l_list <- unique(c(links$x, links$y))
		temp <- net[[2]][, c("id", "age", "sex")] %>%
					dplyr::mutate(id=as.integer(id))
		l_list <- data.frame(id=l_list[!(l_list %in% nodes$id)]) %>% # List of ID in the file for children but not in the other ones
					dplyr::left_join(., temp, by="id")
		nodes <- nodes %>%
						dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
						dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
		nodes[match(l_list$id, nodes$id), c("age", "sex")] <- l_list[na.omit(match(nodes$id, l_list$id)), c("age", "sex")]
		nodes <-  nodes %>%
						dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
						dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
						dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
							phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
									ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
						# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
						dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
						dplyr::mutate(sex=ifelse(is.na(sex), NA,
							ifelse(sex==2, "F", "M")),
							hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

		links_l <- links
		links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
							"Phone", "No Phone")
		links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
							(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
								"Phone", "No Phone")
		nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
								nodes$id %in% nodes$id[nodes$phone=="Phone"],
								"Phone", "No Phone")
		links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
							(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
								"Phone", "No Phone")
		links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
							(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
								"Phone", "No Phone")
		nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
								nodes$id %in% nodes$id[nodes$phone=="Phone"],
								"Phone", "No Phone")
	}

	edges <- setNames(edges, c("x", "y"))
	edges$link <- unlist(lapply(1:nrow(edges), function(x){
		paste(sort(t(edges)[,x]), collapse="-")
	}))
	links$link <- unlist(lapply(1:nrow(links), function(x){
		paste(sort(t(links)[,x]), collapse="-")
	}))
	links_df <- rbind(edges, links) %>%
					dplyr::group_by(link) %>%
					dplyr::mutate(n=dplyr::row_number()) %>%
					dplyr::ungroup() %>%
					dplyr::filter(n==1) %>%
					dplyr::select(x, y, link)
	added_id <- unique(c(edges$x, edges$y, as.character(nodes$id)))

	net <- igraph::graph_from_data_frame(d=links_df[,c("x", "y")],
				vertices=data.frame(id=added_id) %>%
							dplyr::left_join(nodes[,c("id", "phone")] %>%
								dplyr::mutate(id=as.character(id)), by="id"),
				directed=FALSE)
	gnet <- intergraph::asNetwork(net)

	links_df$part <- ifelse(links_df$link %in% edges$link, "solid", "dotted")
	links_df$part_col <- ifelse(links_df$link %in% edges$link, "grey85", "grey40")

	set.seed(20)
	fig <- GGally::ggnet2(gnet, mode = "fruchtermanreingold",
				alpha=0.8, size=3)

	coord <- fig$data[,c("label", "x", "y")] %>%
				dplyr::left_join(nodes[,c("id", "phone", "clinic_time")] %>%
					dplyr::mutate(id=as.character(id)), by=c("label"="id")) %>%
				dplyr::mutate(phone=ifelse(phone=="No data", NA, as.character(phone)))

	links_net <- setNames(data.frame(links_df), c("from", "to", "link", "part", "part_col")) %>%
					dplyr::left_join(., coord[,-c(4, 5)], by=c(c("to"="label"))) %>%
					dplyr::rename(xmax=x) %>%
					dplyr::rename(ymax=y) %>%
					dplyr::left_join(., coord[,-c(4, 5)], by=c(c("from"="label")))
	coord$participant <- ifelse(grepl("sp", coord$label)==TRUE, 0, 1)

	if(phone){
		frame <- ggplot2::ggplot() +
					ggplot2::geom_segment(data=links_net[links_net$part=="solid",],
						ggplot2::aes(x=x*1.5, xend=xmax*1.5, y=y*1.2, yend=ymax*1.2, color=I(part_col)),
						linetype="solid", size=0.8) +
					ggplot2::geom_segment(data=links_net[links_net$part=="dotted",],
						ggplot2::aes(x=x*1.5, xend=xmax*1.5, y=y*1.2, yend=ymax*1.2, color=I(part_col)),
						linetype="solid", size=0.8) +
					ggplot2::geom_point(data=coord[coord$participant==0,],
						ggplot2::aes(x=x*1.5, y=y*1.2),
						shape=21, color="black", fill="grey80", size=3) +
					ggplot2::geom_point(data=coord[coord$participant==1 & is.na(coord$phone),],
						ggplot2::aes(x=x*1.5, y=y*1.2),
						shape=21, color="black", fill="grey80", size=5) +
					ggplot2::geom_point(data=coord[coord$participant==1 & !is.na(coord$phone),],
						ggplot2::aes(x=x*1.5, y=y*1.2, fill=phone),
						shape=21, color="black", size=5) +
					ggplot2::scale_fill_manual("",
						values=c("Phone"="red", "No phone"="blue", "Children"="slategray1")) +
					ggplot2::theme_void()
	}else{
		frame <- ggplot2::ggplot() +
					ggplot2::geom_segment(data=links_net[links_net$part=="solid",],
						ggplot2::aes(x=x*1.5, xend=xmax*1.5, y=y*1.2, yend=ymax*1.2, color=I(part_col)),
						linetype="solid", size=0.8) +
					ggplot2::geom_segment(data=links_net[links_net$part=="dotted",],
						ggplot2::aes(x=x*1.5, xend=xmax*1.5, y=y*1.2, yend=ymax*1.2, color=I(part_col)),
						linetype="solid", size=0.8) +
					ggplot2::geom_point(data=coord[coord$participant==0,],
						ggplot2::aes(x=x*1.5, y=y*1.2),
						shape=21, color="black", fill="grey80", size=3) +
					ggplot2::geom_point(data=coord[coord$participant==1 & is.na(coord$clinic_time),],
						ggplot2::aes(x=x*1.5, y=y*1.2),
						shape=21, color="black", fill="grey80", size=5) +
					ggplot2::geom_point(data=coord[coord$participant==1 & !is.na(coord$clinic_time),],
						ggplot2::aes(x=x*1.5, y=y*1.2, fill=clinic_time),
						shape=21, color="black", size=5) +
					ggplot2::scale_fill_distiller("Time to reach a\nhealth care center",
						palette = "Reds", direction=1) +
					ggplot2::theme_void()
	}

	ggplot2::ggsave(file, frame, width=12.5, height=10)

	note("Figure saved...\n")
	return(frame)	
}

#' Creating a html table on the logistic regression
#'
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A html table.

logistic_table <- function(file=file.path(path.workspace, "logistic.manuscript")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016_ind <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016_ind <- nodes2016_ind[!(nodes2016_ind$id==216 & nodes2016_ind$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"]) | (links_l2016$y %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016_ind$phone_hh <- ifelse(nodes2016_ind$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016_ind$id %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone" | is.na(nodes2016_ind$phone_own)]) |
						(links_l2016$y %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone" | is.na(nodes2016_ind$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016_ind$phone_hh_opt <- ifelse(nodes2016_ind$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016_ind$id %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015_ind <- unique(data[[2]])
	nodes2015_ind <- nodes2015_ind %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015_ind$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	# l_list <- unique(c(links2015$x, links2015$y))
	# temp <- net2015[[2]][, c("id", "age", "sex")] %>%
	# 			dplyr::mutate(id=as.integer(id))
	# l_list <- data.frame(id=l_list[!(l_list %in% nodes2015_ind$id)]) %>% # List of ID in the file for children but not in the other ones
	# 			dplyr::left_join(., temp, by="id")
	nodes2015_ind <- nodes2015_ind %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015_ind[match(l_list$id, nodes2015_ind$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015_ind$id, l_list$id)), c("age", "sex")]
	nodes2015_ind <-  nodes2015_ind %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"]) | (links_l2015$y %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015_ind$phone_hh <- ifelse(nodes2015_ind$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015_ind$id %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone" | is.na(nodes2015_ind$phone_own)]) |
						(links_l2015$y %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone" | is.na(nodes2015_ind$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015_ind$phone_hh_opt <- ifelse(nodes2015_ind$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015_ind$id %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015_ind$age <- factor(nodes2015_ind$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016_ind$age <- factor(nodes2016_ind$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015_ind$data <- ifelse(nodes2015_ind$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016_ind$data <- ifelse(nodes2016_ind$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015_ind$travel <- factor(ifelse(is.na(nodes2015_ind$clinic_travel), NA,
							ifelse(nodes2015_ind$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016_ind$travel <- factor(ifelse(is.na(nodes2016_ind$clinic_travel), NA,
							ifelse(nodes2016_ind$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	# nodes2015_ind$d_child0 <- ifelse(is.na(nodes2015_ind$deceased_child), NA,
	# 						ifelse(nodes2015_ind$deceased_child>0, 1, 0))
	# nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
	# 						ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2015_ind$d_child0 <- ifelse(is.na(nodes2015_ind$deceased_child), NA,
							ifelse(nodes2015_ind$deceased_child>0, 1, 0))
	nodes2016_ind$d_child0 <- ifelse(is.na(nodes2016_ind$deceased_child), NA,
							ifelse(nodes2016_ind$deceased_child>0, 1, 0))

	data2015_ind <- nodes2015_ind[nodes2015_ind$phone!="Children",] %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015_ind[nodes2015_ind$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(as.character(nodes2016_ind$other_id)))) %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016_ind <- nodes2016_ind[nodes2016_ind$phone!="Children",] %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_red <- nodes2016_ind[nodes2016_ind$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015_ind$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2015_mis <- nodes2015_ind[nodes2015_ind$phone!="Children",] %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)
	data2015_mis$phone[as.character(data2015_mis$id) %in% na.omit(nodes2016_ind$other_id)] <- NA

	data2016_mis <- nodes2016_ind[nodes2016_ind$phone!="Children",] %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)
	data2016_mis$phone[!is.na(data2016_mis$other_id) & (data2016_mis$other_id %in% as.character(nodes2015_ind$id))] <- NA
	data2016_mis <- data2016_mis %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, year)

	model3 <- "phone~age+sex+dest+clinic_unable+travel+adult_house+year+clinic_time"
	model3_s <- "phone~age+dest+clinic_unable+travel+adult_house+year+clinic_time"
	model3_y <- "phone~age+sex+dest+clinic_unable+travel+adult_house+clinic_time"

	data_imp <- mice::mice(
					rbind(data2015_ind, data2016_red) %>%
					dplyr::select(-id),
					m=15, seed=150, verbose=FALSE)
	data_imp_ind <- mice::mice(
						rbind(data2015_ind, data2016_ind) %>%
						dplyr::select(-id),
						m=15, seed=150, verbose=FALSE)

	global <- summary(mice::pool(with(data_imp,
				glm(stats::formula(model3),
				family = "binomial"))),
				conf.int = TRUE,
				exponentiate = TRUE)[-1, c(1:2, 7:8, 6)]
	men <- summary(mice::pool(with(data_imp,
				glm(stats::formula(model3_s),
				subset=sex=="M", 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)]
	women <- summary(mice::pool(with(data_imp,
				glm(stats::formula(model3_s),
				subset=sex=="F", 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)]
	d_2015 <- summary(mice::pool(with(data_imp_ind,
				glm(stats::formula(model3_y),
				subset=year==2015, 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)]
	d_2016 <- summary(mice::pool(with(data_imp_ind,
				glm(stats::formula(model3_y),
				subset=year==2016, 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)]

	res <- cbind(
			global %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p")) %>%
				dplyr::mutate(ci=paste(round(ci_l, digits=1),
						round(ci_up, digits=1), sep="-"),
					p=round(p, digits=3),
					aOR=round(aOR, digits=1)) %>%
				dplyr::select(var, aOR, ci, p),
			rbind(
				d_2015[1:9,],
				rep(NA, 5),
				d_2015[10,]) %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p")) %>%
				dplyr::mutate(ci=paste(round(ci_l, digits=1),
						round(ci_up, digits=1), sep="-"),
					p=round(p, digits=3),
					aOR=round(aOR, digits=1)) %>%
				dplyr::select(var, aOR, ci, p),
			rbind(
				d_2016[1:9,],
				rep(NA, 5),
				d_2016[10,]) %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p")) %>%
				dplyr::mutate(ci=paste(round(ci_l, digits=1),
						round(ci_up, digits=1), sep="-"),
					p=round(p, digits=3),
					aOR=round(aOR, digits=1)) %>%
				dplyr::select(var, aOR, ci, p),
			rbind(women[1:4,],
				rep(NA, 5),
				women[5:10,]) %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p")) %>%
				dplyr::mutate(ci=paste(round(ci_l, digits=1),
						round(ci_up, digits=1), sep="-"),
					p=round(p, digits=3),
					aOR=round(aOR, digits=1)) %>%
				dplyr::select(var, aOR, ci, p),
			rbind(men[1:4,],
				rep(NA, 5),
				men[5:10,]) %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p")) %>%
				dplyr::mutate(ci=paste(round(ci_l, digits=1),
						round(ci_up, digits=1), sep="-"),
					p=round(p, digits=3),
					aOR=round(aOR, digits=1)) %>%
				dplyr::select(var, aOR, ci, p))[,c(1:4, 6:8, 10:12, 14:16, 18:20)]

	note("Table saved...\n")
	title <- "logistic"
	rmarkdown::render(input = system.file("rmd", "table.Rmd", package = "namibia"),
		output_file=paste0(file, ".html"), output_format = rmarkdown::html_document(toc=FALSE))

}

#' Creating a figure on the pca for the supplementary material
#'
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

pca_suppl <- function(file=file.path(path.workspace, "pca_suppl")){
	note("Data management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	# l_list <- unique(c(links2015$x, links2015$y))
	# temp <- net2015[[2]][, c("id", "age", "sex")] %>%
	# 			dplyr::mutate(id=as.integer(id))
	# l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
	# 			dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$surv <- "2015 survey"
	nodes2016$surv <- "2016 survey"

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")))
	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")))

	note("PCA for the whole dataset...\n")
	prglob <- prcomp(model.matrix(formula(paste0("~",
					paste(c("dest", "clinic_time", "clinic_unable", "travel"), collapse="+"))),
				data=rbind(na.omit(data2015),na.omit(data2016)))[,-1])

	eigenv <- tidyr::gather(data.frame(summary(prglob)$rotation)) %>%
					dplyr::mutate(var=factor(rep(row.names(summary(prglob)$rotation), 4)))

	pc <- tidyr::gather(data.frame(summary(prglob)$importance)[2,]) %>%
			dplyr::mutate(key=factor(key, levels = paste0("PC", 1:4)),
				type="Whole dataset")

	loading <- eigenv[eigenv$key %in% c("PC1", "PC2"),] %>%
				dplyr::mutate(type="Whole dataset")

	note("PCA for the data collection year strata...\n")
	pr2015 <- prcomp(model.matrix(formula(paste0("~",
					paste(c("dest", "clinic_time", "clinic_unable", "travel"), collapse="+"))),
				data=na.omit(data2015))[,-1])
	pr2016 <- prcomp(model.matrix(formula(paste0("~",
					paste(c("dest", "clinic_time", "clinic_unable", "travel"), collapse="+"))),
				data=na.omit(data2016))[,-1])

	eigenv2015 <- tidyr::gather(data.frame(summary(pr2015)$rotation)) %>%
					dplyr::mutate(var=factor(rep(row.names(summary(pr2015)$rotation), 4)))
	eigenv2016 <- tidyr::gather(data.frame(summary(pr2016)$rotation)) %>%
					dplyr::mutate(var=factor(rep(row.names(summary(pr2016)$rotation), 4)))

	pc2015 <- tidyr::gather(data.frame(summary(pr2015)$importance)[2,]) %>%
				dplyr::mutate(key=factor(key, levels = paste0("PC", 1:4)),
				type="2015 stratum")
	pc2016 <- tidyr::gather(data.frame(summary(pr2016)$importance)[2,]) %>%
				dplyr::mutate(key=factor(key, levels = paste0("PC", 1:4)),
				type="2016 stratum")

	loading_fig <- rbind(loading,
					eigenv2015[eigenv2015$key %in% c("PC1", "PC2"),] %>%
						dplyr::mutate(type="2015 stratum"),
					eigenv2016[eigenv2016$key %in% c("PC1", "PC2"),] %>%
						dplyr::mutate(type="2016 stratum")) %>%
					dplyr::mutate(var=factor(var,
						levels=c("clinic_unable", "clinic_time", "travelCar", "dest"),
						labels=c("Unable to access a\nhealth care center", "Travel time to\nhealth care",
							"Travel using a car", "Number of\ntravel destinations"))) %>%
					ggplot2::ggplot(., ggplot2::aes(x=var, y=value, color=value)) +
						ggplot2::geom_hline(yintercept = 0,
							linetype="dashed") +
						ggplot2::geom_point(size=3) +
						ggplot2::facet_grid(type~key) +
						ggplot2::coord_flip() +
						ggplot2::xlab("") +
						ggplot2::ylab("Loading") +
						ggplot2::scale_colour_gradient2(low="blue", mid="grey",
							high="red", midpoint=0) +
						ggplot2::theme_bw() +
						ggplot2::theme(legend.position = "none")

	pc_fig <- rbind(pc, pc2015, pc2016) %>%
				dplyr::mutate(dum="Principal components") %>%
				ggplot2::ggplot(., ggplot2::aes(x=key, y=value*100)) +
					ggplot2::geom_bar(stat = "identity") +
					ggplot2::ylim(0,100) +
					ggplot2::xlab("") +
					ggplot2::ylab("Percentage of variance explained") +
					ggplot2::facet_grid(type~dum) +
					ggplot2::theme_bw()

	fig <- multipanelfigure::multi_panel_figure(width=c(3,6), height=6, rows=1, unit = 'in') %>%
			multipanelfigure::fill_panel(pc_fig, label='A', row=1, column=1) %>%
			multipanelfigure::fill_panel(loading_fig, label='B', row=1, column=2)

	multipanelfigure::save_multi_panel_figure(fig, paste0(file, ".tiff"))
	multipanelfigure::save_multi_panel_figure(fig, paste0(file, ".jpeg"))
}


#' Creating a figure on the trimming for the supplementary material
#'
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

trim_suppl <- function(file=file.path(path.workspace, "trim_supplementary")){
	note("Data management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y))
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$surv <- "2015 survey"
	nodes2016$surv <- "2016 survey"

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)
	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data_imp <- mice::mice(rbind(data2015, data2016), m=15, seed=150)
		
	trim_iter <- function(imp){
		mod_mat <- stats::model.matrix(phone~age+sex+dest+child_house+adult_house+travel+clinic_time+year, data=imp)
		imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
		colnames(imp) <- gsub("-", "_", colnames(imp))
		colnames(imp) <- gsub(">=", "_", colnames(imp))
		colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
		colnames(imp) <- gsub("\\.", "_", colnames(imp))
		repeat{
			n <- nrow(imp)
			mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
				data=imp)
			check <- apply(mod_mat, 2, mean) # Initiating a check
			check <- !(check==1 | check==0) & names(check)!="clinic_time"
			form <- paste0("phone~", paste(names(check)[check], collapse="+"))
			mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
			imp$p <- stats::predict(mod, type="response")
			cutoff_low <- min(imp$p[imp$phone==1])
			cutoff_high <- max(imp$p[imp$phone==0])
			imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
			n_trim <- nrow(imp)
			if(n_trim==n){
				break
			}
		}
		return(imp)
	}

	data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	for(i in 1:length(data_imp)){
		data_imp[[i]]$m <- i
	}

	fig <- data_imp %>%
			do.call("rbind", .) %>%
			dplyr::mutate(m=factor(m,
					levels = 1:15,
					labels = paste0("Imputation ", 1:15)),
				phone=factor(phone,
					levels = 0:1,
					labels = c("No", "Yes"))) %>%
			ggplot2::ggplot(., ggplot2::aes(x=p, color=phone)) +
				ggplot2::geom_density(ggplot2::aes(y=..scaled..), fill=NA, trim=TRUE) +
				ggplot2::xlim(0, 1) +
				ggplot2::facet_wrap(.~m, ncol=3) +
				ggplot2::xlab("Predicted probability of owning a mobile phone") +
				ggplot2::ylab("Scaled density") +
				ggplot2::guides(color=ggplot2::guide_legend(title="Mobile phone owner")) +
				ggplot2::theme_bw() +
				ggplot2::theme(legend.position = "bottom")

	ggplot2::ggsave(paste0(file, ".tiff"), fig, width=8, height=8, unit="in")
	ggplot2::ggsave(paste0(file, ".jpeg"), fig, width=8, height=8, unit="in")
}


#' Creating a Composite figure for figure 1
#'
#' @param boot The number of bootstrap sample.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

composite_fig <- function(boot=1000, file=file.path(path.workspace, "Composite_fig1")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y))
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)


	data <- rbind(data2015, data2016)
	data_ind <- rbind(data2015, data2016_ind)
	data_imp <- data_imp_match <- mice::mice(rbind(data2015, data2016), m=15, seed=150, print=FALSE)
	data_imp_ind_match <- mice::mice(rbind(data2015, data2016_ind), m=15, seed=150, print=FALSE)

	note("Pooled estimate based on matching...")
	data_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	mean_diff_fact <- data_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])

	mean_diff_num <- data_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	var_num <- data_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])

	## Pooling difference testing
	z_fact <- data_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp[, c(1:7,10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:9] <- z_fact[,2:9]/sqrt(se_fact[,2:9])

	t_num <- data_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	glob <- data.frame(t=apply(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:3])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num))) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Whole dataset")

	note("Sub figure 1...\n")
	fig1 <- glob %>%
			dplyr::mutate(
				var=factor(var,
				levels=c("age16_25","age26_35", "age36_45", "age46_59", "age_60", "sexM", "hsv", "clinic_unable", "clinic_time", "travelCar", "dest"),
				labels=c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
					"Men", "HSV shedding", "Unable to access a\nhealth care center", "Time to reach a\nhealth care center",
					"Travel using a car", "Number of\ndestinations reached"))) %>%
			ggplot2::ggplot(., ggplot2::aes(x=t, y=var, color=t)) +
				ggplot2::geom_point(ggplot2::aes(shape=p2), size=2) +
				ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
				ggplot2::scale_color_gradient2(low="blue", mid="grey", high="red",
					midpoint = 0) +
				# ggplot2::facet_wrap(.~type, ncol=2) +
				ggplot2::guides(colour="none", shape=ggplot2::guide_legend(title="")) +
				ggplot2::xlab("Standardized difference") +
				ggplot2::ylab("") +
				ggplot2::theme_bw() +
				ggplot2::theme(legend.position = "none")

##############################################
	trim_iter <- function(imp){
		mod_mat <- stats::model.matrix(phone~age+sex+dest+child_house+adult_house+travel+clinic_time+year+hsv, data=imp)
		imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
		colnames(imp) <- gsub("-", "_", colnames(imp))
		colnames(imp) <- gsub(">=", "_", colnames(imp))
		colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
		colnames(imp) <- gsub("\\.", "_", colnames(imp))
		repeat{
			n <- nrow(imp)
			mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
				data=imp)
			check <- apply(mod_mat, 2, mean) # Initiating a check
			check <- !(check==1 | check==0) & names(check)!="clinic_time"
			form <- paste0("phone~", paste(names(check)[check], collapse="+"))
			mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
			imp$p <- stats::predict(mod, type="response")
			cutoff_low <- min(imp$p[imp$phone==1])
			cutoff_high <- max(imp$p[imp$phone==0])
			imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
			n_trim <- nrow(imp)
			if(n_trim==n){
				break
			}
		}
		return(imp)
	}

	data_imp_match <- lapply(1:15, function(x){mice::complete(data_imp_match, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	data_imp_ind_match <- lapply(1:15, function(x){mice::complete(data_imp_ind_match, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	for(i in 1:length(data_imp_match)){
		data_imp_match[[i]]$m <- i
	}
	for(i in 1:length(data_imp_ind_match)){
		data_imp_ind_match[[i]]$m <- i
	}

	# Doing the calculation with matching
	match_est <- data_imp_match %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_combined <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_combined$varest)
	var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

	match_combined <- rbind(match_combined,
							data.frame(m=99,
								est=mean(match_combined$est),
								varest=var_within+(1+(1/nrow(match_combined)))*var_between))

	note("Gender stratification...\n")
	estim_gender <- function(filter="F"){
		if(filter=="F"){
			seed <- 550
		}else{
			seed <- 222
		}
		data_imp_match <- rbind(data2015, data2016) %>%
						dplyr::filter(sex==filter) %>%
						mice::mice(., m=15, seed=seed, print=FALSE)

		trim_iter <- function(imp){
			mod_mat <- stats::model.matrix(phone~age+dest+child_house+adult_house+travel+clinic_time+year+hsv, data=imp)
			imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
			colnames(imp) <- gsub("-", "_", colnames(imp))
			colnames(imp) <- gsub(">=", "_", colnames(imp))
			colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
			colnames(imp) <- gsub("\\.", "_", colnames(imp))
			repeat{
				n <- nrow(imp)
				mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
					data=imp)
				check <- apply(mod_mat, 2, mean) # Initiating a check
				check <- !(check==1 | check==0) & names(check)!="clinic_time"
				form <- paste0("phone~", paste(names(check)[check], collapse="+"))
				mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
				imp$p <- stats::predict(mod, type="response")
				cutoff_low <- min(imp$p[imp$phone==1])
				cutoff_high <- max(imp$p[imp$phone==0])
				imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
				n_trim <- nrow(imp)
				if(n_trim==n){
					break
				}
			}
			return(imp)
		}

		data_imp_match <- lapply(1:15, function(x){mice::complete(data_imp_match, x)}) %>%
							lapply(., function(temp){
								mod <- with(data=temp, glm(phone~age+dest+child_house+adult_house+travel+clinic_time+year+hsv, family = binomial(link = "logit")))
								temp$p <- stats::predict(mod, type="response")
								return(temp)
							}) %>%
							lapply(., trim_iter)

		for(i in 1:length(data_imp_match)){
			data_imp_match[[i]]$m <- i
		}

		match_est <- data_imp_match %>%
						lapply(., function(temp){
							temp <- temp %>%
									dplyr::mutate(lp=log(p/(1-p)))
							m_phone <- median(temp$lp[temp$phone==1])
							# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
							temp <- temp %>%
									dplyr::mutate(mark=-abs(lp-m_phone)) %>%
									dplyr::arrange(-phone, mark) %>%
									dplyr::select(-mark)
							return(temp)
							}) %>%
						do.call("rbind", .)

		match_combined <- lapply(1:max(match_est$m), function(x){
								data.frame(m=x,
									est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
									varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
							}) %>%
							do.call("rbind", .)

		var_within <- mean(match_combined$varest)
		var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

		match_combined <- rbind(match_combined,
								data.frame(m=99,
									est=mean(match_combined$est),
									varest=var_within+(1+(1/nrow(match_combined)))*var_between))

		return(match_combined)
	}

	match_combined_F <- estim_gender()
	match_combined_M <- estim_gender("M")

	note("Year stratification...\n")
	estim_year <- function(filter=2015){
		if(filter==2015){
			seed <- 505
		}else{
			seed <- 999
		}
		data_imp_match <- rbind(data2015, data2016_ind) %>%
						dplyr::filter(year==filter) %>%
						mice::mice(., m=15, seed=seed, print=FALSE)

		trim_iter <- function(imp){
			mod_mat <- stats::model.matrix(phone~age+dest+child_house+adult_house+travel+clinic_time+sex+hsv, data=imp)
			imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
			colnames(imp) <- gsub("-", "_", colnames(imp))
			colnames(imp) <- gsub(">=", "_", colnames(imp))
			colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
			colnames(imp) <- gsub("\\.", "_", colnames(imp))
			repeat{
				n <- nrow(imp)
				mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
					data=imp)
				check <- apply(mod_mat, 2, mean) # Initiating a check
				check <- !(check==1 | check==0) & names(check)!="clinic_time"
				form <- paste0("phone~", paste(names(check)[check], collapse="+"))
				mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
				imp$p <- stats::predict(mod, type="response")
				cutoff_low <- min(imp$p[imp$phone==1])
				cutoff_high <- max(imp$p[imp$phone==0])
				imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
				n_trim <- nrow(imp)
				if(n_trim==n){
					break
				}
			}
			return(imp)
		}

		data_imp_match <- lapply(1:15, function(x){mice::complete(data_imp_match, x)}) %>%
							lapply(., function(temp){
								mod <- with(data=temp, glm(phone~age+dest+child_house+adult_house+travel+clinic_time+sex+hsv, family = binomial(link = "logit")))
								temp$p <- stats::predict(mod, type="response")
								return(temp)
							}) %>%
							lapply(., trim_iter)

		for(i in 1:length(data_imp_match)){
			data_imp_match[[i]]$m <- i
		}

		match_est <- data_imp_match %>%
						lapply(., function(temp){
							temp <- temp %>%
									dplyr::mutate(lp=log(p/(1-p)))
							m_phone <- median(temp$lp[temp$phone==1])
							# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
							temp <- temp %>%
									dplyr::mutate(mark=-abs(lp-m_phone)) %>%
									dplyr::arrange(-phone, mark) %>%
									dplyr::select(-mark)
							return(temp)
							}) %>%
						do.call("rbind", .)

		match_combined <- lapply(1:max(match_est$m), function(x){
								data.frame(m=x,
									est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
									varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
							}) %>%
							do.call("rbind", .)

		var_within <- mean(match_combined$varest)
		var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

		match_combined <- rbind(match_combined,
								data.frame(m=99,
									est=mean(match_combined$est),
									varest=var_within+(1+(1/nrow(match_combined)))*var_between))

		return(match_combined)
	}

	match_combined_2015 <- estim_year()
	match_combined_2016 <- estim_year(2016)

	note("Sub figure 2...\n")
	fig2 <- rbind(match_combined[16,] %>% dplyr::mutate(type="Complete\ndataset", col=viridis::viridis(3)[1]),
				match_combined_M[16,] %>% dplyr::mutate(type="Men\nstratum", col=viridis::viridis(3)[2]),
				match_combined_F[16,] %>% dplyr::mutate(type="Women\nstratum", col=viridis::viridis(3)[2]),
				match_combined_2015[16,] %>% dplyr::mutate(type="2015\nstratum", col=viridis::viridis(3)[3]),
				match_combined_2016[16,] %>% dplyr::mutate(type="2016\nstratum", col=viridis::viridis(3)[3])) %>%
					dplyr::mutate(type=factor(type,
						levels=rev(c("Complete\ndataset", "Men\nstratum", "Women\nstratum", "2015\nstratum", "2016\nstratum")))) %>%
				ggplot2::ggplot(., ggplot2::aes(x=type, y=est, color=I(col))) +
					ggplot2::geom_point(size=3) +
					ggplot2::geom_errorbar(ggplot2::aes(ymin=est-1.96*sqrt(varest), ymax=est+1.96*sqrt(varest), color=I(col)), width=0.1) +
					ggplot2::geom_hline(yintercept = 0, linetype="dashed") +
					ggplot2::coord_flip() +
					ggplot2::xlab("") +
					ggplot2::ylab("Mean reduction in time necessary\nto reach a health care center") +
					ggplot2::theme_bw()

####################################
	BS_imput <- function(data=data){
		 temp <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
		 temp_i <- mice::mice(temp, m=1, print=FALSE)
		 return(cbind(mice::complete(temp_i, 1), id=temp$id))
	}

	boot_imput <- replicate(boot, BS_imput(data), simplify=FALSE)
	boot_ind_imput <- replicate(boot, BS_imput(data_ind), simplify=FALSE)

	estim <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest)) %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest)) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house)) %>%
						data.frame()

		data_total_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house)) %>%
						data.frame()

		ratio <- cbind((data_phone_2[data_phone_2$phone==1, -1]/data_total_2[1,]),
					(data_phone_1[data_phone_1$phone==1, -1]/data_total_1[1,]))
		return(ratio)
	}

	est <- lapply(boot_imput, estim) %>%
			do.call("rbind", .)

	data_fig <- data.frame(var=colnames(est),
						est=apply(est, 2, mean),
						ci_l=apply(est, 2, quantile, prob=0.025),
						ci_up=apply(est, 2, quantile, prob=0.975)) %>%
					mutate(
						var=factor(var, levels = c("clinic_time", "clinic_unable", "dest",
							"n", "n_adult", "n_child"),
							labels = c("Time to reach a\nhealth care center",
								"Being unable to access a\nhealth care center",
								"Destinations\nreached recently",
								"People living\nin the household",
								"Adults living\nin the household",
								"Children living\nin the household")))

	fig3 <- ggplot2::ggplot(data_fig, ggplot2::aes(x=est, y=var, color=est)) +
				ggplot2::geom_errorbarh(ggplot2::aes(xmin=ci_l,
					xmax=ci_up, y=var, color=est), height=0.25) +
				ggplot2::geom_point(size=3) +
				ggplot2::geom_vline(xintercept = 1, linetype="dashed") +
				ggplot2::scale_color_gradient2(low="blue", mid="grey", high="red",
					midpoint = 1) +
				ggplot2::guides(colour="none") +
				ggplot2::xlab("Ratio phone/total") +
				ggplot2::ylab("") +
				ggplot2::theme_bw()

	multipanelfigure::multi_panel_figure(
		width=16,
		height=4,
		unit="in",
		rows=1,
		columns = 3) %>%
		multipanelfigure::fill_panel(fig1, label="A") %>%
		multipanelfigure::fill_panel(fig2, label="B") %>%
		multipanelfigure::fill_panel(fig3, label="C") %>%
		multipanelfigure::save_multi_panel_figure(paste0(file, ".tiff"), dpi=300)

}


net_phone_acess <- function(file=file.path(path.workspace, "net_fig2")){
	net <- net2015
	data <- net_d_manage(net, verbose=FALSE)
	nodes <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	edges <- edge2015
	nodes <- nodes %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links$min <- apply(links[,1:2],1,min)
	links$max <- apply(links[,1:2],1,max)
	links <- links[links$min!=links$max,]
	links$comb <- paste(links$min, links$max,sep="-")
	links <- links[!duplicated(links$comb),c("x","y")]
	l_list <- unique(c(links$x, links$y))
	temp <- net[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes <- nodes %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes[match(l_list$id, nodes$id), c("age", "sex")] <- l_list[na.omit(match(nodes$id, l_list$id)), c("age", "sex")]
	nodes <-  nodes %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2015], "Positive", "Negative"))

	links_l <- links
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
						"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
						(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")

	edges <- setNames(edges, c("x", "y"))
	edges$link <- unlist(lapply(1:nrow(edges), function(x){
		paste(sort(t(edges)[,x]), collapse="-")
	}))
	links$link <- unlist(lapply(1:nrow(links), function(x){
		paste(sort(t(links)[,x]), collapse="-")
	}))
	links_df <- rbind(edges, links) %>%
					dplyr::group_by(link) %>%
					dplyr::mutate(n=dplyr::row_number()) %>%
					dplyr::ungroup() %>%
					dplyr::filter(n==1) %>%
					dplyr::select(x, y, link)
	added_id <- unique(c(edges$x, edges$y, as.character(nodes$id)))

	net <- igraph::graph_from_data_frame(d=links_df[,c("x", "y")],
				vertices=data.frame(id=added_id), directed=FALSE)
	gnet <- intergraph::asNetwork(net)

	links_df$part <- ifelse(links_df$link %in% edges$link, "solid", "dotted")
	links_df$part_col <- ifelse(links_df$link %in% edges$link, "grey85", "grey40")

	components <- igraph::components(net)

	nodes_net <- data.frame(
					id=added_id,
					d=igraph::degree(net),
					betw=igraph::betweenness(net),
					member=components$membership) %>%
					dplyr::left_join(
						nodes[,c("id", "phone", "clinic_time", "clinic_unable")] %>%
							dplyr::mutate(id=as.character(id)),
						by="id") %>%
					dplyr::mutate(phone=as.character(phone)) %>%
					dplyr::mutate(phone=ifelse(phone=="No data", NA, phone)) %>%
					dplyr::mutate(phone_c=ifelse(phone %in% c("No data", "Children"), NA, phone))

	set.seed(20)
	fig <- GGally::ggnet2(gnet, mode = "fruchtermanreingold", alpha=0.8, size=3)
	coord_2015 <- fig$data[,c("label", "x", "y")] %>%
				dplyr::left_join(nodes_net, by=c("label"="id"))

	links_net_2015 <- setNames(data.frame(links_df), c("from", "to", "link", "part", "part_col")) %>%
					dplyr::left_join(., coord_2015[,c("label", "x", "y")], by=c(c("to"="label"))) %>%
					dplyr::rename(xmax=x) %>%
					dplyr::rename(ymax=y) %>%
					dplyr::left_join(., coord_2015[,c("label", "x", "y")], by=c(c("from"="label")))

	net <- net2016
	data <- net_d_manage(net, verbose=FALSE)
	nodes <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	edges <- edge2016
	nodes <- nodes %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links$min <- apply(links[,1:2],1,min)
	links$max <- apply(links[,1:2],1,max)
	links <- links[links$min!=links$max,]
	links$comb <- paste(links$min, links$max,sep="-")
	links <- links[!duplicated(links$comb),c("x","y")]
	l_list <- unique(c(links$x, links$y))
	temp <- net[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes <- nodes %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes[match(l_list$id, nodes$id), c("age", "sex")] <- l_list[na.omit(match(nodes$id, l_list$id)), c("age", "sex")]
	nodes <-  nodes %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l <- links
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone"]) | (links_l$y %in% nodes$id[nodes$phone=="Phone"]),
						"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]) |
						(links_l$y %in% nodes$id[nodes$phone=="Phone" | is.na(nodes$phone_own)]),
							"Phone", "No Phone")
	links_l$phone <- ifelse((links_l$x %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))) |
						(links_l$y %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes$phone_hh_opt <- ifelse(nodes$id %in% unique(c(links_l$x[links_l$phone=="Phone"],links_l$y[links_l$phone=="Phone"])) |
							nodes$id %in% nodes$id[nodes$phone=="Phone"],
							"Phone", "No Phone")

	edges <- setNames(edges, c("x", "y"))
	edges$link <- unlist(lapply(1:nrow(edges), function(x){
		paste(sort(t(edges)[,x]), collapse="-")
	}))
	links$link <- unlist(lapply(1:nrow(links), function(x){
		paste(sort(t(links)[,x]), collapse="-")
	}))
	links_df <- rbind(edges, links) %>%
					dplyr::group_by(link) %>%
					dplyr::mutate(n=dplyr::row_number()) %>%
					dplyr::ungroup() %>%
					dplyr::filter(n==1) %>%
					dplyr::select(x, y, link)
	added_id <- unique(c(edges$x, edges$y, as.character(nodes$id)))

	net <- igraph::graph_from_data_frame(d=links_df[,c("x", "y")],
				vertices=data.frame(id=added_id), directed=FALSE)
	gnet <- intergraph::asNetwork(net)

	links_df$part <- ifelse(links_df$link %in% edges$link, "solid", "dotted")
	links_df$part_col <- ifelse(links_df$link %in% edges$link, "grey85", "grey40")

	components <- igraph::components(net)

	nodes_net <- data.frame(
					id=added_id,
					d=igraph::degree(net),
					betw=igraph::betweenness(net),
					member=components$membership) %>%
					dplyr::left_join(
						nodes[,c("id", "phone", "clinic_time", "clinic_unable")] %>%
							dplyr::mutate(id=as.character(id)) %>%
							unique(),
						by="id") %>%
					dplyr::mutate(phone=as.character(phone)) %>%
					dplyr::mutate(phone=ifelse(phone=="No data", NA, phone)) %>%
					dplyr::mutate(phone_c=ifelse(phone %in% c("No data", "Children"), NA, phone))

	set.seed(20)
	fig <- GGally::ggnet2(gnet, mode = "fruchtermanreingold", alpha=0.8, size=3)
	coord_2016 <- fig$data[,c("label", "x", "y")] %>%
					dplyr::left_join(nodes_net, by=c("label"="id"))

	links_net_2016 <- setNames(data.frame(links_df), c("from", "to", "link", "part", "part_col")) %>%
						dplyr::left_join(., coord_2016[,c("label", "x", "y")], by=c(c("to"="label"))) %>%
						dplyr::rename(xmax=x) %>%
						dplyr::rename(ymax=y) %>%
						dplyr::left_join(., coord_2016[,c("label", "x", "y")], by=c(c("from"="label")))

	coord <- rbind(
				coord_2015 %>% dplyr::mutate(year=2015),
				coord_2016 %>% dplyr::mutate(year=2016)) %>%
				dplyr::mutate(
					clinic_unable=factor(clinic_unable,
						levels=c(1, 0),
						labels = c("Yes", "No")),
					phone_c=ifelse(is.na(phone_c), NA,
						ifelse(phone_c=="No phone", "No", "Yes")),
					clinic_t=cut(clinic_time,
						breaks=c(0, 3, 6, 12, 24, 40),
						label=c("<3", "3-6", "6-12", "12-24", "24+"),
						include.lowest=TRUE,
						right=FALSE))

	links_net <- rbind(
					links_net_2015 %>% dplyr::mutate(year=2015),
					links_net_2016 %>% dplyr::mutate(year=2016))

	fig <- ggplot2::ggplot() +
			ggplot2::geom_segment(data=links_net,
				ggplot2::aes(x=x, xend=xmax, y=y, yend=ymax),
				linetype="solid",
				size=0.8,
				color="grey") +
			ggplot2::geom_point(
				data=coord,
				# ggplot2::aes(x=x, y=y, fill=clinic_time, color=phone_c),
				ggplot2::aes(x=x, y=y, color=phone_c),
				fill="lightgrey",
				shape=21,
				size=3,
				stroke=1) +
			ggplot2::geom_point(
				data=coord[!is.na(coord$clinic_t),],
				# ggplot2::aes(x=x, y=y, fill=clinic_time, color=phone_c),
				ggplot2::aes(x=x, y=y, fill=clinic_t, color=phone_c),
				shape=21,
				size=3,
				stroke=1) +
			ggplot2::facet_wrap(.~year, ncol=2, scales="free") +
			# ggplot2::scale_fill_gradient(
			# 	name="Time to reach a\nhealthcare centre\n(hours)",
			# 	low="white",
			# 	high="red",
			# 	na.value = "lightgrey") +
			ggplot2::scale_fill_brewer(
				name="Time to reach a\nhealthcare centre\n(hours)",
				palette="Reds",
				na.value = "lightgrey") +
			# ggplot2::scale_colour_brewer(
			# 	name="Phone ownership",
			# 	breaks=c("No", "Yes"),
			# 	palette = "Set1",
			# 	na.value = "darkgrey") +
			ggplot2::scale_colour_manual(
				name="Phone ownership",
				breaks=c("No", "Yes"),
				values = c("darkred", "darkblue"),
				na.value = "darkgrey") +
			ggplot2::theme_void() +
			ggplot2::theme(
				strip.text = ggplot2::element_text(size = 10, face = "bold", hjust=0.5),
				panel.border = ggplot2::element_rect(colour = "black", fill=NA),
				strip.background = ggplot2::element_rect(colour = "black", fill="grey"))

	note("Plot saved...\n")
	ggplot2::ggsave(
		paste0(file, ".tiff"),
		fig,
		width=14,
		height=6,
		unit="in")

}

#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

fig1_zoom <- function(boot=1000, file=file.path(path.workspace, "fig1_zoom")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_mis <- data2016_ind %>%
						dplyr::mutate(
							phone=ifelse((other_id %in% na.omit(as.character(nodes2015$id)))==TRUE, NA, phone)) %>%
						dplyr::select(-other_id)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	data2015_mis <- data2015 %>%
						dplyr::mutate(
							phone=ifelse((as.character(id) %in% na.omit(nodes2016$other_id))==TRUE, NA, phone))

	note("\nLove plot...\n")
	data_imp <- mice::mice(rbind(data2015, data2016) %>% select(-id), m=15, seed=150)
	data_ind_imp <- mice::mice(rbind(data2015, data2016_ind) %>% select(-id), m=15, seed=150)

	data_imp_M <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								filter(sex=="M") %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp_F <- lapply(1:15, function(x){
						temp <- mice::complete(data_imp, x) %>%
									filter(sex=="F") %>%
									dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+dest+clinic_time+clinic_unable+travel+hsv+adult_house, data=temp)[,-1])) %>%
								dplyr::mutate(m=x)
						colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
						colnames(temp) <- gsub("\\.", "_", colnames(temp))
						return(temp)}) %>%
						do.call("rbind", .)

	data_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x) %>%
								dplyr::mutate(age16_25=ifelse(age=="16-25", 1, 0))
					temp <- cbind(phone=temp$phone,
								data.frame(stats::model.matrix(phone~age16_25+age+sex+dest+clinic_time+clinic_unable+travel+d_child0+hsv+adult_house, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	mean_diff_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),
						hsv=diff(hsv))

	var_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						d_child0=mean(d_child0)*(1-mean(d_child0))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						d_child0=sum(d_child0),
						hsv=sum(hsv))

	mean_diff_fact[,2:11] <- mean_diff_fact[,2:11]/sqrt(var_fact[,2:11])

	mean_diff_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	var_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),,
						d_child0=mean(d_child0),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						d_child0=diff(d_child0),,
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp[, c(1:7,10:13,15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						d_child0=mean(d_child0),,
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						d_child0=d_child0*(1-d_child0)*((1/n1)+(1/n2)),,
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:9] <- z_fact[,2:9]/sqrt(se_fact[,2:9])

	t_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp[, c(1, 8:9, 14:15)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	glob <- data.frame(
				t=apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
				p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact, df_num))) %>%
				dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
					levels=1:2, labels=c("p<0.05", "p>0.05")),
					type="Whole dataset",
					group="complete")

	# Men
	mean_diff_fact <- data_imp_M[, c(1:6,9:11, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp_M[, c(1:6,9:11, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:9] <- mean_diff_fact[,2:9]/sqrt(var_fact[,2:9])

	mean_diff_num <- data_imp_M[, c(1, 7:8, 12:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	var_num <- data_imp_M[, c(1, 7:8, 12:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp_M[, c(1:6,9:11, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_M[, c(1:6,9:11, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:8] <- z_fact[,2:8]/sqrt(se_fact[,2:8])

	t_num <- data_imp_M[, c(1, 7:8, 12:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_M[, c(1, 7:8, 12:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_M <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="Gender strata",
						group="Men") %>%
					dplyr::filter(var %in% c("clinic_time", "dest"))

	# Women
	mean_diff_fact <- data_imp_F[, c(1:6,9:11, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp_F[, c(1:6,9:11, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:9] <- mean_diff_fact[,2:9]/sqrt(var_fact[,2:9])

	mean_diff_num <- data_imp_F[, c(1, 7:8, 12:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	var_num <- data_imp_F[, c(1, 7:8, 12:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n(),
						adult_house=var(adult_house)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	mean_diff_num[,2:4] <- mean_diff_num[,2:4]/sqrt(var_num[,2:4])

	## Pooling difference testing
	z_fact <- data_imp_F[, c(1:6,9:11, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	between_var_fact <- apply(z_fact[,-1], 2, function(x){
		(1/(max(z_fact$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp_F[, c(1:6,9:11, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv),
						n1=mean(n1),
						n2=mean(n2)) %>%
					dplyr::mutate(age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	within_var_fact <- apply(se_fact[,-1], 2, mean)
	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2

	# z_fact[,2:8] <- z_fact[,2:8]/sqrt(se_fact[,2:8])

	t_num <- data_imp_F[, c(1, 7:8, 12:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time),
						adult_house=mean(adult_house)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time),
						adult_house=diff(adult_house))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})

	se_num <- data_imp_F[, c(1, 7:8, 12:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest),
						clinic_time=var(clinic_time),
						adult_house=var(adult_house),
						n=mean(n)) %>%
					dplyr::mutate(dest=dest/n,
						clinic_time=clinic_time/n,
						adult_house=adult_house/n) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time),
						adult_house=sum(adult_house))

	within_var_num <- apply(se_num[,-1], 2, mean)
	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2

	stratum_F <- data.frame(
					var=colnames(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1]),
					t=round(apply(cbind(mean_diff_fact, mean_diff_num[,2:4])[,-1], 2, mean), digits=3),
					p=apply(cbind(z_fact[,-1], t_num[,-1]), 2, mean)^2) %>%
					dplyr::mutate(p=p/(c(within_var_fact, within_var_num)+(1+(1/max(z_fact$m)))*c(between_var_fact, between_var_num))) %>%
					dplyr::mutate(p=round(1-pf(abs(p), df1=1, df2=c(df_fact, df_num)), digits=3))  %>%
					dplyr::mutate(p2=factor(as.integer(cut(p, breaks=c(0, 0.05, 1), include.lowest=TRUE)),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
						type="Gender strata",
						group="Women") %>%
					dplyr::filter(var %in% c("clinic_time", "dest"))

	note("Plotting...\n")
	love_fig <- rbind(glob,
					stratum_M,
					stratum_F) %>%
					dplyr::mutate(
						type=factor(type,
							levels=c("Whole dataset", "Gender strata")),
						var=factor(var,
						levels=rev(c("age16_25","age26_35", "age36_45", "age46_59", "age_60", "sexM", "adult_house", "hsv", "d_child0", "clinic_unable", "clinic_time", "travelCar", "dest")),
						labels=rev(c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
							"Proportion of men", "Number of adults\nin the household","HSV shedding",
							"Reported at least\none deceased child", "Unable to access a\nhealth care center", "Time to reach a\nhealth care center",
							"Travel using a car", "Number of\ndestinations reached")))) %>%
					ggplot2::ggplot(., ggplot2::aes(x=t, y=var, color=group)) +
						ggplot2::geom_point(
							ggplot2::aes(shape=p2),
							# position=ggplot2::position_dodge(width=1),
							size=2) +
						ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
						ggplot2::scale_color_manual(
							name="",
							breaks=c("Men", "Women"),
							values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
						ggplot2::facet_grid(.~type) +
						ggplot2::guides(shape=ggplot2::guide_legend(title="")) +
						ggplot2::xlab("Standardized difference between mobile phone owners and non-owners") +
						ggplot2::ylab("") +
						ggplot2::theme_bw() +
						ggplot2::theme(
							legend.position = c(0.9, 0.7),
							legend.box.background = ggplot2::element_rect(
								colour = "black",
								fill="white"))

	ggplot2::ggsave(
		paste0(file, ".tiff"),
		love_fig,
		width=7,
		height=7)
	note("Figure saved...\n")
}


#' Creating a html table on the logistic regression
#'
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A html table.

logistic_fig <- function(file=file.path(path.workspace, "logistic_fig"), zoom=TRUE){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016_ind <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016_ind <- nodes2016_ind[!(nodes2016_ind$id==216 & nodes2016_ind$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"]) | (links_l2016$y %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016_ind$phone_hh <- ifelse(nodes2016_ind$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016_ind$id %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone" | is.na(nodes2016_ind$phone_own)]) |
						(links_l2016$y %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone" | is.na(nodes2016_ind$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016_ind$phone_hh_opt <- ifelse(nodes2016_ind$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016_ind$id %in% nodes2016_ind$id[nodes2016_ind$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015_ind <- unique(data[[2]])
	nodes2015_ind <- nodes2015_ind %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015_ind$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	# l_list <- unique(c(links2015$x, links2015$y))
	# temp <- net2015[[2]][, c("id", "age", "sex")] %>%
	# 			dplyr::mutate(id=as.integer(id))
	# l_list <- data.frame(id=l_list[!(l_list %in% nodes2015_ind$id)]) %>% # List of ID in the file for children but not in the other ones
	# 			dplyr::left_join(., temp, by="id")
	nodes2015_ind <- nodes2015_ind %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015_ind[match(l_list$id, nodes2015_ind$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015_ind$id, l_list$id)), c("age", "sex")]
	nodes2015_ind <-  nodes2015_ind %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"]) | (links_l2015$y %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015_ind$phone_hh <- ifelse(nodes2015_ind$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015_ind$id %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone" | is.na(nodes2015_ind$phone_own)]) |
						(links_l2015$y %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone" | is.na(nodes2015_ind$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015_ind$phone_hh_opt <- ifelse(nodes2015_ind$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015_ind$id %in% nodes2015_ind$id[nodes2015_ind$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015_ind$age <- factor(nodes2015_ind$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016_ind$age <- factor(nodes2016_ind$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015_ind$data <- ifelse(nodes2015_ind$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016_ind$data <- ifelse(nodes2016_ind$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015_ind$travel <- factor(ifelse(is.na(nodes2015_ind$clinic_travel), NA,
							ifelse(nodes2015_ind$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016_ind$travel <- factor(ifelse(is.na(nodes2016_ind$clinic_travel), NA,
							ifelse(nodes2016_ind$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	# nodes2015_ind$d_child0 <- ifelse(is.na(nodes2015_ind$deceased_child), NA,
	# 						ifelse(nodes2015_ind$deceased_child>0, 1, 0))
	# nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
	# 						ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2015_ind$d_child0 <- ifelse(is.na(nodes2015_ind$deceased_child), NA,
							ifelse(nodes2015_ind$deceased_child>0, 1, 0))
	nodes2016_ind$d_child0 <- ifelse(is.na(nodes2016_ind$deceased_child), NA,
							ifelse(nodes2016_ind$deceased_child>0, 1, 0))

	data2015_ind <- nodes2015_ind[nodes2015_ind$phone!="Children",] %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015_ind[nodes2015_ind$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(as.character(nodes2016_ind$other_id)))) %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016_ind <- nodes2016_ind[nodes2016_ind$phone!="Children",] %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_red <- nodes2016_ind[nodes2016_ind$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015_ind$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2015_mis <- nodes2015_ind[nodes2015_ind$phone!="Children",] %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)
	data2015_mis$phone[as.character(data2015_mis$id) %in% na.omit(nodes2016_ind$other_id)] <- NA

	data2016_mis <- nodes2016_ind[nodes2016_ind$phone!="Children",] %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)
	data2016_mis$phone[!is.na(data2016_mis$other_id) & (data2016_mis$other_id %in% as.character(nodes2015_ind$id))] <- NA
	data2016_mis <- data2016_mis %>%
					dplyr::select(id, phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, year)

	model3 <- "phone~age+sex+dest+clinic_unable+travel+adult_house+year+clinic_time"
	model3_s <- "phone~age+dest+clinic_unable+travel+adult_house+year+clinic_time"
	model3_y <- "phone~age+sex+dest+clinic_unable+travel+adult_house+clinic_time"

	data_imp <- mice::mice(
					rbind(data2015_ind, data2016_red) %>%
					dplyr::select(-id),
					m=15, seed=150, verbose=FALSE)
	data_imp_ind <- mice::mice(
						rbind(data2015_ind, data2016_ind) %>%
						dplyr::select(-id),
						m=15, seed=150, verbose=FALSE)

	global <- summary(mice::pool(with(data_imp,
				glm(stats::formula(model3),
				family = "binomial"))),
				conf.int = TRUE,
				exponentiate = TRUE)[-1, c(1:2, 7:8, 6)] %>%
				dplyr::mutate(
					type="Whole dataset",
					col="Whole dataset")
	men <- summary(mice::pool(with(data_imp,
				glm(stats::formula(model3_s),
				subset=sex=="M", 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)] %>%
				dplyr::mutate(
					type="Gender strata",
					col="Men")
	women <- summary(mice::pool(with(data_imp,
				glm(stats::formula(model3_s),
				subset=sex=="F", 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)] %>%
				dplyr::mutate(
					type="Gender strata",
					col="Women")
	d_2015 <- summary(mice::pool(with(data_imp_ind,
				glm(stats::formula(model3_y),
				subset=year==2015, 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)] %>%
				dplyr::mutate(
					type="Yearly strata",
					col="2015")
	d_2016 <- summary(mice::pool(with(data_imp_ind,
				glm(stats::formula(model3_y),
				subset=year==2016, 
				family = "binomial"))),
				conf.int = TRUE, exponentiate = TRUE)[-1, c(1:2, 7:8, 6)] %>%
				dplyr::mutate(
					type="Yearly strata",
					col="2016")

	res <- rbind(
			global %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p", "type", "col")),
			rbind(
				d_2015 %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p", "type", "col")),
			rbind(
				d_2016 %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p", "type", "col")),
			rbind(women %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p", "type", "col")),
			rbind(men %>%
				setNames(., c("var", "aOR", "ci_l", "ci_up", "p", "type", "col")))))))
	if(zoom){
		res <- res %>%
				dplyr::filter(var %in% c(
							"age16-25","age26-35", "age36-45",
							"age46-59", "age>=60", "sexM",
							"dest", "clinic_time", "year")) %>%
				dplyr::mutate(
					type=factor(type,
						levels=c("Whole dataset", "Yearly strata", "Gender strata")),
					var=factor(var,
						levels=rev(c("age16-25","age26-35", "age36-45", "age46-59", "age>=60",
							"sexM", "clinic_time", "dest", "year")),
						labels=rev(c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
							"Men", "Travel time to access a\nhealth care center", "Number of travel\ndestinations",
							"Interviewed in 2016"))))
	}else{
		res <- res %>%
				dplyr::mutate(
					type=factor(type,
						levels=c("Whole dataset", "Yearly strata", "Gender strata")),
					var=factor(var,
						levels=rev(c("age16-25","age26-35", "age36-45", "age46-59", "age>=60",
							"sexM", "year", "adult_house", "hsv", "d_child0",
							"clinic_unable", "clinic_time", "travelCar", "dest")),
						labels=rev(c("16-25 years","26-35 years", "36-45 years", "46-59 years", "60+ years",
							"Men", "Interviewed in 2016", "Number of adults\nin the household",
							"HSV shedding", "At least 1 deceased child", "Unable to access a\nhealth care center",
							"Travel time to acess a\nhealth care center",
							"Travel using a car", "Number of travel\ndestinations"))))
	}


	base_breaks <- function(n = 5){
	    function(x) {
	        axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
	    }
	}

	fig <- ggplot2::ggplot(res) +
			ggplot2::geom_errorbarh(
				ggplot2::aes(xmin=ci_l, xmax=ci_up, y=var, color=col),
				height=0.3,
				position=ggplot2::position_dodge(width=0.5)) +
			ggplot2::geom_point(
				ggplot2::aes(x=aOR, y=var, color=col),
				size=2,
				position=ggplot2::position_dodge(width=0.5)) +
			ggplot2::geom_vline(xintercept = 1, linetype="dashed") +
			ggplot2::facet_grid(.~type) +
			ggplot2::scale_x_continuous(
				trans="log10",
				breaks = base_breaks(),
				labels=prettyNum) +
			ggplot2::theme_bw() +
			ggplot2::ylab("") +
			ggplot2::scale_color_manual(
				name="",
				breaks=c("Whole dataset", "Men", "Women", "2015", "2016"),
				values=c("black", "red", "blue", "darkgreen", "brown"))

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

	fig_light <- ggplot2::ggplot(
					data=res %>%
						dplyr::filter(var %in% c("60+ years", "Men", "Interviewed in 2016",
							"Travel time to access a\nhealth care center", "Number of travel\ndestinations")) %>%
						dplyr::mutate(
							group_var=ifelse(var %in% c("60+ years", "Men"), "Demographic\ncharacteristics",
							ifelse(var %in% c("Travel time to access a\nhealth care center", "Number of travel\ndestinations"),
								"Mobility", "Rectuitment\nyear")),
							col=factor(
								col,
								levels=rev(c("Whole dataset", "Men", "Women", "2015", "2016"))))) +
					ggplot2::geom_errorbarh(
						ggplot2::aes(xmin=ci_l, xmax=ci_up, y=var, color=col),
						height=0.3,
						position=ggplot2::position_dodge(width=0.5)) +
					ggplot2::geom_point(
						ggplot2::aes(x=aOR, y=var, color=col),
						size=2,
						position=ggplot2::position_dodge(width=0.5)) +
					ggplot2::geom_vline(xintercept = 1, linetype="dashed") +
					ggh4x::facet_grid2(
							group_var~.,
							scales="free",
							independent="x",
							space="free") +
					# ggplot2::facet_grid(
					# 	group_var~.,
					# 	scales="free",
					# 	space="free_y") +
					# ggplot2::scale_y_discrete(
					# 	breaks=c("25%ile", "Mean", "75%ile")) +
					ggplot2::scale_x_continuous(
						trans="log10",
						breaks = base_breaks(),
						labels=prettyNum) +
					ggplot2::theme_bw() +
					ggplot2::ylab("") +
					ggplot2::xlab("Adjusted Odds Ratio") +
					ggplot2::scale_color_manual(
						name="",
						breaks=c("Whole dataset", "Men", "Women", "2015", "2016"),
						values=c("black", "red", "blue", "darkgreen", "brown")) +
					ggplot2::theme(strip.text.y = ggplot2::element_blank())

	ggplot2::ggsave(
		paste0(file, ".eps"),
		fig_light,
		width=6,
		height=8)

	if(zoom){
		ggplot2::ggsave(
			paste0(file, ".tiff"),
			fig_light,
			width=6,
			height=8)
	}else{
		ggplot2::ggsave(
			paste0(file, ".tiff"),
			fig,
			width=10,
			height=6.25)
	}
	note("Figure saved...\n")
}


#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

fig1_mean_red <- function(boot=1000, file=file.path(path.workspace, "fig1_mean_red")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel, d_child0,
						child_house, adult_house, size_house, hsv, id, other_id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_mis <- data2016_ind %>%
						dplyr::mutate(
							phone=ifelse((other_id %in% na.omit(as.character(nodes2015$id)))==TRUE, NA, phone)) %>%
						dplyr::select(-other_id)

	data2016_ind <- data2016_ind %>%
						dplyr::select(-other_id)

	data2015_mis <- data2015 %>%
						dplyr::mutate(
							phone=ifelse((as.character(id) %in% na.omit(nodes2016$other_id))==TRUE, NA, phone))

	note("\nMean reduction...\n")
	note("\nPooled estimate based on matching...\n")
	# data_imp <- mice::mice(rbind(data2015, data2016), m=15, seed=150, print=FALSE)
	# data_imp_ind <- mice::mice(rbind(data2015, data2016_ind), m=15, seed=150, print=FALSE)
	data_imp <- mice::mice(rbind(data2015, data2016), m=15, seed=1234, print=FALSE)
	data_imp_ind <- mice::mice(rbind(data2015, data2016_ind), m=15, seed=1234, print=FALSE)
	data_imp <- mice::mice(rbind(data2015, data2016), m=18, seed=1234, print=FALSE)
	data_imp_ind <- mice::mice(rbind(data2015, data2016_ind), m=18, seed=1234, print=FALSE)

	trim_iter <- function(imp){
		mod_mat <- stats::model.matrix(phone~age+sex+dest+child_house+adult_house+travel+clinic_time+year+hsv, data=imp)
		imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
		colnames(imp) <- gsub("-", "_", colnames(imp))
		colnames(imp) <- gsub(">=", "_", colnames(imp))
		colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
		colnames(imp) <- gsub("\\.", "_", colnames(imp))
		repeat{
			n <- nrow(imp)
			mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
				data=imp)
			check <- apply(mod_mat, 2, mean) # Initiating a check
			check <- !(check==1 | check==0) & names(check)!="clinic_time"
			form <- paste0("phone~", paste(names(check)[check], collapse="+"))
			mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
			imp$p <- stats::predict(mod, type="response")
			cutoff_low <- min(imp$p[imp$phone==1])
			cutoff_high <- max(imp$p[imp$phone==0])
			imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
			n_trim <- nrow(imp)
			if(n_trim==n){
				break
			}
		}
		return(imp)
	}

	data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	data_imp_ind <- lapply(1:15, function(x){mice::complete(data_imp_ind, x)}) %>%
						lapply(., function(temp){
							mod <- with(data=temp, glm(phone~age+sex+dest+child_house+adult_house+travel+year+hsv, family = binomial(link = "logit")))
							temp$p <- stats::predict(mod, type="response")
							return(temp)
						}) %>%
						lapply(., trim_iter)

	for(i in 1:length(data_imp)){
		data_imp[[i]]$m <- i
	}
	for(i in 1:length(data_imp_ind)){
		data_imp_ind[[i]]$m <- i
	}

	# Doing the calculation with matching
	match_est <- data_imp %>%
					lapply(., function(temp){
						temp <- temp %>%
								dplyr::mutate(lp=log(p/(1-p)))
						m_phone <- median(temp$lp[temp$phone==1])
						# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
						temp <- temp %>%
								dplyr::mutate(mark=-abs(lp-m_phone)) %>%
								dplyr::arrange(-phone, mark) %>%
								dplyr::select(-mark)
						return(temp)
						}) %>%
					do.call("rbind", .)

	match_combined <- lapply(1:max(match_est$m), function(x){
							data.frame(m=x,
								est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
								varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
						}) %>%
						do.call("rbind", .)

	var_within <- mean(match_combined$varest)
	var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

	match_combined <- rbind(match_combined,
							data.frame(m=99,
								est=mean(match_combined$est),
								varest=var_within+(1+(1/nrow(match_combined)))*var_between))

	note("Gender stratification...\n")
	estim_gender <- function(filter="F"){
		if(filter=="F"){
			seed <- 550
		}else{
			seed <- 222
		}
		data_imp <- rbind(data2015, data2016) %>%
						dplyr::filter(sex==filter) %>%
						mice::mice(., m=15, seed=seed, print=FALSE)

		trim_iter <- function(imp){
			mod_mat <- stats::model.matrix(phone~age+dest+child_house+adult_house+travel+clinic_time+year+hsv, data=imp)
			imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
			colnames(imp) <- gsub("-", "_", colnames(imp))
			colnames(imp) <- gsub(">=", "_", colnames(imp))
			colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
			colnames(imp) <- gsub("\\.", "_", colnames(imp))
			repeat{
				n <- nrow(imp)
				mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
					data=imp)
				check <- apply(mod_mat, 2, mean) # Initiating a check
				check <- !(check==1 | check==0) & names(check)!="clinic_time"
				form <- paste0("phone~", paste(names(check)[check], collapse="+"))
				mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
				imp$p <- stats::predict(mod, type="response")
				cutoff_low <- min(imp$p[imp$phone==1])
				cutoff_high <- max(imp$p[imp$phone==0])
				imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
				n_trim <- nrow(imp)
				if(n_trim==n){
					break
				}
			}
			return(imp)
		}

		data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
							lapply(., function(temp){
								mod <- with(data=temp, glm(phone~age+dest+child_house+adult_house+travel+clinic_time+year+hsv, family = binomial(link = "logit")))
								temp$p <- stats::predict(mod, type="response")
								return(temp)
							}) %>%
							lapply(., trim_iter)

		for(i in 1:length(data_imp)){
			data_imp[[i]]$m <- i
		}

		match_est <- data_imp %>%
						lapply(., function(temp){
							temp <- temp %>%
									dplyr::mutate(lp=log(p/(1-p)))
							m_phone <- median(temp$lp[temp$phone==1])
							# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
							temp <- temp %>%
									dplyr::mutate(mark=-abs(lp-m_phone)) %>%
									dplyr::arrange(-phone, mark) %>%
									dplyr::select(-mark)
							return(temp)
							}) %>%
						do.call("rbind", .)

		match_combined <- lapply(1:max(match_est$m), function(x){
								data.frame(m=x,
									est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
									varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
							}) %>%
							do.call("rbind", .)

		var_within <- mean(match_combined$varest)
		var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

		match_combined <- rbind(match_combined,
								data.frame(m=99,
									est=mean(match_combined$est),
									varest=var_within+(1+(1/nrow(match_combined)))*var_between))

		return(match_combined)
	}

	match_combined_F <- estim_gender()
	match_combined_M <- estim_gender("M")

	note("Year stratification...\n")
	estim_year <- function(filter=2015){
		if(filter==2015){
			seed <- 505
		}else{
			seed <- 999
		}
		data_imp <- rbind(data2015, data2016_ind) %>%
						dplyr::filter(year==filter) %>%
						mice::mice(., m=15, seed=seed, print=FALSE)

		trim_iter <- function(imp){
			mod_mat <- stats::model.matrix(phone~age+dest+child_house+adult_house+travel+clinic_time+sex+hsv, data=imp)
			imp <- cbind(phone=imp$phone, data.frame(mod_mat)[,-1])
			colnames(imp) <- gsub("-", "_", colnames(imp))
			colnames(imp) <- gsub(">=", "_", colnames(imp))
			colnames(imp) <- gsub("\\.\\.", "_", colnames(imp))
			colnames(imp) <- gsub("\\.", "_", colnames(imp))
			repeat{
				n <- nrow(imp)
				mod_mat <- stats::model.matrix(stats::formula(paste0("phone~", paste(colnames(imp)[-1], collapse="+"))),
					data=imp)
				check <- apply(mod_mat, 2, mean) # Initiating a check
				check <- !(check==1 | check==0) & names(check)!="clinic_time"
				form <- paste0("phone~", paste(names(check)[check], collapse="+"))
				mod <- glm(stats::formula(form), family = binomial(link = "logit"), data=imp)
				imp$p <- stats::predict(mod, type="response")
				cutoff_low <- min(imp$p[imp$phone==1])
				cutoff_high <- max(imp$p[imp$phone==0])
				imp <- imp[(imp$phone==0 & imp$p>=cutoff_low)|(imp$phone==1 & imp$p<=cutoff_high),]
				n_trim <- nrow(imp)
				if(n_trim==n){
					break
				}
			}
			return(imp)
		}

		data_imp <- lapply(1:15, function(x){mice::complete(data_imp, x)}) %>%
							lapply(., function(temp){
								mod <- with(data=temp, glm(phone~age+dest+child_house+adult_house+travel+clinic_time+sex+hsv, family = binomial(link = "logit")))
								temp$p <- stats::predict(mod, type="response")
								return(temp)
							}) %>%
							lapply(., trim_iter)

		for(i in 1:length(data_imp)){
			data_imp[[i]]$m <- i
		}

		match_est <- data_imp %>%
						lapply(., function(temp){
							temp <- temp %>%
									dplyr::mutate(lp=log(p/(1-p)))
							m_phone <- median(temp$lp[temp$phone==1])
							# ordering the rows so that the individuals with the more extreme lp are matched first with the greedy algorithm
							temp <- temp %>%
									dplyr::mutate(mark=-abs(lp-m_phone)) %>%
									dplyr::arrange(-phone, mark) %>%
									dplyr::select(-mark)
							return(temp)
							}) %>%
						do.call("rbind", .)

		match_combined <- lapply(1:max(match_est$m), function(x){
								data.frame(m=x,
									est=Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$est,
									varest=(Matching::Match(Y=match_est$clinic_time[match_est$m==x], match_est$phone[match_est$m==x], match_est$lp[match_est$m==x], replace=FALSE)$se.standard)^2)
							}) %>%
							do.call("rbind", .)

		var_within <- mean(match_combined$varest)
		var_between <- (1/(nrow(match_combined)-1))*sum((match_combined$est-mean(match_combined$est))^2)

		match_combined <- rbind(match_combined,
								data.frame(m=99,
									est=mean(match_combined$est),
									varest=var_within+(1+(1/nrow(match_combined)))*var_between))

		return(match_combined)
	}

	match_combined_2015 <- estim_year()
	match_combined_2016 <- estim_year(2016)

	note("Plotting...\n")
	redu <- rbind(match_combined[16,] %>% dplyr::mutate(type="Whole\ndataset", col=viridis::viridis(3)[1]),
				match_combined_M[16,] %>% dplyr::mutate(type="Men\nstratum", col=viridis::viridis(3)[2]),
				match_combined_F[16,] %>% dplyr::mutate(type="Women\nstratum", col=viridis::viridis(3)[2]),
				match_combined_2015[16,] %>% dplyr::mutate(type="Rainy season -\n2015 stratum", col=viridis::viridis(3)[3]),
				match_combined_2016[16,] %>% dplyr::mutate(type="Dry season -\n2016 stratum", col=viridis::viridis(3)[3])) %>%
					dplyr::mutate(type=factor(type,
						levels=rev(c("Whole\ndataset", "Men\nstratum", "Women\nstratum", "Rainy season -\n2015 stratum", "Dry season -\n2016 stratum")))) %>%
				ggplot2::ggplot(., ggplot2::aes(x=type, y=est, color=type)) +
					ggplot2::geom_point(size=3) +
					ggplot2::geom_errorbar(
						ggplot2::aes(ymin=est-1.96*sqrt(varest), ymax=est+1.96*sqrt(varest)),
						width=0.1) +
					ggplot2::geom_hline(yintercept = 0, linetype="dashed") +
					ggplot2::scale_color_manual(
						name="",
						breaks=c("Whole\ndataset", "Men\nstratum", "Women\nstratum",
							"Rainy season -\n2015 stratum", "Dry season -\n2016 stratum"),
						values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2],
							"gray65", "gray50")) +
					ggplot2::guides(colour="none") +
					ggplot2::coord_flip() +
					ggplot2::xlab("") +
					ggplot2::ylab("Mean reduction in travel time (hours) to a health\ncare center for mobile phone owners compared to non-owners") +
					ggplot2::theme_bw()

	ggplot2::ggsave(
		paste0(file, ".tiff"),
		redu,
		width=6,
		height=4)
	note("Figure saved...\n")
}

#' Creating a figure on the bias introduced
#'
#' @param boot Number of bootstrap simulations to calculate the 95% CI. By default it is 1000.
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

fig2_zoom <- function(boot=1000, file=file.path(path.workspace, "fig2_zoom")){
	note("\nData management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- as.integer(unique(c(links2015$x, links2015$y)))
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data2016_ind <- nodes2016[nodes2016$phone!="Children",] %>%
					dplyr::filter(age_c==0) %>%
					dplyr::select(phone, age, sex, dest, 
						clinic_time, clinic_unable, travel,
						child_house, adult_house, size_house,
						clinic_cost_travel, clinic_cost_travel_ind, d_child0, d_child1, hsv, id) %>%
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	data <- rbind(data2015, data2016)
	data_ind <- rbind(data2015, data2016_ind)

	BS_imput <- function(data=data){
		 temp <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
		 temp_i <- mice::mice(temp, m=1, print=FALSE)
		 return(cbind(mice::complete(temp_i, 1), id=temp$id))
	}

	boot_imput <- replicate(boot, BS_imput(data), simplify=FALSE)
	# boot_ind_2015 <- replicate(boot, BS_imput(data2015), simplify=FALSE)
	# boot_ind_2016 <- replicate(boot, BS_imput(data2016_ind), simplify=FALSE)
	boot_ind <- replicate(boot, BS_imput(data_ind), simplify=FALSE)

	estim <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_total_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		data_total_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		ratio <- cbind(
					(data_phone_2[data_phone_2$phone==1, -1]/data_total_2[1,]),
					# (data_phone_2[data_phone_2$phone==1, 4]-data_total_2[1,4]),
					# (data_phone_2[data_phone_2$phone==1, -c(1, 4)]/data_total_2[1,-4]),
					# (data_phone_2[data_phone_2$phone==1, 4]-data_total_2[1,4]),
					(data_phone_1[data_phone_1$phone==1, -1]/data_total_1[1,]))
		return(ratio)
	}

	estim_ratio <- function(temp){
		data_phone_1 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(
							clinic_time_75=quantile(clinic_time, probs=0.75),
							clinic_time_25=quantile(clinic_time, probs=0.25),
							clinic_time=mean(clinic_time),
							clinic_unable=mean(clinic_unable),
							dest=mean(dest),
							d_child0=mean(d_child0),
							d_child1=mean(d_child1)) %>%
						data.frame()

		data_phone_2 <- temp %>%
						dplyr::select(-id) %>%
						dplyr::group_by(phone) %>%
						dplyr::summarise(n=mean(size_house),
							n_adult=mean(adult_house),
							n_child=mean(child_house),
							clinic_cost_travel_75=quantile(clinic_cost_travel, probs=0.75),
							clinic_cost_travel_25=quantile(clinic_cost_travel, probs=0.25),
							clinic_cost_travel=mean(clinic_cost_travel)
							# clinic_cost_travel_ind_75=quantile(clinic_cost_travel_ind, probs=0.75),
							# clinic_cost_travel_ind_25=quantile(clinic_cost_travel_ind, probs=0.25),
							# clinic_cost_travel_ind=mean(clinic_cost_travel_ind)
							) %>%
						data.frame()

		ratio <- cbind(
					(data_phone_2[data_phone_2$phone==1, -1]/data_phone_2[data_phone_2$phone==0, -1]),
					(data_phone_1[data_phone_1$phone==1, -1]/data_phone_1[data_phone_1$phone==0, -1]))
		return(ratio)
	}

	est <- lapply(boot_imput, estim) %>%
			do.call("rbind", .)

	est_M <- lapply(boot_imput, function(X){
				estim(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F <- lapply(boot_imput, function(X){
				estim(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est_phone <- lapply(boot_imput, estim_ratio) %>%
			do.call("rbind", .)

	est_M_phone <- lapply(boot_imput, function(X){
				estim_ratio(X[X$sex=="M",])
			}) %>%
			do.call("rbind", .)

	est_F_phone <- lapply(boot_imput, function(X){
				estim_ratio(X[X$sex=="F",])
			}) %>%
			do.call("rbind", .)

	est <- est %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M <- est_M %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F <- est_F %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_phone <- est_phone %>%
			dplyr::select(-clinic_cost_travel_25) %>%
			dplyr::select(-d_child1)
	est_M_phone <- est_M_phone %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)
	est_F_phone <- est_F_phone %>%
				dplyr::select(-clinic_cost_travel_25) %>%
				dplyr::select(-d_child1)

	data_fig <- rbind(
					data.frame(var=colnames(est),
						est=apply(est, 2, mean),
						ci_l=apply(est, 2, quantile, prob=0.025),
						ci_up=apply(est, 2, quantile, prob=0.975),
						type="complete",
						group="Whole dataset") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest")),
					data.frame(var=colnames(est_M),
						est=apply(est_M, 2, mean),
						ci_l=apply(est_M, 2, quantile, prob=0.025),
						ci_up=apply(est_M, 2, quantile, prob=0.975),
						type="Men",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest")),
					data.frame(var=colnames(est_F),
						est=apply(est_F, 2, mean),
						ci_l=apply(est_F, 2, quantile, prob=0.025),
						ci_up=apply(est_F, 2, quantile, prob=0.975),
						type="Women",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest"))
					) %>%
					mutate(
						group=factor(group,
							levels = c("Whole dataset", "Gender strata")),
						var=factor(var,
							levels = rev(c("clinic_time","clinic_cost_travel", "clinic_cost_travel_75",
								"clinic_unable", "clinic_time", "clinic_time_25", "clinic_time_75", 
								"d_child0", "d_child1", "dest", "n", "n_adult", "n_child")),
							labels = rev(c("Time to reach a\nhealth care center",
								"Mean travel cost", "75p travel cost",
								"Being unable to access a\nhealth care center",
								"Mean travel time to\na health care centre", "25p travel time to\na health care centre", "75p travel time to\na health care centre",
								"At least 1 deceased child",
								"More than 1 deceased child",
								"Destinations\nreached recently",
								"People living\nin the household",
								"Adults living\nin the household",
								"Children living\nin the household"))))

	data_phone_fig <- rbind(
					data.frame(var=colnames(est_phone),
						est=apply(est_phone, 2, mean),
						ci_l=apply(est_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_phone, 2, quantile, prob=0.975),
						type="complete",
						group="Whole dataset") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest")),
					data.frame(var=colnames(est_M_phone),
						est=apply(est_M_phone, 2, mean),
						ci_l=apply(est_M_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_M_phone, 2, quantile, prob=0.975),
						type="Men",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest")),
					data.frame(var=colnames(est_F_phone),
						est=apply(est_F_phone, 2, mean),
						ci_l=apply(est_F_phone, 2, quantile, prob=0.025),
						ci_up=apply(est_F_phone, 2, quantile, prob=0.975),
						type="Women",
						group="Gender strata") %>%
					dplyr::filter(var %in% c("clinic_cost_travel_75",
						"clinic_cost_travel", "clinic_time_75",
						"clinic_time_25", "clinic_time", "dest"))
					) %>%
					mutate(
						group=factor(group,
							levels = c("Whole dataset", "Gender strata")),
						var=factor(var,
							levels = rev(c("clinic_time","clinic_cost_travel", "clinic_cost_travel_75",
								"clinic_unable", "clinic_time", "clinic_time_25", "clinic_time_75", 
								"d_child0", "d_child1", "dest", "n", "n_adult", "n_child")),
							labels = rev(c("Time to reach a\nhealth care center",
								"Mean travel cost", "75p travel cost",
								"Being unable to access a\nhealth care center",
								"Mean travel time to\na health care centre", "25p travel time to\na health care centre", "75p travel time to\na health care centre",
								"At least 1 deceased child",
								"More than 1 deceased child",
								"Destinations\nreached recently",
								"People living\nin the household",
								"Adults living\nin the household",
								"Children living\nin the household"))))

	note("Plotting...\n")
	figA <- ggplot2::ggplot(
				data_fig %>%
					dplyr::mutate(ref="Reference: All participants") %>%
					dplyr::bind_rows(.,
						data_phone_fig %>%
							dplyr::mutate(ref="Reference: Non owners")),
				ggplot2::aes(x=est, y=var, color=type)) +
				ggplot2::geom_errorbarh(
					ggplot2::aes(xmin=ci_l,
						xmax=ci_up, y=var, color=type),
					position=ggplot2::position_dodge(width=0.5),
					height=0.25) +
				ggplot2::geom_point(
					size=3,
					position=ggplot2::position_dodge(width=0.5)) +
				ggplot2::geom_vline(xintercept = 1, linetype="dashed") +
				ggplot2::scale_color_manual(
					name="",
					breaks=c("complete", "Men", "Women"),
					labels=c("All participants", "Men", "Women"),
					values=c("black", RColorBrewer::brewer.pal(3, "RdBu")[-2])) +
				ggplot2::scale_x_continuous(
					trans="log2") +
				ggplot2::facet_grid(.~ref) +
				# ggplot2::xlim(c(0, 6.6)) +
				# ggplot2::guides(colour="none") +
				ggplot2::xlab("Ratio of the value for people owning a mobile phone\nover the value for the reference") +
				ggplot2::ylab("") +
				ggplot2::theme_bw()
				# ggplot2::theme(legend.position="none")

	ggplot2::ggsave(
		paste0(file, ".tiff"),
		figA,
		width=7.5,
		height=5)
	note("Figure saved...\n")
}

suppl_log_sens <- function(){
	note("Data management...\n")
	data <- net_d_manage(net2016, verbose=FALSE)
	nodes2016 <- unique(data[[2]]) # 214, 215, and 216 have duplicates and 216 hsa different values for phone_own_duration
	nodes2016 <- nodes2016[!(nodes2016$id==216 & nodes2016$phone_own_duration==24),] %>%
				dplyr::filter(!(id==214 & adult_house==4)) %>%
				dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
						ifelse(phone_own>0, "Phone", "No phone")),
					phone_compound=ifelse(is.na(other_phone_own), "No data",
						ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
				dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
						ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
						phone_c2=ifelse(phone=="Phone", "Phone owned",
						ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
							ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
				dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
							ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
				dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
				dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
				dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data"))) %>%
				dplyr::mutate(sex=ifelse(is.na(sex), NA,
					ifelse(sex==2, "F", "M")),
					hsv=ifelse(id %in% samp$id[samp$year==2017], "Positive", "Negative"))

	links2016 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2016$min <- apply(links2016[,1:2],1,min)
	links2016$max <- apply(links2016[,1:2],1,max)
	links2016 <- links2016[links2016$min!=links2016$max,]
	links2016$comb <- paste(links2016$min, links2016$max,sep="-")
	links2016 <- links2016[!duplicated(links2016$comb),c("x","y")]
	links_l2016 <- links2016
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone"]) | (links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]) |
						(links_l2016$y %in% nodes2016$id[nodes2016$phone=="Phone" | is.na(nodes2016$phone_own)]),
							"Phone", "No Phone")
	links_l2016$phone <- ifelse((links_l2016$x %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))) |
						(links_l2016$y %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2016$phone_hh_opt <- ifelse(nodes2016$id %in% unique(c(links_l2016$x[links_l2016$phone=="Phone"],links_l2016$y[links_l2016$phone=="Phone"])) |
							nodes2016$id %in% nodes2016$id[nodes2016$phone=="Phone"],
							"Phone", "No Phone")
	nodes2016$phone_use_ever <- ifelse(nodes2016$phone_use_ever=="No data", NA,
									ifelse(nodes2016$phone_use_ever=="Already used a phone", "Yes", "No"))

	data <- net_d_manage(net2015, verbose=FALSE)
	nodes2015 <- unique(data[[2]])
	nodes2015 <- nodes2015 %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data",
							ifelse(phone_own>0, "Phone", "No phone")),
						phone_compound=ifelse(is.na(other_phone_own), "No data",
							ifelse(other_phone_own>0, "Phone", "No phone"))) %>%
					dplyr::mutate(phone_c=ifelse(phone=="Phone" | phone_compound=="Phone", "Phone",
							ifelse(phone=="No data" | phone_compound=="No data", "No data", "No phone")),
							phone_c2=ifelse(phone=="Phone", "Phone owned",
							ifelse(phone!="Phone" & phone_compound=="Phone", "Phone in\nthe coumpound",
								ifelse(phone=="No data" & phone_compound=="No data", "No data", "No phone")))) %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone))

	links2015 <- setNames(data[[1]][!is.na(data[[1]]$id_link), c("id", "id_link")], c("x", "y"))
	links2015$min <- apply(links2015[,1:2],1,min)
	links2015$max <- apply(links2015[,1:2],1,max)
	links2015 <- links2015[links2015$min!=links2015$max,]
	links2015$comb <- paste(links2015$min, links2015$max,sep="-")
	links2015 <- links2015[!duplicated(links2015$comb),c("x","y")]
	l_list <- unique(c(links2015$x, links2015$y)) %>%
				as.integer()
	temp <- net2015[[2]][, c("id", "age", "sex")] %>%
				dplyr::mutate(id=as.integer(id))
	l_list <- data.frame(id=l_list[!(l_list %in% nodes2015$id)]) %>% # List of ID in the file for children but not in the other ones
				dplyr::left_join(., temp, by="id")
	nodes2015 <- nodes2015 %>%
					dplyr::full_join(., data.frame(id=l_list$id), by="id") %>%
					dplyr::mutate(phone=ifelse(is.na(phone_own), "No data", phone))
	nodes2015[match(l_list$id, nodes2015$id), c("age", "sex")] <- l_list[na.omit(match(nodes2015$id, l_list$id)), c("age", "sex")]
	nodes2015$phone[nodes2015$id %in% c(96:98)] <- "Children"
	nodes2015 <- dplyr::filter(nodes2015, !(is.na(age) & id %in% c(96:98)))

	nodes2015 <-  nodes2015 %>%
					dplyr::mutate(phone=ifelse(age=="<=15" & (is.na(phone) | phone=="No phone" | phone=="No data"), "Children", phone)) %>%
					dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone=factor(phone, levels = c("Phone", "No phone", "Children", "No data")),
						phone_use_ever=ifelse(is.na(phone_use_ever), "No data",
								ifelse(phone_use_ever==0, "Never used a phone", "Already used a phone"))) %>%
					# dplyr::mutate(phone=ifelse(is.na(phone), "No data", phone)) %>%
					dplyr::mutate(phone_use_ever=ifelse(phone=="Phone", "Already used a phone", phone_use_ever)) %>%
					dplyr::mutate(sex=ifelse(is.na(sex), NA,
						ifelse(sex==2, "F", "M")),
						hsv=ifelse(id %in% samp$id[samp$year==2016], "Positive", "Negative"))

	links_l2015 <- links2015
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone"]) | (links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone"]),
						"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]) |
						(links_l2015$y %in% nodes2015$id[nodes2015$phone=="Phone" | is.na(nodes2015$phone_own)]),
							"Phone", "No Phone")
	links_l2015$phone <- ifelse((links_l2015$x %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))) |
						(links_l2015$y %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"]))),
							"Phone", "No Phone")
	nodes2015$phone_hh_opt <- ifelse(nodes2015$id %in% unique(c(links_l2015$x[links_l2015$phone=="Phone"],links_l2015$y[links_l2015$phone=="Phone"])) |
							nodes2015$id %in% nodes2015$id[nodes2015$phone=="Phone"],
							"Phone", "No Phone")
	nodes2015$phone_use_ever <- ifelse(nodes2015$phone_use_ever=="No data", NA,
									ifelse(nodes2015$phone_use_ever=="Already used a phone", "Yes", "No"))

	nodes2015$age <- factor(nodes2015$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2016$age <- factor(nodes2016$age, levels = c("<=15", "16-25", "26-35", "36-45", "46-59", ">=60"))
	nodes2015$data <- ifelse(nodes2015$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2016$data <- ifelse(nodes2016$phone %in% c("Children", "No data"), "Children or no data", "Adult")
	nodes2015$travel <- factor(ifelse(is.na(nodes2015$clinic_travel), NA,
							ifelse(nodes2015$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2016$travel <- factor(ifelse(is.na(nodes2016$clinic_travel), NA,
							ifelse(nodes2016$clinic_travel=="Car", "Car", "Other")),
							levels=c("Other", "Car"))
	nodes2015$d_child0 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>0, 1, 0))
	nodes2015$d_child1 <- ifelse(is.na(nodes2015$deceased_child), NA,
							ifelse(nodes2015$deceased_child>1, 1, 0))
	nodes2016$d_child0 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>0, 1, 0))
	nodes2016$d_child1 <- ifelse(is.na(nodes2016$deceased_child), NA,
							ifelse(nodes2016$deceased_child>1, 1, 0))

	data2015 <- nodes2015[nodes2015$phone!="Children",] %>%
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					# dplyr::mutate(year=2015)
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2015)

	# data2015_red <- nodes2015[nodes2015$phone!="Children",] %>%
	# 				dplyr::filter(!(as.character(id) %in% na.omit(nodes2016$other_id))) %>% # Removing the 8 participants that were already in the 2016 survey
	# 				dplyr::select(phone, phone_use_ever, age, sex, dest, 
	# 					clinic_time, clinic_unable, clinic_cost_travel, travel,
	# 					child_house, adult_house,
	# 					size_house, d_child0, hsv, id) %>%
	# 				# dplyr::mutate(year=2015)
	# 				dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
	# 					hsv=ifelse(hsv=="Positive", 1, 0),
	# 					sex=factor(sex, levels = c("F", "M")),
	# 					age=factor(as.character(age),
	# 						levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
	# 					year=2015)

	data2016 <- nodes2016[nodes2016$phone!="Children" & nodes2016$age_c==0,] %>%
					dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
					dplyr::select(phone, phone_use_ever, age, sex, dest, 
						clinic_time, clinic_unable, clinic_cost_travel, travel,
						child_house, adult_house,
						size_house, d_child0, hsv, id) %>%
					# dplyr::mutate(year=2016)
					dplyr::mutate(phone=ifelse(as.character(phone)=="No data", NA, ifelse(phone=="Phone",1,0)),
						hsv=ifelse(hsv=="Positive", 1, 0),
						sex=factor(sex, levels = c("F", "M")),
						age=factor(as.character(age),
							levels=c("16-25", "26-35", "36-45", "46-59", ">=60")),
						year=2016)

	# data2016_red <- nodes2016[nodes2016$phone!="Children" & nodes2016$age_c==0,] %>%
	# 				# dplyr::filter(!(other_id %in% na.omit(as.character(nodes2015$id)))) %>% # Removing the 8 participants that were already in the 2015 survey
	# 				dplyr::select(phone, phone_use_ever, age, sex, dest, 
	# 					clinic_time, clinic_unable, clinic_cost_travel, travel,
	# 					child_house, adult_house,
	# 					size_house, d_child0, hsv, id) %>%
	# 				dplyr::mutate(year=2016)

	data2015_mis <- data2015 %>%
						dplyr::filter(!is.na(phone))

	data2016_mis <- data2016 %>%
						dplyr::filter(!is.na(phone))

	data_imp <- mice::mice(rbind(data2015, data2016), m=15, seed=150)
	data_imp_mis <- mice::mice(rbind(data2015_mis, data2016_mis), m=15, seed=150)

	model1 <- "phone~age+sex+clinic_unable+adult_house+hsv+year+clinic_time+dest+travel+d_child0"
	model2 <- "phone~age+sex+clinic_unable+adult_house+hsv+clinic_time+dest+travel+d_child0"
	model3 <- "phone~age+sex+clinic_unable+adult_house+year+clinic_time+dest+travel+d_child0"
	model4 <- "phone~age+sex+clinic_unable+adult_house+hsv+clinic_time+dest+d_child0"
	model5 <- "phone~age+sex+clinic_unable+adult_house+year+clinic_time+dest+travel"
	model6 <- "phone~age+sex+clinic_unable+adult_house+clinic_time+dest+d_child0"
	model7 <- "phone~age+clinic_unable+adult_house+clinic_time+dest*sex+d_child0"
	model8 <- "phone~age+clinic_unable+clinic_time+dest+sex+d_child0"
	model9 <- "phone~age+clinic_unable+clinic_time+dest*sex+d_child0"
	model10 <- "phone~age+sex+clinic_time+dest+travel+d_child0"
	model11 <- "phone~age+clinic_unable+adult_house+hsv+year+clinic_time*sex+dest+travel+d_child0"
	model12 <- "phone~age+sex+clinic_unable+adult_house+hsv+year+clinic_time+clinic_time2+dest+dest2+travel+d_child0"
	model13 <- "phone~age+sex+clinic_unable+adult_house+hsv+year+clinic_time+clinic_time2+dest+travel+d_child0"
	model14 <- "phone~age+sex+clinic_unable+adult_house+hsv+year+clinic_time+dest+dest2+travel+d_child0"

	CV <- lapply(1:15, function(x){
		mice::complete(data_imp, x) %>%
			dplyr::mutate(
				dest2=dest^2,
				clinic_time2=clinic_time^2)})

	CV_estimate <- function(data, model=model1){
		estimate <- function(x){
			mod <- stats::glm(stats::formula(model), family = "binomial", data=data[-x,])
			agreement <- data.frame(
							obs=data$phone[x],
							pred=ifelse(stats::predict(mod, newdata=data[x,])>0.5, 1, 0))
			return(agreement)
		}
		CV_est <- lapply(1:nrow(data), estimate) %>%
					do.call("rbind", .) %>%
					dplyr::mutate(accuracy=ifelse(obs==pred, 1, 0))
		return(sum(CV_est$accuracy/nrow(CV_est)))
	}

	agreement <- data.frame(
					model=paste0("Model ", 1:14),
					agreement=NA)

	models <- c(model1, model2, model3,
				model4, model5, model6,
				model7, model8, model9,
				model10, model11, model12,
				model13, model14)

	pb <- txtProgressBar(min = 0, max = 14, style=3) 

	for(i in 1:14){
		agreement[i, 2] <- lapply(CV, function(X){CV_estimate(data=X, model=models[i])}) %>%
							do.call("c", .) %>%
							mean(.)
		setTxtProgressBar(pb,i)
	}

	close(pb)

	CV <- lapply(1:15, function(x){
		mice::complete(data_imp_mis, x) %>%
			dplyr::mutate(
				dest2=dest^2,
				clinic_time2=clinic_time^2)})

	agreement_mis <- data.frame(
					model=paste0("Model ", 1:14),
					agreement=NA)

	pb <- txtProgressBar(min = 0, max = 14, style=3) 

	for(i in 1:14){
		agreement_mis[i, 2] <- lapply(CV, function(X){CV_estimate(data=X, model=models[i])}) %>%
							do.call("c", .) %>%
							mean(.)
		setTxtProgressBar(pb,i)
	}

	close(pb)

	note("Saving results...\n")
	write.csv(
		agreement,
		file.path(path.workspace, "agreement.csv"),
		row.names=FALSE)
	write.csv(
		agreement_mis,
		file.path(path.workspace, "agreement_mis.csv"),
		row.names=FALSE)

}

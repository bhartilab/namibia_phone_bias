#' Creating the love plot for Nita
#'
#' @param file The output_file pathway. The file will be a tiff file.
#' @return A ggplot2 figure.

Nita_loveplot <- function(file=file.path(path.workspace, "love_plot_report.jpg")){
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

	# net16 <- igraph::graph_from_data_frame(d=links2016, vertices=nodes2016, directed=FALSE)
	# igraph::V(net16)$comm <- igraph::membership(igraph::optimal.community(net16))
	# gnet16 <- intergraph::asNetwork(net16)
	# net15 <- igraph::graph_from_data_frame(d=links2015, vertices=nodes2015, directed=FALSE)
	# igraph::V(net15)$comm <- igraph::membership(igraph::optimal.community(net15))
	# gnet15 <- intergraph::asNetwork(net15)

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
	data_red_imp <- mice::mice(rbind(data2015_red, data2016) %>% select(-id), m=15, seed=150)
	data_mis_imp <- mice::mice(rbind(data2015_mis, data2016_mis) %>% select(-id), m=15, seed=150)

	data_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_imp, x)
					temp <- cbind(
						phone=temp$phone,
						age16_25=ifelse(temp$age=="16-25", 1, 0),
						data.frame(stats::model.matrix(phone~age+sex+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	data_ind_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_ind_imp, x)
					temp <- cbind(
						phone=temp$phone,
						age16_25=ifelse(temp$age=="16-25", 1, 0),
						data.frame(stats::model.matrix(phone~age+sex+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	data_red_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_red_imp, x)
					temp <- cbind(
						phone=temp$phone,
						age16_25=ifelse(temp$age=="16-25", 1, 0),
						data.frame(stats::model.matrix(phone~age+sex+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	data_mis_imp <- lapply(1:15, function(x){
					temp <- mice::complete(data_mis_imp, x)
					temp <- cbind(
						phone=temp$phone,
						age16_25=ifelse(temp$age=="16-25", 1, 0),
						data.frame(stats::model.matrix(phone~age+sex+dest+clinic_time+clinic_unable+travel+hsv, data=temp)[,-1])) %>%
					dplyr::mutate(m=x)
					colnames(temp) <- gsub("\\.\\.", "_", colnames(temp))
					colnames(temp) <- gsub("\\.", "_", colnames(temp))
					return(temp)}) %>%
					do.call("rbind", .)

	## love plot using pooled standardized difference
	mean_diff_fact <- data_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	mean_diff_fact_ind <- data_ind_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	mean_diff_fact_red <- data_red_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	mean_diff_fact_mis <- data_mis_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	var_fact <- data_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	var_fact_ind <- data_ind_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	var_fact_red <- data_red_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	var_fact_mis <- data_mis_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25)*(1-mean(age16_25))/dplyr::n(),
						age26_35=mean(age26_35)*(1-mean(age26_35))/dplyr::n(),
						age36_45=mean(age36_45)*(1-mean(age36_45))/dplyr::n(),
						age46_59=mean(age46_59)*(1-mean(age46_59))/dplyr::n(),
						age_60=mean(age_60)*(1-mean(age_60))/dplyr::n(),
						sexM=mean(sexM)*(1-mean(sexM))/dplyr::n(),
						clinic_unable=mean(clinic_unable)*(1-mean(clinic_unable))/dplyr::n(),
						travelCar=mean(travelCar)*(1-mean(travelCar))/dplyr::n(),
						hsv=mean(hsv)*(1-mean(hsv))/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=sum(age16_25),
						age26_35=sum(age26_35),
						age36_45=sum(age36_45),
						age46_59=sum(age46_59),
						age_60=sum(age_60),
						sexM=sum(sexM),
						clinic_unable=sum(clinic_unable),
						travelCar=sum(travelCar),
						hsv=sum(hsv))

	mean_diff_fact[,2:10] <- mean_diff_fact[,2:10]/sqrt(var_fact[,2:10])
	mean_diff_fact_ind[,2:10] <- mean_diff_fact_ind[,2:10]/sqrt(var_fact_ind[,2:10])
	mean_diff_fact_red[,2:10] <- mean_diff_fact_red[,2:10]/sqrt(var_fact_red[,2:10])
	mean_diff_fact_mis[,2:10] <- mean_diff_fact_mis[,2:10]/sqrt(var_fact_mis[,2:10])

	mean_diff_num <- data_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	mean_diff_num_ind <- data_ind_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	mean_diff_num_red <- data_red_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	mean_diff_num_mis <- data_mis_imp[, c(1, 8:9, 13)] %>%
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

	var_num_ind <- data_ind_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	var_num_red <- data_red_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	var_num_mis <- data_mis_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=var(dest)/dplyr::n(),
						clinic_time=var(clinic_time)/dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=sum(dest),
						clinic_time=sum(clinic_time))

	mean_diff_num[,2:3] <- mean_diff_num[,2:3]/sqrt(var_num[,2:3])
	mean_diff_num_ind[,2:3] <- mean_diff_num_ind[,2:3]/sqrt(var_num_ind[,2:3])
	mean_diff_num_red[,2:3] <- mean_diff_num_red[,2:3]/sqrt(var_num_red[,2:3])
	mean_diff_num_mis[,2:3] <- mean_diff_num_mis[,2:3]/sqrt(var_num_mis[,2:3])

	## Pooling difference testing
	z_fact <- data_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	z_fact_ind <- data_ind_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	z_fact_red <- data_red_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=diff(age16_25),
						age26_35=diff(age26_35),
						age36_45=diff(age36_45),
						age46_59=diff(age46_59),
						age_60=diff(age_60),
						sexM=diff(sexM),
						clinic_unable=diff(clinic_unable),
						travelCar=diff(travelCar),
						hsv=diff(hsv))

	z_fact_mis <- data_mis_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
						age26_35=mean(age26_35),
						age36_45=mean(age36_45),
						age46_59=mean(age46_59),
						age_60=mean(age_60),
						sexM=mean(sexM),
						clinic_unable=mean(clinic_unable),
						travelCar=mean(travelCar),
						hsv=mean(hsv)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=diff(age16_25),
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
	between_var_fact_ind <- apply(z_fact_ind[,-1], 2, function(x){
		(1/(max(z_fact_ind$m)-1))*sum((x-mean(x))^2)
	})
	between_var_fact_red <- apply(z_fact_red[,-1], 2, function(x){
		(1/(max(z_fact_red$m)-1))*sum((x-mean(x))^2)
	})
	between_var_fact_mis <- apply(z_fact_mis[,-1], 2, function(x){
		(1/(max(z_fact_mis$m)-1))*sum((x-mean(x))^2)
	})

	se_fact <- data_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
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
					dplyr::mutate(
						age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	se_fact_ind <- data_ind_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
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
					dplyr::mutate(
						age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	se_fact_red <- data_red_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
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
					dplyr::mutate(
						age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
						age26_35=age26_35*(1-age26_35)*((1/n1)+(1/n2)),
						age36_45=age36_45*(1-age36_45)*((1/n1)+(1/n2)),
						age46_59=age46_59*(1-age46_59)*((1/n1)+(1/n2)),
						age_60=age_60*(1-age_60)*((1/n1)+(1/n2)),
						sexM=sexM*(1-sexM)*((1/n1)+(1/n2)),
						clinic_unable=clinic_unable*(1-clinic_unable)*((1/n1)+(1/n2)),
						travelCar=travelCar*(1-travelCar)*((1/n1)+(1/n2)),
						hsv=hsv*(1-hsv)*((1/n1)+(1/n2))) %>%
					dplyr::select(m:hsv)

	se_fact_mis <- data_mis_imp[, c(1:7, 10:13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::mutate(n=dplyr::n()) %>%
					dplyr::group_by(m) %>%
					dplyr::mutate(n1=min(n), n2=max(n)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(
						age16_25=mean(age16_25),
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
					dplyr::mutate(
						age16_25=age16_25*(1-age16_25)*((1/n1)+(1/n2)),
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
	within_var_fact_ind <- apply(se_fact_ind[,-1], 2, mean)
	within_var_fact_red <- apply(se_fact_red[,-1], 2, mean)
	within_var_fact_mis <- apply(se_fact_mis[,-1], 2, mean)

	r_var_inc <- (1+(1/max(z_fact$m)))*between_var_fact/within_var_fact
	r_var_inc_ind <- (1+(1/max(z_fact_ind$m)))*between_var_fact_ind/within_var_fact_ind
	r_var_inc_red <- (1+(1/max(z_fact_red$m)))*between_var_fact_red/within_var_fact_red
	r_var_inc_mis <- (1+(1/max(z_fact_mis$m)))*between_var_fact_mis/within_var_fact_mis

	df_fact <- (max(z_fact$m)-1)*(1+(1/r_var_inc))^2
	df_fact_ind <- (max(z_fact_ind$m)-1)*(1+(1/r_var_inc_ind))^2
	df_fact_red <- (max(z_fact_red$m)-1)*(1+(1/r_var_inc_red))^2
	df_fact_mis <- (max(z_fact_mis$m)-1)*(1+(1/r_var_inc_mis))^2

	# z_fact[,2:9] <- z_fact[,2:9]/sqrt(se_fact[,2:9])

	t_num <- data_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	t_num_ind <- data_ind_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	t_num_red <- data_red_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	t_num_mis <- data_mis_imp[, c(1, 8:9, 13)] %>%
					dplyr::group_by(m, phone) %>%
					dplyr::summarise(dest=mean(dest),
						clinic_time=mean(clinic_time)) %>%
					dplyr::group_by(m) %>%
					dplyr::summarise(dest=diff(dest),
						clinic_time=diff(clinic_time))

	between_var_num <- apply(t_num[,-1], 2, function(x){
		(1/(max(t_num$m)-1))*sum((x-mean(x))^2)
	})
	between_var_num_ind <- apply(t_num_ind[,-1], 2, function(x){
		(1/(max(t_num_ind$m)-1))*sum((x-mean(x))^2)
	})
	between_var_num_red <- apply(t_num_red[,-1], 2, function(x){
		(1/(max(t_num_red$m)-1))*sum((x-mean(x))^2)
	})
	between_var_num_mis <- apply(t_num_mis[,-1], 2, function(x){
		(1/(max(t_num_mis$m)-1))*sum((x-mean(x))^2)
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

	se_num_ind <- data_ind_imp[, c(1, 8:9, 13)] %>%
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

	se_num_red <- data_red_imp[, c(1, 8:9, 13)] %>%
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

	se_num_mis <- data_mis_imp[, c(1, 8:9, 13)] %>%
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
	within_var_num_ind <- apply(se_num_ind[,-1], 2, mean)
	within_var_num_red <- apply(se_num_red[,-1], 2, mean)
	within_var_num_mis <- apply(se_num_mis[,-1], 2, mean)

	r_var_inc <- (1+(1/max(t_num$m)))*between_var_num/within_var_num
	r_var_inc_ind <- (1+(1/max(t_num_ind$m)))*between_var_num_ind/within_var_num_ind
	r_var_inc_red <- (1+(1/max(t_num_red$m)))*between_var_num_red/within_var_num_red
	r_var_inc_mis <- (1+(1/max(t_num_mis$m)))*between_var_num_mis/within_var_num_mis

	df_num <- (max(t_num$m)-1)*(1+(1/r_var_inc))^2
	df_num_ind <- (max(t_num_ind$m)-1)*(1+(1/r_var_inc_ind))^2
	df_num_red <- (max(t_num_red$m)-1)*(1+(1/r_var_inc_red))^2
	df_num_mis <- (max(t_num_mis$m)-1)*(1+(1/r_var_inc_mis))^2

	# t_num[,2:3] <- t_num[,2:3]/sqrt(se_num[,2:3])

	lovep <- data.frame(
				t=apply(cbind(mean_diff_fact_ind, mean_diff_num_ind[,2:3])[,-1], 2, mean),
				var=colnames(cbind(mean_diff_fact_ind, mean_diff_num_ind[,2:3])[,-1]),
				p=apply(cbind(z_fact_ind[,-1], t_num_ind[,-1]), 2, mean)^2) %>%
				dplyr::mutate(p=p/(c(within_var_fact_ind, within_var_num_ind)+(1+(1/max(z_fact_ind$m)))*c(between_var_fact_ind, between_var_num_ind))) %>%
				dplyr::mutate(p=1-pf(abs(p), df1=1, df2=c(df_fact_ind, df_num_ind))) %>%
				dplyr::mutate(
					p=factor(as.integer(cut(p, breaks=c(0, 0.05, 1))),
						levels=1:2, labels=c("p<0.05", "p>0.05")),
					var=factor(var,
						levels = c("age16_25", "age26_35", "age36_45",
							"age46_59","age_60", "sexM", "hsv",
							"clinic_unable", "travelCar", "dest", "clinic_time"),
						labels= c("16-25 years", "26-35 years", "36-45 years",
							"46-59 years","60+ years", "Men", "HSV shedding",
							"Unable to reach a\nhealthcare center",
							"Can travel\nwith a car", "Number of\ndestinations reached",
							"Time to reach a\nhealthcare center\n(hours)"))) %>%
				ggplot2::ggplot(., ggplot2::aes(x=t, y=var, color=t)) +
					ggplot2::geom_point(ggplot2::aes(shape=p), size=2) +
					ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
					ggplot2::scale_color_gradient2(low="blue", mid="grey", high="red",
						midpoint = 0) +
					ggplot2::guides(colour="none", shape=ggplot2::guide_legend(title="")) +
					ggplot2::xlab("Standardized difference") +
					ggplot2::ylab("") +
					ggplot2::theme_bw() +
					ggplot2::theme(legend.position = "bottom")

	ggplot2::ggsave(
		file,
		lovep,
		width=6,
		height=6,
		unit="in")
}

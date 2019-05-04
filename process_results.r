library(dplyr)
library(readr)
library(stringr)
library(randomForest)
library(reshape2)
library(futile.logger)
library(ggplot2)
library(stringr)
library(cetcolor)
library(scales)
library(forcats)

library(tidyr)
library(animation)

# pattern unique to these downloads
directories = dir()[grepl( 'marathon.*.csv$',dir())]
files = file.path(directories, directories)

years = c(2015, 2016, 2017)

load_data<-function(i){read_csv(files[i], col_types=cols(.default='c')) %>% mutate(year=years[i])}

marathon = bind_rows(lapply(1:3, load_data))

#H:M:S to seconds and converters
hms_to_sec <- function(x){
  x <- as.numeric(x)
  3600 * x[1] + 60*x[2] + x[3]
}

to_seconds <- function(x){
  if (is.numeric(x)){
    return(x)
  }
  splits = strsplit(x, ':')
  sapply(splits, hms_to_sec)
}

time_labeller <- function(x, use_hours=TRUE, ...){
  if (use_hours){
    hours = x %/% 3600
    x = x - 3600 * hours
  }
  minutes = x %/% 60
  x = x - 60*minutes
  if (use_hours){
    return(paste(hours, str_pad(minutes, 2, pad='0'), str_pad(x, 2, pad='0'), sep=':'))
  } else {
    return(paste(minutes, str_pad(x, 2, pad='0'), sep=':'))
  }
}

time_labeller_f <- function(x, use_hours=FALSE, ...){
  if (use_hours){
    hours = x %/% 3600
    x = x - 3600 * hours
  }
  minutes = x %/% 60
  x = x - 60*minutes
  if (use_hours){
    return(paste(hours, str_pad(minutes, 2, pad='0'), str_pad(x, 2, pad='0'), sep=':'))
  } else {
    return(paste(minutes, str_pad(x, 2, pad='0'), sep=':'))
  }
}

secs_to_pace <- function(t, dist, units='km'){
  if (units=='km'){
    return(t/(dist/1.609))
  } else {
    return(t/dist)
  }
}

group_age <- function(age){
  case_when(
    between(age, 18, 24) ~ '18-24',
    between(age, 25, 29) ~ '25-29',
    between(age, 30, 34) ~ '30-34',
    between(age, 35, 44) ~ '35-44',
    between(age, 45, 54) ~ '45-54',
    TRUE ~ '55+'
  )
}

calc_split_pace <- function(v1, v2, d1, d2, u1='km', u2='km'){
  if (u1=='km')
    d1 = d1/1.609
  if (u2=='km')
    d2 = d2/1.609
  
  1/((d2-d1)/(d2*v2-d1*v1))
}

marathon = marathon %>% 
  mutate_at(
    vars('5K','10K','15K','20K','Half','25K','30K','35K','40K','Proj Time','Official Time'),
    funs(to_seconds)
  ) %>% mutate(
    pace_5K = secs_to_pace(`5K`, 5),
    pace_10K = secs_to_pace(`10K`, 10),
    pace_15K = secs_to_pace(`15K`, 15), 
    pace_20K = secs_to_pace(`20K`, 20),
    pace_half = secs_to_pace(Half, 13.1, units='mi'),
    pace_25K = secs_to_pace(`25K`, 25),
    pace_30K = secs_to_pace(`30K`, 30),
    pace_35K = secs_to_pace(`35K`, 35),
    pace_40K = secs_to_pace(`40K`, 40),
    pace_official = secs_to_pace(`Official Time`, 26.2, units='mi')
  ) %>% mutate(
    split_pace_0K_5K = pace_5K,
    split_pace_5K_10K = calc_split_pace(pace_5K, pace_10K, 5, 10),
    split_pace_10K_15K = calc_split_pace(pace_10K, pace_15K, 10, 15),
    split_pace_15K_20K = calc_split_pace(pace_15K, pace_20K, 15, 20),
    split_pace_20K_25K = calc_split_pace(pace_20K, pace_25K, 20, 25),
    split_pace_25K_30K = calc_split_pace(pace_25K, pace_30K, 25, 30),
    split_pace_30K_35K = calc_split_pace(pace_30K, pace_35K, 30, 35),
    split_pace_35K_40K = calc_split_pace(pace_35K, pace_40K, 35, 40),
    split_pace_40K_official = calc_split_pace(pace_40K, pace_official, 40, 26.2, u2='mile')
  )

#marathon$Gender = NULL

marathon = marathon %>%
  rename(Gender = `M/F`) 

marathon = marathon %>%
  mutate(age = group_age(as.numeric(Age)))

marathon = marathon %>% mutate_if(is.character, as.factor) 

## markov chain - predict future times from past times

#good_model = step(lm(pace_official ~ pace_5K*gender*age + pace_10K*gender*age + pace_15K*gender*age + 
#                       pace_20K*gender*age + factor(year)*gender*age,
#                     data=marathon), 
#                  k=log(nrow(marathon)))

build_formula <- function(i, interaction=TRUE, demographics=c('age','Gender','year')){
  var_list = paste0('pace_',
                    c('5K','10K','15K','20K','half','25K','30K','35K','40K'))
  
  var_list = var_list[1:i]
  combined_demographics = paste(demographics, collapse='*')
  if (interaction){
    formula_string = paste0(
      'pace_official ~ ',
      paste(
        var_list,
        combined_demographics,
        sep='*',
        collapse='+'
      )
    )
  } else {
    formula_string = paste0(
      'pace_official ~',
      paste(demographics,paste(var_list, collapse='+'), sep='+')
    )
  }  
  
  return(as.formula(formula_string))
}

get_predictors <- function(i){
  var_list = paste0('pace_',
                    c('5K','10K','15K','20K','half','25K','30K','35K','40K'))
  
  var_list = var_list[1:i]
  
  return(c('age','Gender','year', var_list))
}

build_linear_model <- function(i, use_demographics=TRUE){
  flog.info('-LM-')
  flog.info(i)
  if (use_demographics){
    model_formula = build_formula(i)
  } else {
    model_formula = build_formula(i, FALSE, character(0))
  }
  
  pvars = get_predictors(i)
  data = marathon 
  data = data[rowSums(is.na(data[,pvars]))==0,] %>% mutate(year=factor(year))
  data = data[,c('pace_official', pvars)]
  print(plyr::colwise(class)(data))
  
  model = step(lm(model_formula, data=data, na.action=na.exclude), k=log(nrow(data)))
  
  return(model)
}

build_simple_linear_model <- function(i){
  flog.info('-SLM-')
  flog.info(i)
  predictors = get_predictors(i)
  data = marathon
  model_formula = as.formula(sprintf('pace_official ~ %s', predictors[i+3]))
  data = data[rowSums(is.na(data[,predictors]))==0,]
  model = lm(model_formula, data=data)
  return(model)
}

build_rf_model <- function(i){
  flog.info('-RF-')
  flog.info(i)
  model_formula = build_formula(i, interaction=FALSE)
  
  pvars = get_predictors(i)
  data = marathon 
  data = data[rowSums(is.na(data[,pvars]))==0,]  %>% mutate(year=factor(year), age=factor(age), Gender=factor(Gender))
  data = data[,c('pace_official', pvars)]
  
  #print(plyr::colwise(class)(data))
  print(model_formula)
  #print(sum(is.na(unlist(data))))
  model = randomForest(x=data[,pvars], y=data$pace_official, na.action=na.exclude,
                       nodesize=100, ntree=300)
  return(model)
}

distances = c(5,10,15,20,13.1*1.609,25,30,35,40,26.2*1.609)/1.609

names(distances) = paste0('pace_',
                          c('5K','10K','15K','20K','half','25K','30K','35K', '40K','official')
                          )


## random forest isn't ideal for this highly correlated of modelling
#rf_models = list()
#for (i in 1:(length(distances)-1))
  #rf_models[[i]] = build_rf_model(i)

#save()

linear_models = list()
for (i in 1:(length(distances) - 1))
  linear_models[[i]] = build_linear_model(i)

ni_linear_models = list()
for (i in 1:(length(distances) - 1))
  ni_linear_models[[i]] = build_linear_model(i, FALSE)

simple_linear_models = list()
for (i in 1:(length(distances) - 1))
  simple_linear_models[[i]] = build_simple_linear_model(i)

f_tests = lapply(1:8, function(i) anova(ni_linear_models[[i]], simple_linear_models[[i]]))

model_r2 = lapply(1:8, function(i) c(summary(linear_models[[i]])$r.squared,
                         summary(ni_linear_models[[i]])$r.squared, 
                          summary(simple_linear_models[[i]])$r.squared)) %>%
   unlist() %>% matrix(ncol=3, byrow=T) %>%
  as.data.frame() 

names(model_r2) = c('Interaction Model','Linear Model','Simple Linear Model')
model_r2$distance = c(distances[1:4], distances[6:9])
molten_model_r2 = melt(model_r2, id.var='distance')

png('model_r2.png', height=720, width=720, res=100)
print(
ggplot(molten_model_r2) + 
  geom_line(aes(x=distance, y=value, color=variable), size=2) + 
  xlab('Distance in Race (miles)') + ylab('R-squared of Model (% of variation of final pace explained)') + 
  ggtitle('Effectiveness of Linear Models at Predicting Final Pace in Boston Marathon',
          subtitle='Simple Linear Model = uses current pace as predictor \nLinear Model = uses current pace and paces at previous splits as predictors \nInteraction Model = Uses current and all previous split paces, combining 
                                these with demographics as predictors') +
  scale_color_discrete('Model')
)
dev.off()

#save(list=ls(), file='.RData')

#bumps lower end of range if it gets caught at border
fix_range_to_res <- function(rng, res, bump=TRUE){
  p1 = (rng[1] %/% res - (rng[2] %% res == 0)*bump) * res
  p2 = (rng[2] %/% res + 1 - (rng[2] %% res == 0)) * res 
  return(c(p1,p2))
}

pace_cut_labeller <- function(x, labeller=time_labeller){
  xlevs = na.omit(levels(x))
  #print(xlevs)
  x = as.character(x)
  match_obj = str_match(x, '\\((\\d+\\.?\\d*),(\\d+\\.?\\d*)]')
  xlev_match_obj = str_match(xlevs, '\\((\\d+\\.?\\d*),(\\d+\\.?\\d*)]')
  new_levels = sprintf('%s-%s', xlev_match_obj[,2], xlev_match_obj[,3])
  x1 = (as.numeric(match_obj[,2])+1) %>% labeller(use_hours=FALSE)
  x2 = as.numeric(match_obj[,3]) %>% labeller(use_hours=FALSE)
  xl1 = (as.numeric(xlev_match_obj[,2])+1) %>% labeller(use_hours=FALSE)
  xl2 = as.numeric(xlev_match_obj[,3]) %>% labeller(use_hours=FALSE)
  new_levels = sprintf('%s-%s', xl1, xl2)
  #print(new_levels)
  return(factor(sprintf('%s-%s', x1, x2), levels=new_levels))
}

tile_cut_calc <- function(x, res){
  rng= range(x, na.rm=TRUE) %>% fix_range_to_res(res)
  cut(x, breaks=seq(rng[1],rng[2], res), dig.lab=8) %>% pace_cut_labeller()
}

molten_times = melt(marathon %>% select_at(vars(starts_with('pace_'), c('age','Gender','year','Bib'))),
                    id.vars=c('age','Gender','year','Bib'))
molten_times = molten_times %>% 
  mutate(distance = distances[variable])

molten_splits = melt(marathon %>% select_at(vars(starts_with('split_pace_'), c('age','Gender','year','Bib'))),
                     id.vars=c('age','Gender','year','Bib')) 

marathon_cut = marathon %>%
  mutate_at(vars(starts_with('pace_'), starts_with('split_pace_')), tile_cut_calc, res=10)


pairwise_density_plot <- function(data, var1, var2, selected_levels=NA, groupings=character(0), trans='log10', subtitle=''){
  plot_data = data %>%
    group_by_at(vars(var1, var2, groupings)) %>%
    summarize(cnt=n()) %>%
    ungroup() %>%
    mutate(percentage=cnt/nrow(data)) #%>%
    #complete(!!sym(var1), !!sym(var2), fill=list(percentage=0, cnt=0))
  
  overlapping_levels = base::intersect(levels(data[,var1,T]), levels(data[,var2,T]))
  all_levels = base::union(levels(data[,var1,T]), levels(data[,var2,T]))
  if (is.na(selected_levels)){
    selected_levels = all_levels
  }
  
  make_title <- function(var1, var2){
    xl = make_lab(var1)
    yl = make_lab(var2)
    title_lab = sprintf('Boston Marathon %s vs. %s', xl, yl)
    gsub(' ?\\(.+?\\)','', title_lab)
    
  }
  
  make_lab <- function(x){
    x = gsub('pace_(.+)','Pace at \\1 (min./mile)', x)
    gsub('official','End of Race', x)
  }
  
  xlabel = make_lab(var1)
  ylabel = make_lab(var2)
  title_lab = make_title(var1, var2)
  
  ##data[,var1] = factor(data[,var1,T], levels=selected_levels)
  ##data[,var2] = factor(data[,var2,T], levels=selected_levels)
  
  selected_breaks = selected_levels[seq(1,length(selected_levels), by=2)]
  #print(class(overlapping_levels))
  overlap_frame = as.tbl(data.frame(i=1:length(selected_levels)))

  overlap_frame[,var1] = factor(selected_levels, levels=selected_levels)
  overlap_frame[,var2] = factor(selected_levels, levels=selected_levels)
  
  # I will adjust the value a bit if it gets too high, but this doesn't happen too often,
  # so I won't skew the majority of the results for this
  max_pct = max(max(plot_data$percentage) * 10000, 500)
  
  ggplot(plot_data) + geom_tile(aes(x=!!as.name(var1), y=!!as.name(var2), fill=percentage)) + 
    scale_fill_gradientn('Proportion of\nRunners', colors=cet_pal(7, 'inferno'),trans=trans, label=percent, 
                         breaks=c(0.1,0.3,1,3,10,30, 100, 300)/10000, 
                         limits = c(0.05,max_pct)/10000) +
    geom_tile(data=overlap_frame,aes(x=!!as.name(var1), y=!!as.name(var2), color='#11EE3333'), fill=NA) + 
    scale_color_identity() + 
    scale_x_discrete(limits=selected_levels, breaks=selected_breaks) + 
    scale_y_discrete(limits=selected_levels, breaks=selected_breaks) + 
    theme(axis.text.x=element_text(angle=90, hjust=1)) + 
    xlab(xlabel) + ylab(ylabel) + 
    ggtitle(title_lab, subtitle=subtitle)
}


univariate_pairwise_density_plot <- function(data, var1, var2, selected_levels=NA, groupings=character(0), trans='log10', subtitle=''){
  plot_data1 = data %>%
    group_by_at(vars(var1, groupings)) %>%
    summarize(cnt=n()) %>%
    ungroup() %>%
    mutate(percentage=cnt/nrow(data), split_var=var1, pace=!!as.name(var1))
  
  plot_data2 = data %>%
    group_by_at(vars(var2, groupings)) %>%
    summarize(cnt=n()) %>%
    ungroup() %>%
    mutate(percentage=cnt/nrow(data), split_var=var2, pace=!!as.name(var2))
  
  plot_data = bind_rows(plot_data1,plot_data2)
  
  overlapping_levels = base::intersect(levels(data[,var1,T]), levels(data[,var2,T]))
  all_levels = base::union(levels(data[,var1,T]), levels(data[,var2,T]))
  if (is.na(selected_levels)){
    selected_levels = all_levels
  }
  
  make_title <- function(var1, var2){
    xl = make_lab(var1)
    yl = make_lab(var2)
    title_lab = sprintf('Boston Marathon Pace Distribution at %s and %s', xl, yl)
    gsub(' ?\\(.+?\\)','', title_lab)
    
  }
  
  make_lab <- function(x){
    x = gsub('pace_(.+)','Pace at \\1 (min./mile)', x)
    str_pad(gsub('official','End of Race', x), width=12)
  }
  
  xlabel = make_lab(var1)
  ylabel = make_lab(var2)
  title_lab = make_title(var1, var2)
  
  
  selected_breaks = selected_levels[seq(1,length(selected_levels), by=2)]
  #print(class(overlapping_levels))
  
  # I will adjust the value a bit if it gets too high, but this doesn't happen too often,
  # so I won't skew the majority of the results for this
  #max_pct = max(max(plot_data$percentage) * 10000, 500)
  
  ggplot(plot_data) + geom_bar(aes(x=pace, y=percentage, fill=split_var), stat='identity', position='dodge') + 
    scale_fill_manual(values=c('#33EE66','#EE4444')) +
    #scale_fill_gradientn('Proportion of\nRunners', colors=cet_pal(7, 'inferno'), label=percent) +
    #                     #breaks=c(0.1,0.3,1,3,10,30, 100, 300)/10000, 
    #                     #limits = c(0.05,max_pct)/10000) +
    #scale_color_identity() + 
    scale_x_discrete(limits=selected_levels, breaks=selected_breaks) + 
    theme(axis.text.x=element_text(angle=90, hjust=1)) + 
    xlab('Pace') + ylab('Proportion') + 
    ggtitle(title_lab, subtitle=subtitle)
}

# requires marathon, not marathon_cut, as input
smooth_univariate_pairwise_density_plot <- function(data, var1, var2, selected_levels=NA, groupings=character(0), trans='log10', subtitle=''){
 
  

  #print(dim(data))
  overlapping_levels = base::intersect(levels(data[,var1,T]), levels(data[,var2,T]))
  #all_levels = base::union(levels(data[,var1,T]), levels(data[,var2,T]))
  if (is.na(selected_levels)){
    stop('NEED TO SPECIFY LEVELS FOR BREAKS')
  }
  
  make_title <- function(var1, var2){
    xl = make_lab(var1)
    yl = make_lab(var2)
    title_lab = sprintf('Boston Marathon Pace Distribution at %s and %s', xl, yl)
    gsub(' ?\\(.+?\\)','', title_lab)
    
  }
  
  make_lab <- function(x){
    x = gsub('pace_(.+)','Pace at \\1', x)
    gsub('official','Finish', x)
  }
  
  xlabel = make_lab(var1)
  ylabel = make_lab(var2)
  title_lab = make_title(var1, var2)
  
  plot_data1 = data[,var1,] 
  names(plot_data1) = 'pace'
  plot_data1$split_var = xlabel
  plot_data2 = data[,var2,]
  names(plot_data2) = 'pace'
  plot_data2$split_var = ylabel
  
  
  
  #print(dim(plot_data1))
  
  
  plot_data = bind_rows(
    #data %>% transmute(pace=(!!as.name(var1)), split_var=xlabel),
    #data %>% transmute(pace=(!!as.name(var2)), split_var=ylabel)
    plot_data1,
    plot_data2
  ) %>%
   filter(
    pace < tail(selected_levels, 1)[1]
  )
  
  #print(tail(selected_levels, 1)[1])
  
  
  selected_breaks = selected_levels[seq(1,length(selected_levels), by=2)]
  #print(class(overlapping_levels))
  
  # I will adjust the value a bit if it gets too high, but this doesn't happen too often,
  # so I won't skew the majority of the results for this
  #max_pct = max(max(plot_data$percentage) * 10000, 500)
  
  plot_data$split_var = factor(as.character(plot_data$split_var), levels=c(xlabel, ylabel))
  
  ggplot(plot_data) + geom_density(aes(x=pace, fill=split_var), alpha=0.5) + 
    scale_fill_manual('Split', values=c('#3366EE','#EE4444')) +
    #scale_fill_gradientn('Proportion of\nRunners', colors=cet_pal(7, 'inferno'), label=percent) +
    #                     #breaks=c(0.1,0.3,1,3,10,30, 100, 300)/10000, 
    #                     #limits = c(0.05,max_pct)/10000) +
    #scale_color_identity() + 
    scale_x_continuous(breaks=selected_breaks, label=time_labeller_f, limits=range(selected_levels)) + 
    theme(axis.text.x=element_text(angle=90, hjust=1)) + 
    xlab('Pace (min./mile)') + ylab('Proportion') + 
    ggtitle(title_lab, subtitle=subtitle)
}

SELECTED_LEVELS = levels(marathon_cut$pace_5K)[1:which(levels(marathon_cut$pace_5K)=='14:51-15:00')]

SELECTED_PACE_COLS = names(distances)[names(distances) != 'pace_half']

png('overall_2d.png', width=840, height=840, res=100)
print(
pairwise_density_plot(marathon_cut, 'pace_5K','pace_official', selected_levels=SELECTED_LEVELS, trans='log10', subtitle='Data from 2015-2017')
)
dev.off()

univariate_pairwise_density_plot(marathon_cut, 'pace_5K','pace_official', selected_levels=SELECTED_LEVELS, trans='log10')

png('overall_1d.png', width=840, height=420, res=100)
print(
smooth_univariate_pairwise_density_plot(marathon, 'pace_5K','pace_official', selected_levels=seq(270,900,by=10), trans='log10', subtitle='Data from 2015-2017')
)
dev.off()

plot_mutations <- function(data, start_i, end_i=length(SELECTED_PACE_COLS), subtitle=''){
  lapply((start_i+1):end_i, function(i) pairwise_density_plot(data, 
                                                              SELECTED_PACE_COLS[start_i], 
                                                              SELECTED_PACE_COLS[i], 
                                                              selected_levels=SELECTED_LEVELS,
                                                              subtitle=subtitle))
}

plot_smooth_mutations <- function(data, start_i, end_i=length(SELECTED_PACE_COLS), subtitle='', SELECTED_PACES=seq(270,900, by=10)){
  lapply((start_i+1):end_i, function(i) smooth_univariate_pairwise_density_plot(data, 
                                                              SELECTED_PACE_COLS[start_i], 
                                                              SELECTED_PACE_COLS[i], 
                                                              selected_levels=SELECTED_PACES,
                                                              subtitle=subtitle))
}
application_indices = 1:(length(SELECTED_PACE_COLS) - 1)

# example
mutations_1 = plot_mutations(marathon_cut, 1)
saveGIF(
  { for (i in 1:length(mutations_1)){print(mutations_1[[i]])}},
  movie.name='mutations_5K.gif',img.name='pace_plot'
)


## overall
lapply(application_indices, 
       function(i){
         print(i)
         mutations <<- plot_mutations(marathon_cut, i, subtitle='Data from 2015-2017')
         saveGIF(
           { for (i in 1:length(mutations)){print(mutations[[i]])}},
           movie.name=sprintf('gif/mutations_%s.gif',i),img.name='pace_plot',
           autoplay=FALSE, autobrowse=FALSE
         )
       })

## sex
genders = c('F','M')#levels(marathon_cut$Gender)

gender_labels = c('M'='Men','F'='Women')

for (g in genders){
  print(g)
  lapply(application_indices, 
         function(i){
           print(i)
           mutations <<- plot_mutations(marathon_cut %>% filter(Gender==g), i, 
                                        subtitle=sprintf('%s only; Data from 2015-2017', gender_labels[g]))
           saveGIF(
             { for (i in 1:length(mutations)){print(mutations[[i]])}},
             movie.name=sprintf('mutations_gender_%s_%s.gif',g, i),img.name='pace_plot',
             autoplay=FALSE, autobrowse=FALSE
           )
         })
}


## age groups
ages = levels(marathon_cut$age)

for (a in ages){
  print(a)
  lapply(application_indices, 
         function(i){
           print(i)
           mutations <<- plot_mutations(marathon_cut %>% filter(age==a), i, subtitle=sprintf('Ages %s only; Data from 2015-2017', a))
           saveGIF(
             { for (i in 1:length(mutations)){print(mutations[[i]])}},
             movie.name=sprintf('mutations_age_%s_%s.gif',a, i),img.name='pace_plot',
             autoplay=FALSE, autobrowse=FALSE
           )
         })
}

## sex + age groups
for (a in ages){
  print(a)
  for (g in genders){
    print(g)
    lapply(application_indices, 
           function(i){
             print(i)
             mutations <<- plot_mutations(marathon_cut %>% filter(Gender==g, age==a), i, 
                                          subtitle=sprintf('%s ages %s only; Data from 2015-2017', 
                                                           gender_labels[g], a))
             saveGIF(
               { for (i in 1:length(mutations)){print(mutations[[i]])}},
               movie.name=sprintf('mutations_gender_age_%s_%s_%s.gif',g, a, i),img.name='pace_plot',
               autoplay=FALSE, autobrowse=FALSE
             )
           })
  }
}

years = c(2015,2016,2017)

print('--2D--')
for (a in c(ages, 'all')){
  print(a)
  for (g in c(genders, 'all')){
    print(g)
    for (y in c(years,'all')){
      print(y)
      lapply(application_indices, 
             function(i){
               print(i)
               subdata = marathon_cut
               if (a != 'all')
                 subdata = subdata %>% filter(age==a)
               if (g != 'all')
                 subdata = subdata %>% filter(Gender==g)
               if (y != 'all')
                 subdata = subdata %>% filter(year==y)
               
               if (g == 'all'){
                 if (a=='all'){
                   participant_portion = ''
                 } else {
                   participant_portion = sprintf('Ages %s only; ', a)
                 }
               } else {
                 if (a=='all'){
                   participant_portion = sprintf('%s only; ', gender_labels[g])
                 } else {
                   participant_portion = sprintf('%s %s only; ', gender_labels[g], a)
                 }
               }
               if (y=='all'){
                 year_portion='Data from 2015-2017'
               } else {
                 year_portion=sprintf('Data from %s', y)
               }
               
               size_portion = sprintf('; Total of %s runners', nrow(subdata))
               mutations <<- plot_mutations(subdata, i, 
                                            subtitle=paste0(participant_portion, year_portion, size_portion)
               )
               
               saveGIF(
                 { for (i in 1:length(mutations)){print(mutations[[i]])}},
                 movie.name=sprintf('mutations_genderageyear_%s_%s_%s_%s.gif',g, a, y, i),img.name='pace_plot',
                 autoplay=FALSE, autobrowse=FALSE
               )
             })
      }
    }
}

# smooth plot
print('--SMOOTH--')
for (a in c(ages, 'all')){
  print(a)
  for (g in c(genders, 'all')){
    print(g)
    for (y in c(years,'all')){
      print(y)
      lapply(application_indices, 
             function(i){
               print(i)
               subdata = marathon
               if (a != 'all')
                 subdata = subdata %>% filter(age==a)
               if (g != 'all')
                 subdata = subdata %>% filter(Gender==g)
               if (y != 'all')
                 subdata = subdata %>% filter(year==y)
               
               if (g == 'all'){
                 if (a=='all'){
                   participant_portion = ''
                 } else {
                   participant_portion = sprintf('Ages %s only; ', a)
                 }
               } else {
                 if (a=='all'){
                   participant_portion = sprintf('%s only; ', gender_labels[g])
                 } else {
                   participant_portion = sprintf('%s %s only; ', gender_labels[g], a)
                 }
               }
               if (y=='all'){
                 year_portion='Data from 2015-2017'
               } else {
                 year_portion=sprintf('Data from %s', y)
               }
               
               lower_limit = min(subdata[,SELECTED_PACE_COLS], na.rm=TRUE)
               lower_limit = (lower_limit %/% 10) * 10
               upper_limit = max(subdata[,SELECTED_PACE_COLS], na.rm=TRUE)
               upper_limit = min(upper_limit, 60*15)
               upper_limit = ceiling(upper_limit / 10) * 10
               
               SELECTED_PACES = seq(lower_limit, upper_limit, by=10)
               #print(SELECTED_PACES)
               #print(c(lower_limit, upper_limit))
               
               size_portion = sprintf('; Total of %s runners', nrow(subdata))
               mutations <<- plot_smooth_mutations(subdata, i, 
                                            subtitle=paste0(participant_portion, year_portion, size_portion
                                                            ), SELECTED_PACES=SELECTED_PACES
               )
               
               saveGIF(
                 { for (i in 1:length(mutations)){print(mutations[[i]])}},
                 movie.name=sprintf('mutations_smooth_genderageyear_%s_%s_%s_%s.gif',g, a, y, i),img.name='pace_plot_smooth',
                 autoplay=FALSE, autobrowse=FALSE, ani.height=280
               )
             })
    }
  }
}

# look at top 6 runners from each year for both genders
top_genders = marathon %>%
  group_by(year, Gender) %>%
  mutate(gender_rank = rank(`Official Time`)) %>%
  filter(gender_rank <= 6) %>%
  ungroup() %>%
  select_at(vars('year','Gender','gender_rank', starts_with('pace_')))
  
pace_molten = melt(top_genders, id.vars=c('year','Gender','gender_rank')) %>% 
  mutate(gender_rank = factor(gender_rank))
pace_molten$distance = distances[pace_molten$variable]

png('top_paces.png', width=720, height=720, res=100)
print(
ggplot(pace_molten %>% mutate(Gender=plyr::mapvalues(Gender, from=c('M','F'), to=c('Men','Women')))) + 
  geom_line(aes(x=distance, y=value, color=gender_rank), size=1.1) + 
  facet_grid(Gender~year, scales='free_y') + 
  scale_y_continuous(label=time_labeller_f) + 
  xlab('Distance (miles)') + 
  ylab('Average Pace (min./mile)') + 
  scale_color_discrete('Rank') + 
  ggtitle('Average Paces of Top 6 Runners in Boston Marathon', subtitle='Data from 2015, 2016, and 2017')
)
dev.off()


## pace density chart at various distances
pace_genders = marathon %>%
  group_by(year, Gender) %>%
  mutate(gender_rank = rank(`Official Time`)) %>%
  ungroup() %>%
  select_at(vars('year','Gender','gender_rank', starts_with('pace_')))

pace_molten_all = melt(pace_genders, id.vars=c('year','Gender','gender_rank')) %>% 
  mutate(gender_rank = factor(gender_rank))
pace_molten_all$distance = distances[pace_molten_all$variable]

proper_distances = gsub('official','Overall',gsub('pace_','',names(distances)))
names(proper_distances) = names(distances)

pace_molten_all$proper_distance = proper_distances[pace_molten_all$variable]

make_overall_density_plot <- function(max_i){
  data = pace_molten_all %>% mutate(Gender=plyr::mapvalues(Gender, from=c('M','F'), to=c('Men','Women')),
                                    proper_distance=as_factor(as.character(proper_distance, levels=proper_distances))) %>% filter(proper_distance != 'half', value <= 900)
  data = data %>% filter(proper_distance %in% proper_distances[1:max_i])
  ggplot(data) + 
    geom_density(aes(x=value, fill=proper_distance), alpha=0.3) + 
    facet_grid(Gender ~ year, scales='free_y') +
    scale_x_continuous('Pace (min./mile)', label=time_labeller_f, breaks=5:15*60) + 
    ggtitle('Paces at Splits for Boston Marathon by Gender and Year', subtitle='red dashed lines indicate paces corresponding to 3, 4, 5, and 6-hour finish times') + 
    geom_vline(xintercept=3:6*3600/26.2, lty='dashed', color='red', alpha=0.9) +
    geom_vline(xintercept=60*5:15, lty='solid', color='#333333', alpha=0.2) + 
    #geom_vline(xintercept=(1800+2:6*3600)/26.2, lty='dotted', color='yellow', alpha=0.8) + 
    theme_bw() + 
    scale_fill_manual('Split Distance', values=cet_pal(9)[1:max_i]) + 
    theme(axis.text.x = element_text(angle=90))
}

overall_density_plots = lapply(1:9, make_overall_density_plot)

saveGIF(
  { for (i in 1:length(overall_density_plots)){print(overall_density_plots[[i]])}},
  movie.name=sprintf('overall_density_plot.gif'),img.name='overall_density',
  autoplay=FALSE, autobrowse=FALSE, ani.height=640, ani.width=840
)

## just test code here
str_match(c('(1,4]'),'\\((\\d+\\.?\\d*),(\\d+\\.?\\d*)]')


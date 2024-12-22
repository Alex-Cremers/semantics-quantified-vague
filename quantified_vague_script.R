library(tidyverse)
options(tibble.width = Inf)
library(XML) # to read degree labels


# Standard error function for graphs
se <- function(x){sd(x,na.rm=T)/sqrt(length(x[!is.na(x)]))}

# Color palettes for graphs:
TwoColorPalette = c(rgb(.5,0,.7),rgb(.9,.65,0))
FourColorPalette = c("#F5793A","#A95AA1","#85C0F9","#0F2080")
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#FF00FF","#808080")

decode <- function(x) {
  if(x==""){return("")} else {
    xmlValue(getNodeSet(htmlParse(x, asText = TRUE,encoding="UTF8"), "//p|//span")[[1]])
  }
}


qv_data <- read_csv('quantified_vague_anonymous_data.csv')

# Process the information about the acceptability of the adjective separately
degree_data <- qv_data %>%
  filter(Condition=="Tr") %>%
  select(Subject,itemID,DegreeOrder,starts_with("adj")) %>%
  select(-adjective) %>%
  separate(DegreeOrder,into=paste0("deg",1:8),sep=1:7) %>%
  rename(
    adj1=adjFirst,
    adj2=adjSecond,
    adj3=adjThird,
    adj4=adjFourth,
    adj5=adjFifth,
    adj6=adjSixth,
    adj7=adjSeventh,
    adj8=adjEighth
  ) %>%
  pivot_longer(starts_with(c("adj","deg")),names_to = c(".value","DegreePosition"),names_sep = 3) %>%
  select(-DegreePosition) %>%
  pivot_wider(names_from = deg,values_from = adj) %>%
  select(Subject,itemID,LETTERS[1:8])

# Put everything back together
qv_data <- qv_data %>%
  left_join(degree_data) %>%
  select(-starts_with("adj"),adjective)



######################
# Remove participants
######################

qv_data %>%
  summarize(n_distinct(Subject))

# Remove participants who failed both true and false control:
excluded_subjects <- qv_data %>%
  filter(Condition%in%c("Tr","Fa")) %>%
  mutate(error = (Condition=="Tr"&Answer<50)|(Condition=="Fa"&Answer>50)) %>%
  group_by(Subject) %>%
  summarize(error_rate=mean(error)) %>%
  filter(error_rate==1) %>%
  pull(Subject)

length(excluded_subjects)

# Remove participants who didn't touch more than half of the sliders for adjective ratings:
untouched_sliders <- qv_data %>%
  filter(Condition=="Tr") %>%
  mutate(across(LETTERS[1:8],~(.x==50))) %>%
  mutate(number_untouched=A+B+C+D+E+F+G+H) %>%
  filter(number_untouched>4) %>%
  pull(Subject)

excluded_subjects <- union(excluded_subjects,untouched_sliders)


length(excluded_subjects)



#########
# Graphs
#########


# Quick graph of adjective acceptability:
context_info <- read_csv("Contexts.csv") %>%
  rename(adjective=ContextName,
         items = ItemNames) %>%
  mutate(across(DegNumA:DegNumC,~as.numeric(sub("−","-",.x))))


# Export latex table with adj info:
pred_info <- qv_data %>%
  select(adjective, Predicate) %>%
  distinct() %>%
  rename(predicate = Predicate)

context_info %>%
  select(adjective, items, dimension) %>%
  left_join(pred_info, by = "adjective") %>%
  xtable::xtable() %>%
  print(booktabs = TRUE, include.rownames = FALSE)

adjective_list <- context_info$adjective

degree_plot_data <- qv_data %>%
  filter(Condition=="Tr") %>%
  select(Subject,adjective,LETTERS[1:8]) %>%
  rename_with(~paste0("Acceptability",.x),any_of(LETTERS[1:8])) %>%
  left_join(select(context_info,c(adjective,starts_with("DegNum")))) %>%
  pivot_longer(-c(Subject,adjective),names_to = c(".value","Degree"),names_sep = -1) %>%
  mutate(adjective=factor(adjective,levels=adjective_list)) %>%
  group_by(adjective,DegNum) %>%
  summarize(mean_acceptability=mean(Acceptability),SE=se(Acceptability),) %>%
  ungroup()

# Simple ribbon plot:
pdf("Adjective_acceptability.pdf",width=6,height=6)
degree_plot_data %>%
  ggplot(aes(x=DegNum,y=mean_acceptability,ymin=mean_acceptability-SE,ymax=mean_acceptability+SE,group=adjective)) +
  facet_wrap(.~adjective,scales = "free_x")+
  geom_line(col="blue")+
  geom_ribbon(col="blue",fill=rgb(0,0,1,0.2))+
  theme_bw()+
  ylab("Acceptability")+
  xlab("Degree")
dev.off()



##########################################
# Fancy graph of adjective acceptability
##########################################

# Format numeric degrees to merge with main data:
numeric_degrees <- context_info %>%
  select(adjective,DegNumA:DegNumH) %>%
  mutate(across(starts_with("DegNum"), function(x){
    x %>%
      as.character() %>%
      str_replace("−", "-") %>%
      as.numeric()
  })) %>%
  pivot_longer(
    DegNumA:DegNumH,
    names_to = "Probe",
    values_to = "Degree"
  ) %>%
  rename(Adjective=adjective) %>%
  mutate(Probe = str_replace(Probe, "DegNum", "")) %>%
  mutate(Degree = if_else(Adjective == "late", Degree-30, Degree))

probes <- toupper(letters[1:8])
names(probes) <- paste0("Deg",1:8)

degree_labels <- context_info %>%
  select(adjective,matches("Deg\\d")) %>%
  pivot_longer(
    starts_with("Deg"),
    names_to = "Probe",
    values_to = "DegreeLabel"
  ) %>%
  rename(Adjective=adjective) %>%
  mutate(DegreeLabel=if_else(is.na(DegreeLabel),"",DegreeLabel),
         DecodedLabel = as.character(sapply(DegreeLabel,decode)),
         Probe = probes[Probe]
  )
rm(probes)

plot_data <- qv_data  %>%
  select(Subject, A:H, adjective) %>%
  group_by(Subject) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_longer(cols = A:H, names_to = "Probe", values_to = "Rating") %>%
  rename(Adjective = adjective) %>%
  left_join(select(degree_labels,-DegreeLabel), by = c("Adjective", "Probe")) %>%
  mutate(Adjective = factor(Adjective,
                            levels=c("tall","powerful","hot","expensive","large","young","late","spicy","profitable","complete","safe","full")),
         DecodedLabel = gsub(" ","",DecodedLabel)
  )

plot_data <- plot_data %>% 
  left_join(numeric_degrees, by = c("Adjective", "Probe")) %>%
  mutate(
    Adjective = factor(Adjective,
                       levels=c("tall","powerful","hot","expensive","large","young","late","spicy","profitable","complete","safe","full")),
    AdjMonotonicity = if_else(Adjective%in%c("young","safe","full"),"Negative","Positive"),
    AdjRating=Rating/100,
    pol_degree = if_else(AdjMonotonicity=="Positive",Degree,-Degree)
  ) %>%
  group_by(Adjective) %>%
  mutate(
    mean_pol_degree=mean(pol_degree),
    sd_degree=sd(pol_degree),
    pol_norm_degree=(pol_degree-mean_pol_degree)/sd_degree,
    norm_degree=if_else(AdjMonotonicity=="Positive",pol_norm_degree,-pol_norm_degree)) %>%
  ungroup()


breaks_fun <- function(x) {
  count_breaks <<- count_breaks + 0.5
  switch(
    floor(count_breaks),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="tall"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="powerful"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="hot"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="expensive"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="large"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="young"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="late"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="spicy"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="profitable"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="complete"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="safe"])),
    sort(unique(plot_data$pol_norm_degree[plot_data$Adjective=="full"]))
  )
}

profitable_labels <- unique(plot_data$DecodedLabel[plot_data$Adjective=="profitable"][order(plot_data$pol_norm_degree[plot_data$Adjective=="profitable"])])
profitable_labels[5] <- ""
#profitable_labels[2] <- paste0(profitable_labels[2],"\n")
full_labels <- unique(plot_data$DecodedLabel[plot_data$Adjective=="full"][order(plot_data$pol_norm_degree[plot_data$Adjective=="full"])])
#full_labels[1] <- paste0(full_labels[1],"\n")
# full_labels[4] <- paste0(full_labels[4],"\n   ")
full_labels[7] <- paste0(full_labels[7], "   ")

spicy_scale <- c("","\uD83C\uDF36","\uD83C\uDF36\uD83C\uDF36","\uD83C\uDF36\uD83C\uDF36\uD83C\uDF36","\uD83C\uDF36\uD83C\uDF36\uD83C\uDF36\uD83C\uDF36")

labels_fun <- function(x) {
  count_label <<- count_label + 1L
  switch(
    count_label,
    unique(plot_data$DecodedLabel[plot_data$Adjective=="tall"][order(plot_data$pol_norm_degree[plot_data$Adjective=="tall"])]),
    unique(plot_data$DecodedLabel[plot_data$Adjective=="powerful"][order(plot_data$pol_norm_degree[plot_data$Adjective=="powerful"])]),
    unique(plot_data$DecodedLabel[plot_data$Adjective=="hot"][order(plot_data$pol_norm_degree[plot_data$Adjective=="hot"])]),
    unique(plot_data$DecodedLabel[plot_data$Adjective=="expensive"][order(plot_data$pol_norm_degree[plot_data$Adjective=="expensive"])]),
    unique(plot_data$DecodedLabel[plot_data$Adjective=="large"][order(plot_data$pol_norm_degree[plot_data$Adjective=="large"])]),
    unique(plot_data$DecodedLabel[plot_data$Adjective=="young"][order(plot_data$pol_norm_degree[plot_data$Adjective=="young"])]),
    unique(plot_data$DecodedLabel[plot_data$Adjective=="late"][order(plot_data$pol_norm_degree[plot_data$Adjective=="late"])]),
    spicy_scale,
    profitable_labels,
    unique(plot_data$DecodedLabel[plot_data$Adjective=="complete"][order(plot_data$pol_norm_degree[plot_data$Adjective=="complete"])]),
    unique(plot_data$DecodedLabel[plot_data$Adjective=="safe"][order(plot_data$pol_norm_degree[plot_data$Adjective=="safe"])]),
    full_labels
  )
}

zero_data <- plot_data %>% 
  group_by(Adjective) %>% 
  summarize(zero=first(case_when(
    Adjective=="complete" ~ (100-mean_pol_degree)/sd_degree,
    T ~ -mean_pol_degree/sd_degree
  ))) %>%
  mutate(zero = if_else(Adjective %in% c("tall","powerful","hot","expensive","large","young"),NaN,zero))



# Fancy graph:
quartz(width=9,height=8)
# quartz(width=9,height=4)
count_breaks <- 1
count_label <- 0
figure1 <- plot_data %>%
  ggplot(aes(y=AdjRating,x=pol_norm_degree,group=Probe)) +
  facet_wrap(~Adjective,scales = "free_x",dir = "h",ncol=3) +
  # facet_wrap(~Adjective,scales = "free_x",dir = "h",ncol=6) +
  geom_boxplot(outlier.shape = NA,width=.2,col=TwoColorPalette[1]) +
  geom_vline(data=zero_data,aes(xintercept = zero),linetype=2,color="red")+
  geom_smooth(aes(group = NULL), col=TwoColorPalette[2], fill=TwoColorPalette[2]) +
  theme_bw()+
  scale_x_continuous(name="Measure",breaks = breaks_fun, minor_breaks = NULL,labels=labels_fun, limits = c(NA, NA))+
  scale_y_continuous(name="Adjective acceptability",labels=scales::percent)+
  # theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5,size=6))+
  theme(axis.text.x = element_text(angle=60,hjust=1,vjust=1,size=6))+
  theme(text = element_text(family = "Arial Unicode MS"))
figure1
ps = gridSVG::grid.export("Graphs/adjective_acceptability_tall.svg", addClass=T,progress = T)
# ps = gridSVG::grid.export("Graphs/adjective_acceptability_wide.svg", addClass=T,progress = T)


####################################
# Define the fuzzy logic prediction
####################################


# Extract prediction from Zadeh fuzzy logic
qv_data <- qv_data %>%
  mutate(
    Predictor = case_when(
      Expression=="Num2" ~ str_sub(Restrictor,-2,-2),
      Expression=="Num3" ~ str_sub(Restrictor,-3,-3),
      Expression%in%c("Conj2","Conj3","All") ~ str_sub(Restrictor,1,1),
      Expression%in%c("Some","None") ~ str_sub(Restrictor,-1,-1),
    )) %>%
  rowwise() %>%
  mutate(FuzzyPrediction = get(`Predictor`)) %>%
  ungroup() %>%
  mutate(FuzzyPrediction=if_else(Expression=="None",100-FuzzyPrediction,FuzzyPrediction))



# Extract prediction from Lukasiewicz fuzzy logic
list_args <- Vectorize( function(a,b) c( as.list(a), as.list(b) ), 
                        SIMPLIFY = FALSE)
make_args_mtx <- function( alist ) {
  Reduce(function(x, y) outer(x, y, list_args), alist)
}
multi.outer <- function(f, ... ) {
  args <- make_args_mtx(list(...))
  apply(args, 1:length(dim(args)), function(a) do.call(f, a[[1]] ) )
}

Luk_two <- function(x){
  M <- outer(x, x, FUN = function(x, y){pmax(0, x+y-1)})
  M <- M*upper.tri(M)
  min(1, sum(M))
}
Luk_three <- function(x){
  # M <- outer(x, x, FUN = function(x, y){pmax(0, x+y-1)})
  # M <- M*upper.tri(M)
  # A <- outer(M, x, FUN = function(x, y){pmax(0, x+y-1)})
  tri_conj <- function(x, y, z)pmax(0, x+y+z-2)
  A <- multi.outer(tri_conj, x, x, x)
  keep_A <- multi.outer(function(i,j,k)(i<j&j<k), seq_along(x), seq_along(x), seq_along(x))
  min(1, sum(keep_A*A))
}

extract_ratings_from_restrictor <- function(Restrictor, colA, colB, colC, colD, colE, colF, colG, colH) {
  all_cols <- c(colA, colB, colC, colD, colE, colF, colG, colH)
  selector <- str_detect(str_split(Restrictor, "", simplify = TRUE) %>% paste(collapse = "|"), LETTERS[1:8])
  all_cols[selector]
}
predict_Lukasiewicz <- function(Expression, Restrictor, colA, colB, colC, colD, colE, colF, colG, colH) {
  restrictor_ratings <- extract_ratings_from_restrictor(Restrictor, colA, colB, colC, colD, colE, colF, colG, colH)/100
  if(Expression == "Conj2") {
    return(max(0, sum(restrictor_ratings)-1))
  }
  if(Expression == "Conj3") {
    return(max(0, sum(restrictor_ratings)-2))
  }
  if(Expression == "Num2") {
    return(Luk_two(restrictor_ratings))
  }
  if(Expression == "Num3") {
    return(Luk_three(restrictor_ratings))
  }
  if(Expression == "All") {
    return(max(0, sum(restrictor_ratings)-length(restrictor_ratings)+1))
  }
  if(Expression == "Some") {
    return(min(1, sum(restrictor_ratings)))
  }
  if(Expression == "None") {
    return(max(0, 1-sum(restrictor_ratings)))
  }
  stop("condition error")
}

qv_data <- qv_data %>%
  rowwise() %>%
  mutate(LukasiewiczPrediction = 100*predict_Lukasiewicz(Expression, Restrictor, A, B, C, D, E, F, G, H)) %>%
  ungroup()



# Extract predictions for product fuzzy logic
prod_two <- function(x){
  M <- outer(x, x, FUN = "*")
  M <- M*upper.tri(M)
  1-prod(1-M)
}
prod_three <- function(x){
  A <- multi.outer(prod, x, x, x)
  keep_A <- multi.outer(function(i,j,k)(i<j&j<k), seq_along(x), seq_along(x), seq_along(x))
  1-prod(1-keep_A*A)
}

predict_product <- function(Expression, Restrictor, colA, colB, colC, colD, colE, colF, colG, colH) {
  restrictor_ratings <- extract_ratings_from_restrictor(Restrictor, colA, colB, colC, colD, colE, colF, colG, colH)/100
  if(Expression %in% c("Conj2", "Conj3", "All")) {
    return(prod(restrictor_ratings))
  }
  if(Expression == "Num2") {
    return(prod_two(restrictor_ratings))
  }
  if(Expression == "Num3") {
    return(prod_three(restrictor_ratings))
  }
  if(Expression == "Some") {
    return(1-prod(1-restrictor_ratings))
  }
  if(Expression == "None") {
    return(prod(1-restrictor_ratings))
  }
  stop("condition error")
}


qv_data <- qv_data %>%
  rowwise() %>%
  mutate(productPrediction = 100*predict_product(Expression, Restrictor, A, B, C, D, E, F, G, H)) %>%
  ungroup()


# Mean square error comparison
pdf("Graphs/model_errors.pdf",height=5,width=8)
qv_data %>%
  filter(Condition%in%c("T1","T2")) %>%
  mutate(Expression = factor(Expression, levels = c("Conj2", "Conj3", "Some", "All", "None", "Num2", "Num3"))) %>%
  mutate(ChancePrediction = 50) %>%
  pivot_longer(
    cols = c(FuzzyPrediction, LukasiewiczPrediction, productPrediction, ChancePrediction), 
    names_pattern = "(.+)Prediction", 
    names_to = "Logic", 
    values_to = "Prediction") %>%
  group_by(Expression, Logic, Subject) %>%
  summarize(Error = sqrt(mean((Answer-Prediction)^2))/100) %>%
  group_by(Expression, Logic) %>%
  summarize(
    MSE = mean(Error),
    MSE_se = se(Error),
  ) %>%
  ungroup() %>%
  mutate(Logic = if_else(Logic == "Fuzzy", "Godel", Logic)) %>%
  mutate(Expression = factor(Expression, levels = c("Conj2", "Conj3", "Num2", "Num3", "Some", "All", "None"))) %>%
  ggplot(aes(x = 1, y = MSE, ymin = MSE-MSE_se, ymax = MSE+MSE_se, fill = Logic)) +
  facet_wrap(.~Expression, ncol = 4)+
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(position = position_dodge()) +
  theme_bw() +
  scale_color_manual(values=FourColorPalette,aesthetics = c("color","fill")) +
  scale_y_continuous(name = "RMSE") +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )
dev.off()



# Compare predictions:
qv_data %>%
  pivot_longer(
    cols = c(FuzzyPrediction, LukasiewiczPrediction, productPrediction), 
    names_pattern = "(.+)Prediction", 
    names_to = "Logic", 
    values_to = "Prediction") %>%
  # filter(Condition%in%c("T1","T2")) %>%
  ggplot(aes(x = Answer/100, y = Prediction/100, col = Logic)) +
  facet_wrap( ~ Expression) +
  geom_abline(slope=1,intercept = 0,linetype=2)+
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values=FourColorPalette,aesthetics = c("color","fill"))



# Compare predictions by adjective
adj_order <- qv_data %>%
  filter(Condition%in%c("T1","T2")) %>%
  mutate(ChancePrediction = 50) %>%
  group_by(adjective, Subject) %>%
  summarize(Error = sqrt(mean((Answer-ChancePrediction)^2))/100) %>%
  group_by(adjective) %>%
  summarize(mean_chance = mean(Error)) %>%
  ungroup() %>%
  arrange(desc(mean_chance)) %>%
  pull(adjective)

pdf("Graphs/model_errors_by_adj.pdf",height=5,width=8)
qv_data %>%
  filter(Condition%in%c("T1","T2")) %>%
  mutate(Expression = factor(Expression, levels = c("Conj2", "Conj3", "Some", "All", "None", "Num2", "Num3"))) %>%
  mutate(ChancePrediction = 50) %>%
  pivot_longer(
    cols = c(FuzzyPrediction, LukasiewiczPrediction, productPrediction, ChancePrediction), 
    names_pattern = "(.+)Prediction", 
    names_to = "Logic", 
    values_to = "Prediction") %>%
  group_by(adjective, Logic, Subject) %>%
  summarize(Error = sqrt(mean((Answer-Prediction)^2))/100) %>%
  group_by(adjective, Logic) %>%
  summarize(
    MSE = mean(Error),
    MSE_se = se(Error),
  ) %>%
  ungroup() %>%
  mutate(Logic = if_else(Logic == "Fuzzy", "Godel", Logic)) %>%
  mutate(adjective=factor(adjective,levels=adjective_list)) %>%
  ggplot(aes(x = 1, y = MSE, ymin = MSE-MSE_se, ymax = MSE+MSE_se, fill = Logic)) +
  facet_wrap(.~adjective, ncol = 6)+
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(position = position_dodge()) +
  theme_bw() +
  scale_color_manual(values=FourColorPalette,aesthetics = c("color","fill")) +
  scale_y_continuous(name = "RMSE") +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )
dev.off()



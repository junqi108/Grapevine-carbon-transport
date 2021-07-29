read.files <- function(file.directory, subject) {
  # file.directory <- ExpData
  #  subject <- "Carbon-optimization_0"
  # file_list <- list.files(file.directory)
  # filter the files
  file_list <- 
    list.files(file.directory) %>%
    .[grepl(subject, .)]
  
  temp.a = 1
  for (file in file_list){
    # if(temp.a == 1) {
    #   dataset = read_xlsx(file.path(file.directory, file), sheet = 'RawData',
    #                                      col_names = TRUE)
    #   variety = substr(file, 13, 15)
    #   if(variety == 'SB_') variety.name = 'Sauvignon blanc'
    #   if(variety == 'CH_') variety.name = 'Chardonnay'
    #   if(variety == 'PNN') variety.name = 'Pinot noir'
    #   if(variety == 'PGR') variety.name = 'Pinot gris'
    #   dataset = mutate(dataset, variety.name = variety.name)
    #   
    #   temp.a = 2
    # }
    # else if(temp.a == 2) {
    temp_dataset <-read.csv(file.path(file.directory, file),
                            header = F)
    if(temp.a == 1) {dataset = temp_dataset; temp.a =2 }
    else if(temp.a != 1)  
    {
      dataset <- 
        bind_rows(dataset, temp_dataset)
    }
    # }
  }
  return(dataset)
}


GPmodel_fun_g <- function(data_sim_model.gp) {
 
  x_g <- cbind(data_sim_model.gp['KM_BERRY']/10, 
               data_sim_model.gp['HYDROLYSIS_ROOTS'], 
               data_sim_model.gp ['sourceLoading_b']/50, 
               data_sim_model.gp['KM_ROOTS']/10)
  
  
  y_g <- data_sim_model.gp['berryDM']*10
  trans_y_g <- t(y_g)
  
  
  library(GPfit)
  d = 4
  
  GPmodel_g <- GP_fit(x_g, trans_y_g, 
                    control = c(200 * d, 80 * d, 2 * d), nug_thres = 20,
                    trace = FALSE, maxit = 100,
                    corr = list(type = "exponential",power = 1.95), 
                    optim_start = NULL)
  
  return(tibble(mod_g = list(GPmodel_g)))
  
  
}


par1 <- c(0.09, 0.0002, 0.202, 0.202)



library(soma)
Gp_com <- tibble()
df1 <- tibble()

  
soma.optim <- function(df1)
{
 
 ans_j <- soma( cost.fun_g_j, 
                 list(min=c(0.01, 0.0001, 0.1,0.1 ),  max=c(0.2, 0.0002, 0.7, 0.8)),
                 options=list(pathLength=3,perturbationChance=0.1,stepLength = 0.05,
                              minRelativeSep=0.0001,nMigrations=10,populationSize=3))
  
 bestcost_j <-  ans_j$cost[ans_j$leader]
  
  res_j <- tibble(KM_BERRY = ans_j$population[,ans_j$leader][1], 
                   HYDROLYSIS_ROOTS = ans_j$population[,ans_j$leader][2],
                   SourceLoading_b = ans_j$population[,ans_j$leader][3],
                   KM_ROOTS = ans_j$population[,ans_j$leader][4],
                   cost = bestcost_j)
  return(res_j)
  
}

cost.fun_g_j <- function( par1) 
{
  
  mod_g <- as.list(Gp_com[3])[[1]]
  obs_g <- t(Gp_com[4])
  Gp_com$cost <- 0
  
  
  
  for (i in nrow(Gp_com))
  {
    y_hat_obs <-  predict(mod_g[[i]], matrix(par1, ncol = 4))$Y_hat
    
    Gp_com$cost[i] <- sqrt((y_hat_obs - obs_g[i])^2)
  }  
  
  return(sum(Gp_com$cost))
  
}

  
 cost.fun_g <- function( par1) 
  {
    mod_g <- as.list(df1[3])[[1]]
    obs_g <- t(df1[4])
   # df1$cost <- 0
   
      for (i in nrow(df1))
      {
      y_hat_obs <-  predict(mod_g[[i]], matrix(par1, ncol = 4))$Y_hat
      
      df1$cost[i] <- sqrt((y_hat_obs - obs_g[i])^2)
      }
    
       return(sum(df1$cost))
    
  }

  
 

cost.fun_g_working <- function(par1) 
{
  
  
    mod_g <- as.list( Gp_com[3])[[1]]
    obs_g <- t(Gp_com[4])
    
   
      y_hat_obs <-  predict( mod_g[[1]], matrix(par1, ncol = 4))$Y_hat
      
      return((y_hat_obs - obs_g[1])^2) 
        
}


# Soma function does not work for group_by
total.cost <- function(par1)
{
  final.cost <- 
    Gp_com %>% 
    group_by(day, scenario) %>% 
    do(cost.fun_g_i(.,par1)) %>% 
    ungroup() %>% 
    summarise_at('cost', sum)
  
  return(final.cost)
  
}








#
#*********************************************************
#ggplot function
#*********************************************************
# library(cowplot)
# cowplot can use save_plot to save the arranged ggplot figures
# final.plot <-  plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)
#   
# save_plot(file.path(FigureOutput, "carbon partition trial.pdf"), final.plot, base_height = 17, base_width = 15,
#             base_aspect_ratio = 1.2 # make room for figure legend
# for qucik plot we can use qplot to see the data first

# annotation should specify the place where to put the annotation by factor levels
# ann_text <- data.frame(PhytomerRank = factor(c(5,5,7,7)),
#                        variable = factor(c("Blade","Sheath","Blade","Sheath")),
#                        x = c(70,70,70,70), y = c(52,21,80,21),
#                        label.text = c("A Blade 5","B Sheath 5","C Blade 7","D Sheath 7"))
# use parse in geom_text
# custom_label <- data.frame(cyl = c(4,6,8),
#                            wt = c(5,5,5),
#                            lab2 = c("chi^2*':'~18.5*', p =3e-04'",
#                                      "chi^2*':'~4.7*', p =0.1909'",
#                                      "chi^2*':'~15.3*', p =0.0016'"))
# p + facet_grid(. ~ cyl) +
#   geom_text(data = custom_label, aes(x=30, y=wt, label = lab2), size = 3, parse = T)

# limits.leaf.dry <- aes(ymax = total.leaf.dry + se, ymin = total.leaf.dry - se)
# ann_arrows <- data.frame(x = c(167,192,201, 215,222,287),
#                          y = c(0.3,0.3,0.3,0.3,0.3,0.3), 
#                          yend =c(-0.10,-0.1,-0.10,-0.10,-0.10,-0.10),
#                          xend =c(167,192,201, 215, 222,287))
#layout theme
# source(file="Layout_LegendOpts_ScenarioAnalysis.R")

###plotfunction
plot.byrank <- function(data, #data.2,  # can import many datasets if there are
                        scale.xmin,scale.xmax, scale.xby,
                        scale.ymin,scale.ymax, scale.yby, 
                        xaxis.title,
                        yaxis.title, 
                        legend=FALSE, title=FALSE) {
  p <- ggplot(data = data,aes(x = ThermalTime))+    # aes to map the data by specifying what is x and what is y
    # wrap and panels
    facet_wrap(~PhytomerRank + variable, scales = "free") + 
    
    geom_text(data = ann_text,aes(x = x, y = y, label = label.text),hjust=0, giude = "none", size = 5 ) +
    # point figure, can use subset to derive part of the data
    # ase map the data, like how do you divide the data and how to show the legend
    geom_point(data = subset(data, Version == "Obs"), aes(y = value,shape = factor(Treatment)), size = 3) +
    # control the shape of the points manually
    scale_shape_manual(name ="", values=c(1,2)) +
  
    # control the line shape
    # , colour = Version, only defined it in aes then you can change the shape or colour, like defining the map first
    geom_line(data = subset(data, Version == "Sim"),aes(y = value, linetype = factor(RunningIdentity))) +
    # control the shape of the line
    # can suppress the legend display by  guide = "none"
    scale_linetype_manual(name = "",values=c(1,2,3,4,5,6,1))+
    # errorbar
    geom_errorbar(data = data,limits.leaf.dry, width=0.7) +
    scale_size_manual(name = "",values=c(1.5,1,1,1,1,1,1))+   # size of each line
    # add arrows
#     geom_segment(data = ann_arrows,aes(x = x, y = y, xend = xend, yend = yend), 
#                  arrow = arrow(length = unit(0.2, "cm"))) +
#     #scale_colour_manual("", values=c("red", "blue")) + 
    
    theme(panel.margin = unit(1, "lines")) +      # control the distance between different panels
    
    # control the axis
    #      # scale_y_continuous(yaxis.title)+                  # control the x, y lab and ticks
    scale_x_continuous(xaxis.title, expand=c(0,0), limits=c(scale.xmin, scale.xmax),
                       breaks=seq(from = scale.xmin, to=scale.xmax, by=scale.xby)) +
    scale_y_continuous(yaxis.title, expand=c(0,0), limits=c(scale.ymin, scale.ymax),
                       breaks=seq(from=scale.ymin, to=scale.ymax, by=scale.yby)) +   
                       
    # theme of the layout
    lib.opts.layout 
  
  if (legend) {
    p+lib.opts.legend 
  } 
  else {
    p+lib.opts.nolegend
  }
  # if (title) p+opts(title=title)
}


plot.radiation <- function(data.1, # can import many datasets if there are
                           xvar, yvar,
                           scale.xmin,scale.xmax, scale.xby,
                           scale.ymin,scale.ymax, scale.yby, 
                           xaxis.title,
                           yaxis.title, 
                           label.text,
                           legend=FALSE, title=FALSE) {
  p <- ggplot()+    # aes to map the data by specifying what is x and what is y
    # wrap and panels
    # facet_wrap(~treat, nrow = 2, ncol = 1, scales = "free_y")+ # , scales = "free"
    # ifelse(scale.ymax >0,0.9*scale.ymax, 1.05*scale.ymax ),
    geom_text(data = data_frame(x_var = 0, y_var = 0),
              aes(x = scale.xmin + 0.03*(scale.xmax -scale.xmin),
                  y = scale.ymin + 0.96*(scale.ymax -scale.ymin),  
                  label = label.text),
              hjust=0,  size = 5) +
    geom_point(data = data.1, aes(x = xvar, y=yvar))+
    geom_line(data = data.1, aes(x = xvar, y=yvar))+
    # scale_linetype_manual(name ="", values=c(1,2)) +
    # geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
    #           fill='gray80', alpha=0.3) +
    # 
    theme(panel.spacing = unit(0.5, "lines")) +      # control the distance between different panels
    
    # control the axis
    #      # scale_y_continuous(yaxis.title)+                  # control the x, y lab and ticks
    # scale_y_continuous(yaxis.title)+                  # control the x, y lab and ticks
    #scale_x_continuous(xaxis.title, expand=c(0,0), limits=c(scale.xmin*0.95, scale.xmax*1.05),
    scale_x_continuous(xaxis.title, expand=c(0,0), limits=c(scale.xmin, scale.xmax),
                       breaks=seq(from = scale.xmin, to=scale.xmax, by=scale.xby)) +
    scale_y_continuous(yaxis.title, expand=c(0,0), limits=c(scale.ymin*0.98, scale.ymax*1.02),
                       breaks=seq(from=scale.ymin, to=scale.ymax, by=scale.yby)) +
    
    
    # theme of the layout
    lib.opts.layout +
    panel_border(colour = "black", size = 0.5,
                 linetype = 1,remove = FALSE)
  
  if (legend) {
    p+lib.opts.legend 
  } 
  else {
    p+lib.opts.nolegend
  }
  # if (title) p+opts(title=title)
}


#*********************************************************
# use stat_summary for ploting mean, errorbar
#*********************************************************
function_unused <- function(.) {
# stat_sum_single <- function(fun, geom="point", ...) {
#   stat_summary(fun.y=fun, colour="red", geom=geom, size = 1, ...)
# }
# 
# errorUpper <- function(x){ 
#   x.mean <- mean(x) 
#   x.sd <- sd(x) 
#   SEM <- x.sd / (sqrt(length(x))) 
#   return(x.mean + (SEM*1.96)) 
# } 
# 
# errorLower <- function(x){ 
#   x.mean <- mean(x) 
#   x.sd <- sd(x) 
#   SEM <- x.sd / (sqrt(length(x))) 
#   return(x.mean - (SEM*1.96)) 
# } 
# ggplot(C_N.data, aes(x=day, y=value.N.content, group=as.factor(stress), 
#                      colour=as.factor(stress))) + 
#   facet_wrap(~N.content,  scales = "free_y") +
#   stat_summary(fun.y=mean, geom="point") + 
#   stat_summary(fun.y=mean, geom="line")+ 
#   stat_summary(fun.ymax = errorUpper, fun.ymin = errorLower, geom = 
#                  "errorbar") + ggtitle("Gourieroux 2003 (HHH) and (LLL)") +
#   ylab("N content  (mg g-1)")
}













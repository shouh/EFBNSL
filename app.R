## app.R ##
library(shiny)
library(shinydashboard)
library("stringr")
library("philentropy")
library("Rgraphviz")
library("stringr")
library(bnlearn)
library(minerva)
library("ggplot2")
ui <- dashboardPage(
  skin = c("blue"), #自带6种主题颜色：“red”, “blue”, “black”, “purple”, “green”, “yellow”。
  dashboardHeader(title = "EFBNSL!",
          ##消息下拉单	
          dropdownMenu(type = "messages", badgeStatus = "info",	
                       messageItem(from = "wsh_inf@ruc.edu.cn",
                                   message = "E-mail",	
                                   href = "http://mail.ruc.edu.cn"	
                       )
          ),
          
          dropdownMenu(type = "tasks", badgeStatus = "success",	
                       notificationItem(icon = icon("edge"), status = "success",	
                                        text =  "Bayesian Network Repository",	
                                        href = "http://www.bnlearn.com/bnrepository"	
                       ),	
                       
                       notificationItem(icon = icon("edge"), status = "success",	
                                        text = "R语言官网",
                                        href = "https://www.r-project.org"
                       ),	
                       notificationItem(icon = icon("edge"),	
                                        status = "success", 	
                                        text = "blearn官网",
                                        href = "http://www.bnlearn.com"
                       )
          )),
  
  
  dashboardSidebar(sidebarMenu(
    menuItem("简介", tabName = "jianjie", icon = icon("dashboard")),
    menuItem("参数设置", tabName = "canshushezhi", icon = icon("th")),
    menuItem("数据集概况", tabName = "gaikuang", icon = icon("list-ol")),
    menuItem("初始网络", tabName = "initialnetwork", icon = icon("th")),
    menuItem("实验过程", tabName = "shiyanguocheng", icon = icon("exchange-alt")),
    menuItem("实验结果", tabName = "shiyanjieguo", icon = icon("bar-chart-o")),
    menuItem("结果对比", tabName = "jieguoduibi", icon = icon("bar-chart-o")),
    menuItem("关于软件", tabName = "guanyuruanjian", icon = icon("book-open")),
    menuItem("版本信息", tabName = "banben", icon = icon("exclamation-circle")),
    br(),
    hr(style = "color:red"),
    actionButton("initialButton", label = "构建InitialBN", icon = icon("object-ungroup")),
    p("Construct the initial network"),
    actionButton('trainNetworkButton', '   训练BN   ',icon = icon("object-group"),width = "123px"),
    p("Train the bayesian network.")
  )
  ),
  dashboardBody(
    
      tabItems(
        # First tab content
        tabItem(tabName = "jianjie",
            tabBox(
              tabPanel("软件介绍",
                       verbatimTextOutput("abstract")
                       ),
              tabPanel("初始网络构建图",
                       span("Figure 1 Initial network construction process", style = "color:green;font-weight:bold"),
                       hr(style = "color:red"),
                       imageOutput("plot0",height = "auto",width = "auto")
                       ),
              tabPanel("EFBNSL流程图",
                       span("Figure 2 The demonstration of the EFBNSL structure learning algorithm", style = "color:green;font-weight:bold"),
                       hr(style = "color:red"),
                       imageOutput("plot1",height = "auto",width = "auto")
                       ),
              tabPanel("集成学习概念图",
                       span("Figure 3 The ensemeble learning algorithm", style = "color:green;font-weight:bold"),
                       hr(style = "color:red"),
                       imageOutput("plot2",height = "auto",width = "auto")
                       ),
              width = 12
            )
          ),# 以文本形式打印summary变量
  
  
        
        # Second tab content
        tabItem(tabName = "canshushezhi",
          span("算法中的各参数设置", style = "color:green;font-weight:bold"),
          fluidPage(
            br(),
            fluidRow(
              column(4,
                     hr(),
                     selectInput("dataset", "数据集:", 
                                 choices = c("alarm", "asia", "insurance","hepar2","win95pts","andes","link","child","barley"))
              ),
              column(4,
                     hr(),
                     selectInput("score_funcation", "评分函数:",
                                 choices = c("bde", "aic", "bic"))
              ),
              column(4,
                     hr(),
                     numericInput("threshold", "值域Θ设置:", 0.35,step = 0.01,min = 0.1,max = 1.0)
              )
            ),
            fluidRow(
              column(4,
                     hr(),
                     numericInput("yangbenliang", "数据样本量(占总样本量的百分比):", 0.25,step = 0.05)
              ),
              column(4,
                     hr(),
                     sliderInput("train_number", "训练次数:", 
                                 min = 1, max = 20, value =10, step= 1)
              ),
              column(4,
                     hr(),
                     sliderInput("aerfa", "Alpha(α)值:", 
                                 min = 0.1, max = 1, value = 0.9, step= 0.1)
              )
            ),
            fluidRow(
              column(4,
                     hr(),
                     sliderInput("avg", "N次平均结果:", 
                                 min = 1, max = 20, value = 10, step= 1)
              )
            )
          ),
          hr(style = "color:red"),
          span("训练贝叶斯网络结构学习算法进行对比:", style = "color:green;font-weight:bold"),
          # 定义勾选框
          fluidPage(
            fluidRow(
              column(1,
                 checkboxInput("EFBNSL", "EFBNSL", T)
              ),
              column(1,
                 checkboxInput("PC", "PC", F)
              ),
              column(1,
                 checkboxInput("GS", "GS", F)
              ),
              column(1,
                 checkboxInput("IAMB", "IAMB", F)
              ),
              column(1,
                 checkboxInput("HC", "HC", F)
              ),
              column(1,
                 checkboxInput("MMHC", "MMHC", F)
              ),
              column(1,
                 checkboxInput("TABU", "TABU", F)
              )
            )
          )
          
      ),
      
      tabItem(tabName = "initialnetwork",
              
              tabBox(
                tabPanel("Matrix",
                    span("通过MIC进行初始网络构建,其Characteristic Matrix如下所示:", style = "color:green;font-weight:bold"),
                    hr(style = "color:red"),
                    verbatimTextOutput("initialtext")
                ),
                tabPanel("初始网络",
                    span("构造的初始网络结构,如下所示:", style = "color:green;font-weight:bold"),
                    hr(style = "color:red"),
                    plotOutput("initialNet")
                ),
                width = 12
              )
              ),
      
      tabItem(tabName = "shiyanguocheng",
            tabBox(
              tabPanel("参数设置", 
                       span("参数设置情况如下:", style = "color:green;font-weight:bold"),
                       verbatimTextOutput("experimenttext")
              ),
              tabPanel("迭代过程", 
                 span("根据EFBNSL算法进行贝叶斯网络结构学习,初始网络和贝叶斯网络迭代过程如下:", style = "color:green;font-weight:bold"),
                 # verbatimTextOutput("expre_process_initial"),
                 verbatimTextOutput("expre_process")
              ),
              tabPanel("可视化展示", 
                 span("可视化贝叶斯网络迭代过程如下:", style = "color:green;font-weight:bold"),
                 fluidPage(
                   fluidRow(
                     column(2,
                            hr(),
                            plotOutput("bn1",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn2",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn3",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn4",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn5",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn6",height = "150px",width = "200px")
                     )
                   ),
                   fluidRow(
                     column(2,
                            hr(),
                            plotOutput("bn7",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn8",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn9",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn10",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn11",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn12",height = "150px",width = "200px")
                     )
                   ),
                   fluidRow(
                     column(2,
                            hr(),
                            plotOutput("bn13",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn14",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn15",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn16",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn17",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn18",height = "150px",width = "200px")
                     )
                   ),
                   fluidRow(
                     column(2,
                            hr(),
                            plotOutput("bn19",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn20",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn21",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn22",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn23",height = "150px",width = "200px")
                     ),
                     column(2,
                            hr(),
                            plotOutput("bn24",height = "150px",width = "200px")
                     )
                   )
                 )
              ),
              width = 12
            )
        ),
      
      tabItem(tabName = "shiyanjieguo",
           tabBox( 
                tabPanel("训练过程",
                   span("Function: EFBNSL算法用于贝叶斯网络结构学习,其训练结果如下:", style = "color:green;font-weight:bold"),
                   fluidPage(
                     fluidRow(
                       column(4,
                              hr(),
                              plotOutput("resultf1",height = "200px",width = "300px")
                       ),
                       column(4,
                              hr(),
                              plotOutput("resulthd1",height = "200px",width = "300px")
                       ),
                       column(4,
                              hr(),
                              plotOutput("resulttp1",height = "200px",width = "300px")
                       )
                     )
                   ),
                   hr(style = "color:red"),
                   plotOutput("resultbn1",height = "350px",width = "450px")
                   
                ),
               tabPanel("算法结果", 
                  span("贝叶斯网络结构学习各算法学习结果如下:", style = "color:green;font-weight:bold"),
                  hr(style = "color:red"),
                  verbatimTextOutput("resulttext")
              ),
              tabPanel("可视化比较", 
                  span("各算法柱状图比较分析如下:", style = "color:green;font-weight:bold"),

                  plotOutput("show_compare_f1"),

                  plotOutput("show_compare_hd"),

                  plotOutput("show_compare_tp")

              ),
               width = 12
           )
        ),
      
      tabItem(tabName = "gaikuang",
              tabBox(
                tabPanel("网络概况",
                      verbatimTextOutput("dataset1"),
                      plotOutput("bnnetwork")
                    ),
                tabPanel("数据集概况",
                      verbatimTextOutput("dataset2")
                    ),
                width = 12
              )
              ),
      
      tabItem(tabName = "jieguoduibi",
              tabBox(
                tabPanel("算法对比",
                         span("Table 2: The EFBNSL algorithm vs. other algorithms", style = "color:green;font-weight:bold"),
                         hr(style = "color:red"),
                         verbatimTextOutput("textasia"),
                         tableOutput("resultasia"),
                         verbatimTextOutput("textalarm"),
                         tableOutput("resultalarm"),
                         verbatimTextOutput("texthepar"),
                         tableOutput("resulthepar")
                ),
                tabPanel("柱状图",
                         span("Figure 4: Performance of different algorithms with different datasets with F1-score evaluation metrics", style = "color:green;font-weight:bold"),
                         hr(style = "color:red"),
                         imageOutput("resultplot",height = "auto",width = "auto")
                ),
                width = 12
              )
            ),
      
      tabItem(tabName = "guanyuruanjian",
              h5("本软件用于贝叶斯网络结构学习,源于我们提出的一种新的结构学习算法——"),
              em("EFBNSL"),
              h5("Bayesian Network Structure Learning by Ensemble Learning and Feedback Strategy)!"),
              hr(style = "color:red"),
              a(href = "http://www.bnlearn.com/bnrepository", # 前面需要加https://,否在为打开子网页
                        target = "_blank", #target参数表示点击后，超链接的相应方式，_blank表示默认打开新标签页
                       "Bayesian Network Repository"),
              br(),
              a(href = "https://www.r-project.org", # 前面需要加https://,否在为打开子网页
                target = "_blank", #target参数表示点击后，超链接的相应方式，_blank表示默认打开新标签页
                "R语言官网"),
              br(),
              a(href = "http://www.bnlearn.com", # 前面需要加https://,否在为打开子网页
                target = "_blank", #target参数表示点击后，超链接的相应方式，_blank表示默认打开新标签页
                "blearn官网"),
              hr(style = "color:red"),
              h5("联系我们: wsh_inf@ruc.edu.cn")
              ),
      
      tabItem(tabName = "banben",
              h5("版本号: V1.0.0")
              )
    
    )
  )
)
server <- function(input, output) {
  # 软件简介
  output$abstract<-renderPrint({
    cat("研究背景和目的:\n")
    cat("    1、单一结构学习算法学习效果差;\n")
    cat("    2、一些搜索算法易于陷入局部最优;\n")
    cat("    3、如K2算法需要节点顺序和最大父节点数作为先验知识;\n")
    cat("\n\n")
    cat("实验步骤和设计思路:\n")
    cat("    1、根据最大信息系数（MIC）和我们提出的“加边”策略，构建初始网络;\n")
    cat("    2、使用Bootstrap方法对数据样本进行采样获得若干样本集，对于每个样本集，在给定初始网络结构的基础上，利用BDe评分和禁忌搜索算法训练生成若干贝叶斯网络并用邻接矩阵表示;\n")
    cat("    3、利用提出的集成策略函数𝑊，根据得到的若干邻接矩阵计算每条边的权重，通过设定权重阈值得到集成学习结果;\n")
    cat("    4、根据集成学习结果利用反馈策略更新初始网络并进入下一次迭代;\n")
    cat("    5、经过不断迭代最终确定贝叶斯网络结构;\n")
    cat("    6、在7种不同大小的标准数据集中进行实验，计算F1值(F1-score)、汉明距离(Hamming Distance, HD)和学习到的正确边数(TP)并与其他算法进行对比分析;\n")
    cat("\n\n")
    cat("加边规则:\n")
    cat("    1、为避免造成边的冗余,根据MIC值从大到小依次添加有向边时,若从该节点出发有其他边存在,则跳过该列继续判断下一列。如添加边S后,则不再判断边S;\n")
    cat("    2、在添加有向边时,若该边的反向边已经存在,则继续判断下一对满足公式(1)的节点对。如从大到小依次添加时,发现其反向边已经存在,则继续判断;\n")
    cat("    3、在添加有向边时,若构成环结构,则继续判断下一对满足公式(1)的节点对。如若存在边D满足公式(1),在添加该边时发现构成环结构,则跳过该边,继续判断下一个满足条件的节点对;\n")
    cat("\n\n")
    cat("评价指标:\n")
    cat("    本文选用值（F1-score）和汉明距离（Hamming Distance）对生成的贝叶斯网络进行评价.\n")
    cat("    recall 表示召回率\n")
    cat("    precision 表示精准率\n")
    cat("    TP 表示在标准网络和当前网络中均存在的边数，即学习到的正确边数\n")
    cat("    FP 表示存在于当前网络中而不在标准网络中出现的边的个数\n")
    cat("    FN 表示存在于标准网络中而不在当前网络中出现的边的个数\n")
    cat("    HD 值越小表示学习到的网络结构越接近真实网络\n")
    cat("             recall=TP/(TP+FN)\n")
    cat("             precision=TP/(TP+FP)\n")
    cat("             F1=2*recall*precision/(recall+precision)\n")
    cat("             HD=FP+FN\n")
    cat("\n \n")
    cat("贡献:\n")
    cat("    1、提出集成策略函数计算边的权重，利用集成学习方法减少学习到的贝叶斯网络中可能存在的多边、少边和反边情况;\n")
    cat("    2、根据节点间的MIC值直接确定有向边，并得到初始网络结构，相较于Zhang等中先构造无向图再消除环结构最后定向的操作更加简便;\n")
    cat("    3、提出反馈更新策略，根据集成学习结果对初始网络结构(边的方向)进行动态调整，以保证先验知识的有效性;\n")
    cat("\n")
  })
  
  output$plot0<-renderImage({
    return(list(
      src="/Users/wsh/project/R/Haide-master/initial.png",
      filetype ="image/png",
      alt = "plot21"
    ))
  },deleteFile = FALSE)
  
  
  output$plot1<-renderImage({
    return(list(
      src="/Users/wsh/project/R/Haide-master/EFBNSL.png",
      filetype = "image/png",
      alt = "plot1"
    ))
  }, deleteFile = FALSE)
  
  
  output$plot2<-renderImage({
    return(list(
      src="/Users/wsh/project/R/showdemo/ensemble.png",
      filetype = "image/png",
      alt = "plot2"
    ))
  }, deleteFile = FALSE)
  
  output$resultplot<-renderImage({
    return(list(
      src="/Users/wsh/project/R/showdemo/result.png",
      filetype = "image/png",
      alt = "resultplot"
    ))
  }, deleteFile = FALSE)
  
  output$textasia<-renderPrint({
    cat("   ASIA \t\t\t1K \t\t\t2K \t\t\t5K\n")
  })
  
  output$resultasia<-renderTable({
    testdata<-read.csv("/Users/wsh/project/R/Haide-master/resultasia.csv",header=T,sep = ",")
    data.frame(testdata)
  })
  
  output$textalarm<-renderPrint({
    cat("   ALARM \t\t\t1K \t\t\t2K \t\t\t5K\n")
  })
  
  output$resultalarm<-renderTable({
    testdata<-read.csv("/Users/wsh/project/R/Haide-master/resultalarm.csv",header=T,sep = ",")
    data.frame(testdata)
  })
  
  output$texthepar<-renderPrint({
    cat("   HEPAR2 \t\t\t1K \t\t\t2K \t\t\t5K\n")
  })
  
  output$resulthepar<-renderTable({
    testdata<-read.csv("/Users/wsh/project/R/Haide-master/
resulthepar.csv",header=T,sep = ",")
    data.frame(testdata)
  })
  
  readData<-function(dataname){
    str_path<-str_c("/Users/wsh/project/R/dataset/",dataname,"/")
    str_net<-str_c(dataname,".net")
    str_rda<-str_c(dataname,".rda")
    
    setwd(str_path)
    getwd()
    #Alarm贝叶斯网络真实结构
    data_net<-read.net(str_net)
    
    dag_org_data = model2network(modelstring(data_net))
    data_rda<-load(str_rda)
    nparams(bn)
    data<-rbn(bn,n=round(20000))
    return(list(dag_org_data,data))
  }
  
  output$dataset1<-renderPrint({
    lis<-readData(input$dataset)
    cat("网络概况-",input$dataset,'\n')
    lis[[1]]
  })
  
  output$dataset2<-renderPrint({
    lis<-readData(input$dataset)
    cat("数据集-",input$dataset,'\n')
    cat("\n\n")
    lis[[2]]
  })
  
  output$bnnetwork<-renderPlot({
    lis<-readData(input$dataset)
    graphviz.plot(lis[[1]],shape = "ellipse")
  })
  
  #----------------从下面开始处理数据-------------------
  
  # 利用sigmoid函数将结果映射到0-1之间
  sigmoid = function(matrix,ncol) {
    for( q in 1:ncol){
      for(p in 1:ncol){
        #matrix[q,p]<-log10(matrix[q,p])
        matrix[q,p]<-(1/ (1 + exp(-matrix[q,p])));
      }
    }
    return(matrix)
  }
  
  # 利用x/(x+1)函数将数据映射到0-1之间
  # linef<-function(matrix){
  #   for( q in 1:ncol){
  #     for(p in 1:ncol){
  #       matrix[q,p]<-matrix[q,p]/(matrix[q,p]+1)
  #     }
  #   }
  #   return(matrix)
  # }
  
  # 最大最小值归一化
  maxmin<-function(matrix,ncol){
    for( q in 1:ncol){
      for(p in 1:ncol){
        matrix[q,p]<-(matrix[q,p]-min(matrix))/(max(matrix)-min(matrix))
      }
    }
    return(matrix)
  }
  
  # 在生成初始贝叶斯网络结构时，判断此时网络ug中是否存在该边，返回bool类型
  judge_arc_exist<-function(from_node,to_node,ug){
    object<-ug$arcs;#获取学习得到的贝叶斯网络的有向边
    index<-nrow(ug$arcs); #计算一共生成多少个有向边
    result<-FALSE;
    for( i in 1:(index)){
      if((from_node==object[i,1])&(to_node==object[i,2])){
        result<-TRUE;
      }
    }
    return(result)
  }
  
  # 对各个子图进行遍历
  make_list<-function(index,lis,from_list,to_list){
    for(j in 2:length(lis)){
      for(i in 2:index){
        if(lis[j]==from_list[i]){
          lis<-c(lis,to_list[i])
        }
        if(lis[j]==to_list[i]){
          lis<-c(lis,from_list[i])
        }
      }
    }
    return(lis)
  }
  
  # 将生成的非连通有向图，修正为连通有向图
  repair_ug<-function(ug,matrix,matrix_org){
    object<-ug$arcs;#获取学习得到的贝叶斯网络的有向边
    index<-nrow(ug$arcs); #计算一共生成多少个有向边
    matrix_names<-rownames(matrix);
    nodes_name<-rownames(matrix);
    lis<-list();
    
    row_list<-list();
    col_list<-list();
    
    from_list<-object[,1];
    to_list<-object[,2];
    
    lis[1]<-from_list[1];
    lis<-c(lis,to_list[1]);
    
    
    
    for(a in 1:10){
      len<-6;
      while (len>0) {
        lis<-make_list(index,lis,from_list,to_list);
        len<-len-1;
      }
      lis <- lis[!duplicated(lis)]
      
      if(length(lis)==length(nodes_name)){
        break;
      }
      
      for (i in 1:length(lis)) {
        matrix_names<-matrix_names[which(matrix_names!=lis[i])];
      }
      
      max_mine<-0;
      mmine<-0;
      row_name_this<-"";
      col_name_this<-"";
      for (p in 1:length(lis)) {
        
        for (q in 1:length(matrix_names)) {
          mmine<-matrix_org[lis[[p]],matrix_names[q]];
          if (mmine>max_mine){
            max_mine<-mmine;
            row_name_this<-matrix_names[q];
            col_name_this<-lis[[p]];
          }
        }
      }
      # cat("col:",col_name_this,"row:",row_name_this,max_mine,'\n')
      col_list<-c(col_list,col_name_this);
      row_list<-c(row_list,row_name_this);
      lis<-c(lis,row_name_this);
    }
    return(list(col_list,row_list))
  }
  
  # 计算MIC, 并进行初始网络构建
  org_dag<-function(data,data_org){
    ncol<-ncol(data);  #获取数据集的列数ncol，并按照ncol生成ncol×ncol大小的矩阵
    data_num<-lapply(data[,1:ncol],as.numeric);#数据集，将数据框中的字符类型转换为数值型
    df_data<-as.data.frame(data_num);#将List数据类型转换为frame类型
    res <- mine(x=df_data,master = c(1:ncol));#计算MIC值,通过master定义需要计算哪一列的MIC值
    matrix<-res$MIC;
    
    #print(matrix)
    
    # 这里主要是将MIC矩阵进行保存
    #FileName=paste("/wsh/project/R/Result/alarm_mic.xlsx",sep="");#保存文件路径及文件名
    #write.xlsx(matrix,file=FileName,row.names=TRUE);
    
    #将对角线上的元素全部设置为0
    for( q in 1:ncol){
      for(p in 1:ncol){
        if(q==p){
          matrix[q,p]<-0;
        }
      }
    }
    
    matrix<-data.frame(matrix);
    data_names<-c(colnames(matrix));
    matrix_dat<-matrix;
    for( q in 1:ncol){
      max_col<-max(matrix[,q]);
      for(p in 1:ncol){
        if(matrix[p,q]>=0.9*max_col){
          #cat("mic:",matrix[p,q],"col:",data_names[q],"row:",data_names[p],"\n");
        }else{
          matrix[p,q]<-0;
        }
      }
    }
    #print(matrix)
    ug = empty.graph(names(data));#初始化一个空的网络，后面依次添加弧
    
    df<-data.frame(matrix);
    for(i in 1:ncol){
      df_one<-df[order(df[i],decreasing = TRUE),];
      df_one<-df_one[which(df_one[i]>0),];
      row_list<-rownames(df_one[i]);
      col_list<-colnames(df_one[i]);
      # cat("row_list",row_list,'\n')
      # cat("col_list",col_list,'\n')
      for (j in 1:length(row_list)){
        if(nrow(ug$arcs)==0){
          set.arc(ug,col_list[1],row_list[j])->ug;
          #print("Add the first arc");
          break;
        }
        if(judge_arc_exist(row_list[j],col_list[1],ug)){
          next;
        }else{
          failure<-try(set.arc(ug,col_list[1],row_list[j])->ug);
          if(('try-error' %in% class(failure))){
            #print("哈哈哈哈哈哈哈哈哈哈哈哈哈哈哈哈");
            next;
            
          }else{
            set.arc(ug,col_list[1],row_list[j])->ug;
          }
          break;
        }
      }
    }
    
    # 初始网络的矩阵化
    z<-c(0)
    matrix_af<-matrix(z,nrow = ncol,ncol = ncol)
    rownames(matrix_af) <- colnames(data);
    colnames(matrix_af)<-colnames(data);
    
    list_result<-repair_ug(ug,matrix_af,matrix_dat);
    add_col_list<-list_result[[1]];
    add_row_list<-list_result[[2]];
    
    for (index in 1:length(list_result[[1]])) {
      # print(add_col_list[index]);
      # print(add_row_list[index]);
      set.arc(ug,add_col_list[[index]],add_row_list[[index]])->ug;
    }
    
    object<-ug$arcs;
    #对生成的有向边在矩阵上进行表示，1 表示存在该边；0表示两变量之间没有边
    for( i in 1:(nrow(ug$arcs))){
      matrix_af[object[i,1],object[i,2]]<-1;
    }
    
    # graphviz.plot(ug)
    #graphviz.plot(ug, shape = "ellipse")
    calculate_index(data,data_org,ug);
    #cat("初始贝叶斯网络构造完成！");
    return(list(ug,matrix_af,matrix_dat));
  }
  
  #输入参数有：
  #数据集  data
  #初始网络矩阵  matrix_org
  #调整初始网络的依据矩阵  matrix_result
  #调整初始网络结构
  update_matrix<-function(data,matrix_org,matrix_result){
    # 定义一个matrix用于保存更新后的matrix
    ncol<-ncol(data);
    x=c(0)
    matrix_m5<-matrix(x,ncol = ncol,nrow = ncol)
    rownames(matrix_m5) <- colnames(data);
    colnames(matrix_m5)<-colnames(data);
    
    #print(matrix_org)
    # 定义一个初始网络，用与保存更新后的网络
    ug_org = empty.graph(names(data));
    matrix_m5<-data.frame(matrix_m5);
    data_names_m5<-c(colnames(matrix_m5));
    
    # 更新初始网络的matrix
    matrix_t<-t(matrix_result)    
    
    for( q in 1:ncol){
      for(p in 1:ncol){
        if((matrix_org[q,p])&(matrix_t[q,p])){
          matrix_org[p,q]<-1;
          matrix_org[q,p]<-0;
        }
      }
    }
    
    # 根据更新后的matrix，生成初始网络并返回
    for( q in 1:ncol){
      for(p in 1:ncol){
        if(matrix_org[q,p]!=0){
          set.arc(ug_org,data_names_m5[q],data_names_m5[p])->ug_org; # 添加预测的边 
        }
      }
    }
    return(ug_org);
  }
  
  # 基于集成学习思想进行贝叶斯网络结构的,学习
  #data 为数据集
  #dag为初始化贝叶斯网络结构
  #np 为每次选择样本数据集的np%,进行训练
  #nb 训练次数
  #scoref为评分函数
  # data_org真实的网络结构
  # matrix_org 真实网络结构的矩阵化表示
  #每次进行训练时使用的数据集的大小为m*np
  
  ebnsl<-function(data,dag, np, nb, scoref,data_org,matrix_org,isstop,nnodes){
    # 此处初始化matrix_sum矩阵
    ncol<-ncol(data);  #获取数据集的列数ncol，并按照ncol生成ncol×ncol大小的矩阵
    ug = empty.graph(names(data)); #初始化一个空的网络，后面依次添加弧
    
    y<-c(0); #用0进行初始化矩阵
    matrix_final<-matrix(y,nrow = ncol,ncol = ncol);  #有各边概率当权重
    rownames(matrix_final) <- colnames(data);
    colnames(matrix_final)<-colnames(data);
    
    z<-c(1);
    matrix_before<-matrix(z,nrow = ncol,ncol = ncol);  #有各边概率当权重
    rownames(matrix_before) <- colnames(data);
    colnames(matrix_before)<-colnames(data);
    
    t=10; # 设置超参数t=10，即类似与深度学习中的bach_size=10
    val_bde_before<-0; # 记录当前贝叶斯网络的bde评分
    finall_bde_before<-0; #记录最终的贝叶斯网络bic评分
    
    bn_dag<-0;
    count<-0; # 记录在不发生改变时应该迭代多少次
    initial_tp_ls<-list();
    initial_hd_ls<-list();
    f_ls<-list();
    hd_ls<-list();
    tp_ls<-list();
    k_ls<-list();
    lis<-list();
    
    for(k in 1:(nb/t)){
      # 这是用来做Ensemble的矩阵初始化 
      x<-c(0); #用0进行初始化矩阵
      matrix_sum_de<-matrix(x,nrow = ncol,ncol = ncol);  #有各边概率当权重
      rownames(matrix_sum_de) <- colnames(data);
      colnames(matrix_sum_de)<-colnames(data);
      
      matrix_sum_one<-matrix(x,nrow = ncol,ncol = ncol);  #生成贝叶斯结构0-1矩阵
      rownames(matrix_sum_one) <- colnames(data);    
      colnames(matrix_sum_one)<-colnames(data);
      
      matrix_process<-matrix(x,nrow = ncol,ncol = ncol);  #得到的是k/n结构的矩阵
      rownames(matrix_process) <- colnames(data);    
      colnames(matrix_process)<-colnames(data);
      
      matrix_result<-matrix(x,nrow = ncol,ncol = ncol);  #最终的处理结果矩阵
      rownames(matrix_result) <- colnames(data);
      colnames(matrix_result)<-colnames(data);
      
      matrix_update<-matrix(x,nrow = ncol,ncol = ncol);  #用与更新初始结构矩阵的
      rownames(matrix_update) <- colnames(data);  
      colnames(matrix_update)<-colnames(data);
      
      dag_batch<-empty.graph(names(data)); #初始化一个空的网络，后面依次添加弧
      
      # 查看初始网络
      # calculate(data_org,dag);
      # print("----------------------------------初始网络-------------------------")
      finall_bde_before<-score(dag,data,type = "bde");
      
      for(j in 1:t){
        #按照定义的采样规则进行采样
        #boostrap采样
        m<-(dim(data))[1];
        val<-sample(1:m,size = round(m*np),replace = TRUE,prob = rep(1/m,m));
        my_datasets<-data[val,];
        
        # 此处初始化matrix矩阵
        x<-c(0)
        matrix<-matrix(x,nrow = ncol,ncol = ncol)  # 该矩阵用来用表示1/N的矩阵
        rownames(matrix) <- colnames(data);
        colnames(matrix)<-colnames(data);
        
        matrix_bansui<-matrix(x,nrow = ncol,ncol = ncol) # 该矩阵用来表示贝叶斯结构0-1矩阵
        rownames(matrix_bansui) <- colnames(data);
        colnames(matrix_bansui)<-colnames(data);
        
        #贝叶斯网络学习，使用禁忌搜索算法进行学习
        # if(k==1){
        #   bn_tabu<-hc(my_datasets,score =scoref,max.iter = 100);
        # }
        bn_tabu<-tabu(my_datasets,start = dag,score =scoref,max.iter = 100);
        
        object<-bn_tabu$arcs; #获取学习得到的贝叶斯网络的有向边
        index<-nrow(bn_tabu$arcs); #计算一共生成多少个有向边
        
        #对生成的有向边在矩阵上进行表示，1 表示存在该边；0表示两变量之间没有边
        for( i in 1:(index)){
          matrix[object[i,1],object[i,2]]<-sqrt(nnodes/index);
          matrix_bansui[object[i,1],object[i,2]]<-1;
        }
        #print(matrix_bansui)
        matrix_sum_de+matrix->matrix_sum_de;  # matrxi 加上了每条边的权重，即得到每条边的概率值
        matrix_sum_one+matrix_bansui->matrix_sum_one;   # matrix_bansui  每天边的权重为1
      }
      
      # 对各边的概率值进行处理
      matrix_process<-matrix_sum_one*(1/j);
      matrix_result<-matrix_sum_de*matrix_process*log10(np*(dim(data)[1]));
      
      # 对一步集成学习中得到的各边权重矩阵matrix_result进行最大最小值归一化处理
      matrix_update<-maxmin(matrix_result,ncol);
      
      #print(matrix_update)
      
      # 定一个matrix_update用来更新初始网络，经过最大最小值归一化处理后，选择一个超参数阈值，用于确定一步集成学习得到的贝叶斯网络结构
      # 门的定义：通过判断BNi的网络结构与数据集的拟合程度，确定是否对初始网络进行更新；
      # 若本次得到的网络结构与数据集的拟合程度高于上次，说明通过该初始网络得到的贝叶斯网络结构较优，
      # 则不对初始网络进行更新
      # 若本次得到的网络结构与数据集的拟合程度低于上次，说明通过该初始网络得到的贝叶斯网络结构较差，
      # 则需要对初始网络进行更新
      for( q in 1:ncol){
        for(p in 1:ncol){
          if(matrix_update[q,p]<0.9){
            matrix_update[q,p]<-0;
          }else{
            #matrix_update[q,p]<-1;
          }
        }
      }
      #print(matrix_update)
      # 因为matrix_update有两个方面的作用，一方面用来更新初始网络，另一方面用于二步集成学习
      # 因此这里初始化了一个空的网络，并将matrxi_update转化为网络dag_batch
      matrix_update<-data.frame(matrix_update);
      data_names<-c(colnames(matrix_update));
      
      for( q in 1:ncol){
        for(p in 1:ncol){
          if(matrix_update[q,p]!=0){
            failure<-try(set.arc(dag_batch,data_names[q],data_names[p])->dag_batch);
            if(('try-error' %in% class(failure))){
              print("请忽略警告，在处理时，已经忽略该边的添加");
              next;
            }else{
              set.arc(dag_batch,data_names[q],data_names[p])->dag_batch; # 添加预测的边 
            }
          }
        }
      }
      
      
      # 当i==1时，一定进行更新操作，因为最初的bn0是随机给定的边的方向；
      if (k==1){
        dag<-update_matrix(data,matrix_org,matrix_update);# 更新初始边操作
        val_bde_before<-score(dag_batch,data,type="bde");
      }else{
        # 当i!=1时，需要判定当前生成贝叶斯网络结构与数据集的拟合程度，与上一次学习得到的网络与数据集的拟合程度的大小
        # 并判断是否需要更新初始网络
        val_bde<-score(dag_batch,data,type = "bde");
        calculate(data_org,dag)
        if (val_bde<val_bde_before){
          # 更新初始边操作
          dag<-update_matrix(data,matrix_org,matrix_update);
          
          finall_bde<-score(dag,data,type = 'bde');
          if(finall_bde<finall_bde_before){
            break;
          }
          
        }else{
          val_bde_before<-val_bde;
        }
      }
      lis_initial<-calculate_index(data,data_org,dag);
      lis<-calculate_index(data,data_org,dag_batch);
      initial_tp_ls<-c(initial_tp_ls,lis_initial[[3]]);
      initial_hd_ls<-c(initial_hd_ls,lis_initial[[2]]);
      
      f_ls<-c(f_ls,lis[[1]]);
      hd_ls<-c(hd_ls,lis[[2]]);
      tp_ls<-c(tp_ls,lis[[3]]);
      k_ls<-c(k_ls,k);
      
      if(k==1){
        dag_batch1<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
      }
      if (k==2){
        dag_batch2<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
      }
      if(k==3){
        dag_batch3<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
      }

      if(k==4){
        dag_batch4<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
      }

      if(k==5){
        dag_batch5<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
      }

      if(k==6){
        dag_batch6<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
      }

      if(k==7){
        dag_batch7<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
      }


      if(k==8){
        dag_batch8<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
      }

      if(k==9){
        dag_batch9<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
      }

      if(k==10){
        dag_batch10<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
      }

      if(k==11){
        dag_batch11<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
      }


      if(k==12){
        dag_batch12<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
      }


      if(k==13){
        dag_batch13<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
      }


      if(k==14){
        dag_batch14<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
        output$bn14<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
        })
      }

      if(k==15){
        dag_batch15<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
        output$bn14<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
        })
        output$bn15<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch15)),main = "(15)");
        })
      }

      if(k==16){
        dag_batch16<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
        output$bn14<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
        })
        output$bn15<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch15)),main = "(15)");
        })
        output$bn16<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch16)),main = "(16)");
        })
      }

      if(k==17){
        dag_batch17<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
        output$bn14<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
        })
        output$bn15<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch15)),main = "(15)");
        })
        output$bn16<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch16)),main = "(16)");
        })
        output$bn17<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch17)),main = "(17)");
        })
      }

      #
      if(k==18){
        dag_batch18<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
        output$bn14<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
        })
        output$bn15<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch15)),main = "(15)");
        })
        output$bn16<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch16)),main = "(16)");
        })
        output$bn17<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch17)),main = "(17)");
        })
        output$bn18<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch18)),main = "(18)");
        })
      }

      #
      if(k==19){
      dag_batch19<-dag_batch;
      output$bn1<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
      })
      output$bn2<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
      })
      output$bn3<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
      })
      output$bn4<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
      })
      output$bn5<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
      })
      output$bn6<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
      })
      output$bn7<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
      })
      output$bn8<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
      })
      output$bn9<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
      })
      output$bn10<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
      })
      output$bn11<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
      })
      output$bn12<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
      })
      output$bn13<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
      })
      output$bn14<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
      })
      output$bn15<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch15)),main = "(15)");
      })
      output$bn16<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch16)),main = "(16)");
      })
      output$bn17<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch17)),main = "(17)");
      })
      output$bn18<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch18)),main = "(18)");
      })
      output$bn19<-renderPlot({
        graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch19)),main = "(19)");
      })
    }

      if(k==20){
        dag_batch20<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
        output$bn14<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
        })
        output$bn15<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch15)),main = "(15)");
        })
        output$bn16<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch16)),main = "(16)");
        })
        output$bn17<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch17)),main = "(17)");
        })
        output$bn18<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch18)),main = "(18)");
        })
        output$bn19<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch19)),main = "(19)");
        })
        output$bn20<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch20)),main = "(20)");
        })
      }


      if(k==21){
        dag_batch21<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
        output$bn14<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
        })
        output$bn15<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch15)),main = "(15)");
        })
        output$bn16<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch16)),main = "(16)");
        })
        output$bn17<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch17)),main = "(17)");
        })
        output$bn18<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch18)),main = "(18)");
        })
        output$bn19<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch19)),main = "(19)");
        })
        output$bn20<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch20)),main = "(20)");
        })
        output$bn21<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch21)),main = "(21)");
        })
      }

      if(k==22){
        dag_batch22<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
        output$bn14<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
        })
        output$bn15<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch15)),main = "(15)");
        })
        output$bn16<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch16)),main = "(16)");
        })
        output$bn17<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch17)),main = "(17)");
        })
        output$bn18<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch18)),main = "(18)");
        })
        output$bn19<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch19)),main = "(19)");
        })
        output$bn20<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch20)),main = "(20)");
        })
        output$bn21<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch21)),main = "(21)");
        })
        output$bn22<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch22)),main = "(22)");
        })
      }

      if(k==23){
        dag_batch23<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
        output$bn14<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
        })
        output$bn15<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch15)),main = "(15)");
        })
        output$bn16<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch16)),main = "(16)");
        })
        output$bn17<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch17)),main = "(17)");
        })
        output$bn18<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch18)),main = "(18)");
        })
        output$bn19<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch19)),main = "(19)");
        })
        output$bn20<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch20)),main = "(20)");
        })
        output$bn21<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch21)),main = "(21)");
        })
        output$bn22<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch22)),main = "(22)");
        })
        output$bn23<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch23)),main = "(23)");
        })
      }

      if(k==24){
        dag_batch24<-dag_batch;
        output$bn1<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch1)),main = "(1)");
        })
        output$bn2<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch2)),main = "(2)");
        })
        output$bn3<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch3)),main = "(3)");
        })
        output$bn4<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch4)),main = "(4)");
        })
        output$bn5<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch5)),main = "(5)");
        })
        output$bn6<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch6)),main = "(6)");
        })
        output$bn7<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch7)),main = "(7)");
        })
        output$bn8<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch8)),main = "(8)");
        })
        output$bn9<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch9)),main = "(9)");
        })
        output$bn10<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch10)),main = "(10)");
        })
        output$bn11<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch11)),main = "(11)");
        })
        output$bn12<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch12)),main = "(12)");
        })
        output$bn13<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch13)),main = "(13)");
        })
        output$bn14<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch14)),main = "(14)");
        })
        output$bn15<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch15)),main = "(15)");
        })
        output$bn16<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch16)),main = "(16)");
        })
        output$bn17<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch17)),main = "(17)");
        })
        output$bn18<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch18)),main = "(18)");
        })
        output$bn19<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch19)),main = "(19)");
        })
        output$bn20<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch20)),main = "(20)");
        })
        output$bn21<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch21)),main = "(21)");
        })
        output$bn22<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch22)),main = "(22)");
        })
        output$bn23<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch23)),main = "(23)");
        })
        output$bn24<-renderPlot({
          graphviz.plot(data_org,highlight = list(arcs = arcs(dag_batch24)),main = "(24)");
        })
      }
      # 
      # if (k==1000){
      #   break;
      # }
    }

    return(list(lis,k_ls,f_ls,hd_ls,tp_ls,data_org,dag_batch,initial_hd_ls,initial_tp_ls));
  }
  
  # 计算两个向量之间的余弦相似度
  # defcosine<-function(matrix_a,matrix_b){
  #   vect_a<-as.vector(t(matrix_a))
  #   vect_b<-as.vector(t(matrix_b))
  #   return(sum(vect_a*vect_b)/sqrt((sum(vect_a^2)*sum(vect_b^2))))
  # }
  
  calculate<-function(data_org,ug){
    arc_org<-data_org$arcs; #获取学习得到的贝叶斯网络的有向边
    ind_org<-nrow(data_org$arcs); #计算一共生成多少个有向边
    arc_pre<-ug$arcs; #获取学习得到的贝叶斯网络的有向边
    ind_pre<-nrow(ug$arcs); #计算一共生成多少个有向边
    
    zhengquebian<-0;
    fanxiangbian<-0;
    duobian<-0;
    shaobiao<-0;
    
    for( i in 1:(ind_pre)){
      for(j in 1:(ind_org)){
        if(str_c(arc_pre[i,1],"~",arc_pre[i,2])==str_c(arc_org[j,1],"~",arc_org[j,2])){
          zhengquebian=zhengquebian+1;
          str<-str_c("right->","predict:",arc_pre[i,1],"~",arc_pre[i,2],"----","original:",arc_org[j,1],"~",arc_org[j,2])
          #print(str)
          next;
        }else if(str_c(arc_pre[i,1],"~",arc_pre[i,2])==str_c(arc_org[j,2],"~",arc_org[j,1])){
          fanxiangbian=fanxiangbian+1;
          str<-str_c("reverse->","predict:",arc_pre[i,1],"~",arc_pre[i,2],"----","original:",arc_org[j,1],"~",arc_org[j,2])
          #print(str)
          next;
        }
      }
    }
    
    shaobian<-ind_org-zhengquebian-fanxiangbian;
    duobian<-ind_pre-zhengquebian-fanxiangbian;
    cat("      --初始网络一共得到：",ind_pre,"条边，\t正确边：",zhengquebian,"--",'\n');
    
    return(zhengquebian)
  }
  
  
  calculate_index<-function(data,data_org,ug){
    res<-compare(data_org,ug);
    
    r<-(res$tp)/((res$tp)+(res$fn))
    p<-(res$tp)/((res$tp)+(res$fp))
    fscore<-2*r*p/(r+p)
    # cat("tp-fp-fn",res$tp,"-",res$fp,"-",res$fn,"\n");
    # cat("召回率：",r,"\n");
    # cat("准确率",p,"\n");
    # cat("TP:",res$tp,'\n');
    # cat("F1值",fscore,"\n");
    # cat("Hamming Distance(HD)",((res$fn)+(res$fp)),"\n");
    hd<-((res$fn)+(res$fp));
    # cat("Hd",hamming(data_org,ug),"\n");
    # cat("SHD",shd(data_org,ug),"\n");
    #graphviz.plot(data_org,highlight = list(arcs = arcs(ug)));
    #graphviz.compare(data_org,ug);
    # cat("学习得到的贝叶斯网络BIC评分：",score(ug,data,type = "bic"),'\n');
    # cat("标准贝叶斯网络BIC评分：",score(data_org,data,type = "bic"),"\n");
    # cat("学习得到的贝叶斯网络AIC评分：",score(ug,data,type = "aic"),'\n');
    # cat("标准贝叶斯网络AIC评分：",score(data_org,data,type = "aic"),"\n");
    # cat("学习得到的贝叶斯网络BDE评分：",score(ug,data,type = "bde"),'\n');
    # cat("标准贝叶斯网络BDE评分：",score(data_org,data,type = "bde"),"\n");
    return(list(fscore,hd,res$tp));
  }
  
  # 其他算法的实现
  strap<-function(data,np,data_org,fac_name){
    m<-(dim(data))[1];
    val<-sample(1:m,size = round(m*np),replace = TRUE,prob = rep(1/m,m));
    my_datasets<-data[val,];
    
    if(fac_name=="pc"){
      # PC方法
      dag<-pc.stable(my_datasets);
    }else if(fac_name=="gs"){
      # GS方法
      dag<-gs(my_datasets);
    }else if(fac_name=="iamb"){
      # IAMB方法
      dag<-iamb(my_datasets,alpha = 0.5);
    }else if(fac_name=="hc"){
      # HC
      dag<-hc(my_datasets,max.iter =100,score = 'bde');
    }else if(fac_name=="mmhc"){
      # mmhc方法
      dag<-mmhc(my_datasets);
    }else{
      # Tabu
      dag<-tabu(my_datasets,max.iter = 100,score = 'bde');
    }
    
    # zhengqbian<-calculate(data_org,dag);
    res<-compare(data_org,dag);
    
    r<-(res$tp)/((res$tp)+(res$fn))
    p<-(res$tp)/((res$tp)+(res$fp))
    fscore<-2*r*p/(r+p)
    # cat("tp-fp-fn",res$tp,"-",res$fp,"-",res$fn,"\n");
    # cat("召回率：",r,"\n");
    # cat("准确率",p,"\n");
    # cat("F1值",fscore,"\n");
    # cat("Hamming Distance(HD)",((res$fn)+(res$fp)),"\n");
    hd<-((res$fn)+(res$fp));
    # cat("Hd",hamming(data_org,dag),"\n");
    return(list(fscore,hd,res$tp));
  }
  
  cal<-function(data,np,dag_org_data,fac_name){
    f_ls<-list();
    hd_ls<-list();
    tp_ls<-list();
    f<-0;
    hd<-0;
    tp<-0;
    system.time(
      for( i in 1:10){
        lst<-strap(data,np,dag_org_data,fac_name)
        f_ls<-c(f_ls,lst[[1]]);
        hd_ls<-c(hd_ls,lst[[2]]);
        tp_ls<-c(tp_ls,lst[[3]]);
        f<-f+lst[[1]];
        hd<-hd+lst[[2]];
        tp<-tp+lst[[3]];
      })
    
    f_avg<-f/10;
    hd_avg<-hd/10;
    tp_avg<-tp/10;
    return(list(f_avg,hd_avg,tp_avg))
  }
  
  
  
  traininitial<-eventReactive(input$initialButton,{
    lis<-readData(input$dataset)
    list_ala<-org_dag(lis[[2]],lis[[1]]) 
    return(list(list_ala[[1]],list_ala[[2]],list_ala[[3]],lis[[1]],lis[[2]]))
  })
  
  output$initialtext<-renderPrint({
    list_ala<-traininitial()
    print(list_ala[[3]])
    cat("\n")
    print(list_ala[[2]])
    cat("\n")
    cat("初始网络构造完成!\n")
    cat("\n")
  })
  
  output$initialNet<-renderPlot({
    list_ala<-traininitial()
    graphviz.plot(list_ala[[1]], shape = "ellipse")
  })
  
  
  output$experimenttext<-renderPrint({
    cat("   数据集:",input$dataset,"\n")
    cat("   评分函数:",input$score_funcation,"\n")
    cat("   阈值设置:",input$threshold,"\n")
    cat("   训练次数:",input$train_number,"\n")
    cat("   Alpha(α)值:",input$aerfa,"\n")
    cat("   N次平均结果:",input$avg,"\n") 
    cat("   数据样本量大小:",input$yangbenliang,"\n")
    cat("   本次对比的算法有:\n")
    if (input$EFBNSL==TRUE){
      cat("    |EFBNSL|")
    }
    if(input$PC==TRUE){
      cat("    |PC|")
    }
    if(input$GS==TRUE){
      cat("    |GS|")
    }
    if(input$IAMB==TRUE){
      cat("    |IAMB|")
    }
    if(input$HC==TRUE){
      cat("    |HC|")
    }
    if(input$MMHC==TRUE){
      cat("    |MMHC|")
    }
    if(input$TABU==TRUE){
      cat("    |TABU|")
    }
    cat("\n\n")
  })
  
  trainBN<-eventReactive(input$trainNetworkButton,{
    list_haha_result<-list()
    list_ala<-traininitial()
    if(input$EFBNSL==TRUE){
      node_num<-ncol(list_ala[[5]])
      list_efbnsl<-ebnsl(list_ala[[5]],list_ala[[1]],input$yangbenliang,1000,input$score_funcation,list_ala[[4]],list_ala[[2]],1,node_num);
      list_efbnsl<-c(list_efbnsl,"EFBNSL")
      list_haha_result<-c(list_haha_result,efbnsl=list_efbnsl)
    }
    if (input$PC==TRUE){
      list_other_pc<-cal(list_ala[[5]],input$yangbenliang,list_ala[[4]],"pc")
      list_other_pc<-c(list_other_pc,"PC")
      list_haha_result<-c(list_haha_result,pc=list_other_pc)
    }
    if(input$GS==TRUE){
      list_other_gs<-cal(list_ala[[5]],input$yangbenliang,list_ala[[4]],"gs")
      list_other_gs<-c(list_other_gs,"GS")
      list_haha_result<-c(list_haha_result,gs=list_other_gs)
    }
    if(input$IAMB==TRUE){
      list_ala<-traininitial()
      list_other_iamb<-cal(list_ala[[5]],input$yangbenliang,list_ala[[4]],"iamb")
      list_other_iamb<-c(list_other_iamb,"IAMB")
      list_haha_result<-c(list_haha_result,iamb=list_other_iamb)
    }
    if(input$HC==TRUE){
      list_ala<-traininitial()
      list_other_hc<-cal(list_ala[[5]],input$yangbenliang,list_ala[[4]],"hc")
      list_other_hc<-c(list_other_hc,"HC")
      list_haha_result<-c(list_haha_result,hc=list_other_hc)
    }
    if(input$MMHC==TRUE){
      list_ala<-traininitial()
      list_other_mmhc<-cal(list_ala[[5]],input$yangbenliang,list_ala[[4]],"mmhc")
      list_other_mmhc<-c(list_other_mmhc,"MMHC")
      list_haha_result<-c(list_haha_result,mmhc=list_other_mmhc)
    }
    if(input$TABU==TRUE){
      list_ala<-traininitial()
      list_other_tabu<-cal(list_ala[[5]],input$yangbenliang,list_ala[[4]],"tabu")
      list_other_tabu<-c(list_other_tabu,"TABU")
      list_haha_result<-c(list_haha_result,tabu=list_other_tabu)
    }
    return(list_haha_result)
  })
  

  
  output$resulttext<-renderPrint({
    list_haha_result<-trainBN();
    if(input$EFBNSL==TRUE){
      cat("      ",list_haha_result$efbnsl10,"算法:   F1值:",list_haha_result$efbnsl1[[1]],"\t汉明距离(HD):",list_haha_result$efbnsl1[[2]], "\t正确边数(TP):",list_haha_result$efbnsl1[[3]],'\n');
    }
    if(input$PC==TRUE){
      cat("\n")
      cat("      ",list_haha_result$pc4,"算法:   F1值:",list_haha_result$pc1,"\t汉明距离(HD):",list_haha_result$pc2, "\t正确边数(TP):",list_haha_result$pc3 ,'\n');
    }
    if(input$GS==TRUE){
      cat("\n")
      cat("      ",list_haha_result$gs4,"算法:   F1值:",list_haha_result$gs1,"\t汉明距离(HD):",list_haha_result$gs2, "\t正确边数(TP):",list_haha_result$gs3,'\n');
    }
    if(input$IAMB==TRUE){
      cat("\n")
      cat("      ",list_haha_result$iamb4,"算法:   F1值:",list_haha_result$iamb1,"\t汉明距离(HD):",list_haha_result$iamb2, "\t正确边数(TP):",list_haha_result$iamb3,'\n');
    }
    
    if(input$HC==TRUE){
      cat("\n")
      cat("      ",list_haha_result$hc4,"算法:   F1值:",list_haha_result$hc1,"\t汉明距离(HD):",list_haha_result$hc2, "\t正确边数(TP):",list_haha_result$hc3,'\n');
    }
    if(input$MMHC==TRUE){
      cat("\n")
      cat("      ",list_haha_result$mmhc4,"算法:   F1值:",list_haha_result$mmhc1,"\t汉明距离(HD):",list_haha_result$mmhc2, "\t正确边数(TP):",list_haha_result$mmhc3,'\n');
    }
    if(input$TABU==TRUE){
      cat("\n")
      cat("      ",list_haha_result$tabu4, "算法:   F1值:",list_haha_result$tabu1,"\t汉明距离(HD):",list_haha_result$tabu2, "\t正确边数(TP):",list_haha_result$tabu3,'\n');
    }
  })
  
  output$resultf1<-renderPlot({
    list_result<-trainBN();
    if(input$EFBNSL==TRUE){
      plot(list_result$efbnsl2,list_result$efbnsl3,xlab='Epoch',ylab='F1',type="o",col='red',pch=c(16));
    }
  })
  
  output$resulthd1<-renderPlot({
    list_result<-trainBN();
    if(input$EFBNSL==TRUE){
      pigname<-str_c(input$dataset,"(",input$yangbenliang*20000,")");
      plot(list_result$efbnsl2,list_result$efbnsl4,xlab='Epoch',ylab='HD',type="o",col='blue',pch=c(3),sub= pigname);
    }
  })
  
  output$resulttp1<-renderPlot({
    list_result<-trainBN();
    if(input$EFBNSL==TRUE){
     plot(list_result$efbnsl2,list_result$efbnsl5,xlab='Epoch',ylab='TP',type="o",col='black',pch=c(8));
    }
  })
  
  output$resultbn1<-renderPlot({
    list_result<-trainBN();
    if(input$EFBNSL==TRUE){
      graphviz.plot(list_result$efbnsl6,highlight = list(arcs = arcs(list_result$efbnsl7)));
    }
  })
  
  output$show_compare_f1<-renderPlot({
    list_haha_result<-trainBN();
    show_f1_list<-list();
    show_algorithm<-list();
    if(input$EFBNSL==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$efbnsl1[[1]])
      show_algorithm<-c(show_algorithm,list_haha_result$efbnsl10)
    }
    if(input$PC==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$pc1)
      show_algorithm<-c(show_algorithm,list_haha_result$pc4)
    }
    if(input$GS==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$gs1)
      show_algorithm<-c(show_algorithm,list_haha_result$gs4)
    }
    if(input$IAMB==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$iamb1)
      show_algorithm<-c(show_algorithm,list_haha_result$iamb4)
    }
    
    if(input$HC==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$hc1)
      show_algorithm<-c(show_algorithm,list_haha_result$hc4)
    }
    if(input$MMHC==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$mmhc1)
      show_algorithm<-c(show_algorithm,list_haha_result$mmhc4)
    }
    if(input$TABU==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$tabu1)
      show_algorithm<-c(show_algorithm,list_haha_result$tabu4)
    }
    show_f1_list<-as.numeric(show_f1_list)
    show_algorithm<-as.character(show_algorithm)
    show_data<-cbind(show_algorithm,show_f1_list)
    show_data_data<-as.data.frame(show_data)
    barplot(show_f1_list,names.arg=show_algorithm,xlab="Algorithm",ylab="F1",col=rainbow(9),legend = rownames(show_data_data),legend.text = show_algorithm,args.legend = list(x = "topleft"),width = 1,
            main="Performance of different algorithms with different datasets with F1-score evaluation metrics")
  })
  
  output$show_compare_hd<-renderPlot({
    list_haha_result<-trainBN();
    show_f1_list<-list();
    show_algorithm<-list();
    if(input$EFBNSL==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$efbnsl1[[2]])
      show_algorithm<-c(show_algorithm,list_haha_result$efbnsl10)
    }
    if(input$PC==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$pc2)
      show_algorithm<-c(show_algorithm,list_haha_result$pc4)
    }
    if(input$GS==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$gs2)
      show_algorithm<-c(show_algorithm,list_haha_result$gs4)
    }
    if(input$IAMB==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$iamb2)
      show_algorithm<-c(show_algorithm,list_haha_result$iamb4)
    }
    
    if(input$HC==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$hc2)
      show_algorithm<-c(show_algorithm,list_haha_result$hc4)
    }
    if(input$MMHC==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$mmhc2)
      show_algorithm<-c(show_algorithm,list_haha_result$mmhc4)
    }
    if(input$TABU==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$tabu2)
      show_algorithm<-c(show_algorithm,list_haha_result$tabu4)
    }
    show_f1_list<-as.numeric(show_f1_list)
    show_algorithm<-as.character(show_algorithm)
    show_data<-cbind(show_algorithm,show_f1_list)
    show_data_data<-as.data.frame(show_data)
    barplot(show_f1_list,names.arg=show_algorithm,xlab="Algorithm",ylab="HD",col=rainbow(9),legend = rownames(show_data_data),legend.text = show_algorithm,args.legend = list(x = "topleft"),width = 1,
            main="Performance of different algorithms with different datasets with HD evaluation metrics")
  })
  
  output$show_compare_tp<-renderPlot({
    list_haha_result<-trainBN();
    show_f1_list<-list();
    show_algorithm<-list();
    if(input$EFBNSL==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$efbnsl1[[3]])
      show_algorithm<-c(show_algorithm,list_haha_result$efbnsl10)
    }
    if(input$PC==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$pc3)
      show_algorithm<-c(show_algorithm,list_haha_result$pc4)
    }
    if(input$GS==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$gs3)
      show_algorithm<-c(show_algorithm,list_haha_result$gs4)
    }
    if(input$IAMB==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$iamb3)
      show_algorithm<-c(show_algorithm,list_haha_result$iamb4)
    }
    
    if(input$HC==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$hc3)
      show_algorithm<-c(show_algorithm,list_haha_result$hc4)
    }
    if(input$MMHC==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$mmhc3)
      show_algorithm<-c(show_algorithm,list_haha_result$mmhc4)
    }
    if(input$TABU==TRUE){
      show_f1_list<-c(show_f1_list,list_haha_result$tabu3)
      show_algorithm<-c(show_algorithm,list_haha_result$tabu4)
    }
    show_f1_list<-as.numeric(show_f1_list)
    show_algorithm<-as.character(show_algorithm)
    show_data<-cbind(show_algorithm,show_f1_list)
    show_data_data<-as.data.frame(show_data)
    barplot(show_f1_list,names.arg=show_algorithm,xlab="Algorithm",ylab="TP",col=rainbow(9),legend = rownames(show_data_data),legend.text = show_algorithm,args.legend = list(x = "topleft"),width = 1,
            main="Performance of different algorithms with different datasets with TP evaluation metrics")
  })
  
  output$expre_process<-renderPrint({
    cat("初始网络在学习过程中的变化情况如下:\n\n")
    list_ala<-traininitial();
    list_result<-trainBN();
    cat("\n")
    cat("贝叶斯网络学习迭代过程如下:\n")
    cat("\n")
    for(i in 1:length(list_result$efbnsl2)){
      cat("      epoch:",list_result$efbnsl2[[i]],"\tfscore:",list_result$efbnsl3[[i]],'\thd:',list_result$efbnsl4[[i]],'\ttp:',list_result$efbnsl5[[i]],'\n');
    }
    
 })
  
}

shinyApp(ui, server)


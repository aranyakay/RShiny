library(shinydashboard)
library(shiny)
library(shinyjs)
library(lattice)
library(ggplot2)
library(gsubfn)
library(merTools)
library(googleVis)
library(scales)
library(DT)


######################## Prepare data to be displayed ##########################
# load(paste("./www/","model_all_0201.rda",sep=""))

datapath <- ""
load(paste(datapath,"Raw Data/ModelTest/model_all_0207.rda",sep=""))

varClean <- function(model){
  y<-summary(model)$coeff[-1,4] < 0.1
  y<- as.data.frame(y)
  y$names <- rownames(y)
  y<- y[y$y==T,]
  names <-  strsplit(y$names,':')
  names2 <- unique(unlist(names))
  return(names2)
}

roundx <- function(n){
  n=ifelse(n>10|n==1, round(n,0), ifelse(n<2, round(n,2), round(n,1)))
  return(n)
}

showPct <- function(n,name){
  if(grepl("IIHS",name, ignore.case = TRUE)){
    n0<-roundx(n)
    }else{
      n0<- ifelse(grepl("%",name) & n<1, round(as.numeric(n)*100,1),roundx(n))
      }
  return(n0)
  }


rescale <- function(name){
  var15[,name]<- (input[[name]]*total4.17[,name]-
                    mean(total4.17[,name]))/sd(total4.17[,name])
}
pe <- varClean(mod_PE); mo <- varClean(mod_MO); pv <- varClean(mod_PV); 
pv1 <- varClean(mod_PV1); pv2 <- varClean(mod_PV2); pv3 <- varClean(mod_PV3);
pis <- varClean(mod_IS); 


sldRange[sldRange$variable=="iihs_all", "max"] <- 
  round(sldRange[sldRange$variable=="iihs_all", "max"], 2)

v2016Raw$fatality_crossing2 <- c(146, 6,128,70,455,101,34,19,3,549,256,10,32,215,
                                 150,89,79,111,98,21,70,44,232,81,84,128,18,52,52,
                                 10,102,46,148,184,19,223,111,49,169,4,159,9,136,
                                 519,45, 5,110,96,20,124,9)
v2016Raw$label <- label
v2016Raw$GDP_all <- v2016Raw$GDP_all/1000
total4.18<- rbind(total4.17[,colnames(v2016Raw)], v2016Raw)

list_states <- unique(total4.17[c("label", "StateName")])


################################################################################
#datatable heatmap format
brks <- quantile(seq(-100,100), probs = seq(.05, .95, .05), na.rm = TRUE)
brks <- c(brks[1:((length(brks)-1)/2)],brks[(length(brks)+1)/2],brks[((length(brks)+1)/2):length(brks)])
clrs1 <- round(seq(245, 40, length.out = length(which(brks>0) )+ 1), 0) %>%
{paste0("rgb(255,", ., ",", ., ")")}
clrs2 <- round(seq(245, 40, length.out = length(which(brks<0)) + 1), 0) %>%
{paste0("rgb(", ., ",255,", ., ")")}
clrs <-c(rev(clrs2),"rgb(255,255,255),",clrs1)

################################################################################
shinyServer(function(input, output, session) {
###################################
  observe({
##########################  swich model list
    model_list <- req(input$ModelInput)
    if (model_list == "Passenger Vehicle - IIHS") {ModelList = pv1
    }else if (model_list == "Passenger Vehicle - ESC"){ ModelList = pv2
    }else if (model_list == "Passenger Vehicle - NCAP") { ModelList = pv3
    }else if (model_list == "Pedestrian") {ModelList = pe
    }else if (model_list == "Motorcycle") {ModelList = mo
    }else {ModelList = pis}
##########################   add action buttons
    output$buttons <- renderUI({
      new_label <- unique(varAll[,c("variable","label_short")])
      dashboard_buttons <- function(id, label=id) {
        btn_label <- new_label[new_label$variable==id,"label_short"]
        ns <- NS(id)
        actionButton(
          inputId = ns("select")
          ,label = btn_label
          ,style="width: 87%;"
          )
        }
      tagList(
        lapply(ModelList, dashboard_buttons)
        )
      })
##########################  add sliders
    slidertype <- reactiveValues()
    slidertype$type <- "original"
    observeEvent(input$button3, {slidertype$type <- "ff"})
##########################     
    
    output$sliders <- renderUI({
      sliderOut <- function(id, label=id) {
        if(input$stateInput == "National"){
          slid_arguments <- varAll[varAll$variable==id,]
          preset <- ifelse(varAll$range[varAll$variable==id]==1,0.5,1)
          presetFF <- 
            ifelse(varAll$range[varAll$variable==id]==1,0.5,
                   1+varAll$ff[varAll$variable==id])
          ns <- NS(id)
          disabled(
            if(slidertype$type=="original"){
            sliderInput(
              inputId = ns("slid"),
              label = paste0(slid_arguments$label_short,
                             " (",
                             slid_arguments$units,
                             ")"),
              post = "",
              ticks = F,
              min = slid_arguments$min,
              max = slid_arguments$max,
              value = preset,
              step = 0.01
            )}else{
              sliderInput(
                inputId = ns("slid"),
                label = paste0(slid_arguments$label_short,
                               " (",
                               slid_arguments$units,
                               ")"),
                post = "",
                ticks = F,
                min = slid_arguments$min,
                max = slid_arguments$max,
                value = presetFF,
                step =ifelse(varAll$range[varAll$variable==id]==0,0.02,
                             ifelse(varAll$range[varAll$variable==id]==1,0.5,0.001))
              )
            }
        )
        }else{
          slid_arguments <- sldRange[sldRange$variable==id
                                     & sldRange$state==input$stateInput,]
          new_label <- 
            unique(varAll[,c("variable", "label_short", "units")])
          slid_arguments <- merge(slid_arguments, new_label, by = "variable")
          preset <- ifelse(varAll$range[varAll$variable==id]==1,0.5,1)
          presetFF <- ifelse(varAll$range[varAll$variable==id]==1, 0.5,
                             1+sldRange$ff[sldRange$variable==id
                                         & sldRange$state==input$stateInput]) 
          ns <- NS(id)
          disabled(
            if(slidertype$type=="original"){
            sliderInput(
              inputId = ns("slid"),
              label = paste0(slid_arguments$label_short,
                             " (",
                             slid_arguments$units,
                             ")"),
              post = "",
              ticks = F,
              min = slid_arguments$min,
              max = slid_arguments$max,
              value = preset,
              step = 0.01
            )}else{
              sliderInput(
                inputId = ns("slid"),
                label = paste0(slid_arguments$label_short,
                               " (",
                               slid_arguments$units,
                               ")"),
                post = "",
                ticks = F,
                min = slid_arguments$min,
                max = slid_arguments$max,
                value = presetFF,
                step =ifelse(varAll$range[varAll$variable==id]==0,0.02,
                             ifelse(varAll$range[varAll$variable==id]==1,0.5,0.001))
              )
            }
          )

        }
      }
      tagList(lapply(ModelList, sliderOut))
    })
#####################################################################################################################################################    

########################## disable and enable base to selected button 
    observeEvent(input$ModelInput, {
      output$plot.ui <- renderUI({plotOutput("plot_before")})
      })
    
    lapply(ModelList, FUN=function(i){ ns <- NS(i)
    observeEvent(input[[ns("select")]],{
      lapply(ModelList, FUN=function(j){ ns <- NS(j)
      disable(ns("slid"))
      })
      enable(ns("slid"))
      show(
      output$reset <- renderUI({
        validate(
          need(rv$lastBtn, " "),
          need(input$stateInput, " ")
        )
        reset_buttons <- actionButton(inputId = ns("reset") 
                                      ,label = "Reset"
                                      ,style="padding:4px; width: 100%;"
                                      )
        })
      )
        output$plot.Attb <- renderUI({plotOutput("plot")})
      })
    })
##########################     
    output$reset_all <- renderUI({
      reset_all_button <- actionButton(inputId = "reset_all"
                                       ,label = "Reset All"
                                       ,style = "padding:4px; width: 100%;"
                                       )
      })
##########################  reset 
    lapply(ModelList, FUN=function(i){ ns <- NS(i)
    observeEvent(input[[ns("reset")]],{reset(ns("slid"))})
    })
##########################  reset all
    lapply(ModelList, FUN=function(i){ ns <- NS(i)
    observeEvent(input$reset_all, {
      reset(ns("slid"))
      slidertype$type <- "original"
      })
    })
#####################################################################################################################################################   
    tableCal <- function(model,region,output){
      var15 <- var2015[511:561,]
      ori <- total4.17[511:561,]
      var15Re <- var2015[511:561,]
      var15Re$Time <- var15Re$Time+1
      
      
      if(model == "Passenger Vehicle - IIHS"){
        x0 <- exp(predict(mod_PV1, var15))
        ori$expFatalVeh <- round(x0,0)
        for(name in pv1){
          for(i in 1:51){
            validate(need(input[[paste0(name,"-slid")]], " "))
            var15Re[i,name]<-(input[[paste0(name,"-slid")]]*ori[i,name]-mean(total4.17[,name]))/sd(total4.17[,name])
          }}
        ori$expFatalVeh2 <- round(exp(predict(mod_PV1, var15Re)),0)
        
        outTable0<- c("National", sum(total4.17$fatality_veh[total4.17$Year==2015]),
                      sum(v2016Raw$fatality_veh))
        x2 <-ori$expFatalVeh2-ori$expFatalVeh
        x2<-round(x2,0)
        x3=23680
        outTable2<-cbind(as.character(label), ori$fatality_veh, v2016Raw$fatality_veh, 
                         ori$fatality_veh+x2, x2)
      }
      if(model == "Passenger Vehicle - ESC"){
        x0 <- exp(predict(mod_PV2, var15))
        ori$expFatalVeh <- round(x0,0)
        for(name in pv2){
          for(i in 1:51){
            validate(need(input[[paste0(name,"-slid")]], " "))
            var15Re[i,name]<-(input[[paste0(name,"-slid")]]*ori[i,name]-mean(total4.17[,name]))/sd(total4.17[,name])
          }}
        
        ori$expFatalVeh2 <- round(exp(predict(mod_PV2, var15Re)),0)
        outTable0<- c("National", sum(total4.17$fatality_veh[total4.17$Year==2015]),
                      sum(v2016Raw$fatality_veh ))
        x2 <-ori$expFatalVeh2-ori$expFatalVeh
        x2<-round(x2,0)
        x3=23745
        outTable2<-cbind(as.character(label),  ori$fatality_veh, v2016Raw$fatality_veh, ori$fatality_veh+x2, x2)
      }
      if(model == "Passenger Vehicle - NCAP"){
        x0 <- exp(predict(mod_PV3, var15))
        ori$expFatalVeh <- round(x0,0)
        for(name in pv3){
          for(i in 1:51){
            validate(need(input[[paste0(name,"-slid")]], " "))
            var15Re[i,name]<-(input[[paste0(name,"-slid")]]*ori[i,name]-mean(total4.17[,name]))/sd(total4.17[,name])
          }}
        
        #x11 <- exp(predictInterval(mod_PV3, var15Re, n.sims = 999, level=0.025))
        ori$expFatalVeh2 <- round(exp(predict(mod_PV3, var15Re)),0)
        outTable0<- c("National", sum(total4.17$fatality_veh[total4.17$Year==2015]),
                      sum(v2016Raw$fatality_veh ))
        x2 <-ori$expFatalVeh2-ori$expFatalVeh
        x2<-round(x2,0)
        x3=23078
        outTable2<-cbind(as.character(label),  ori$fatality_veh, v2016Raw$fatality_veh, ori$fatality_veh+x2, x2)
      }
      if(model == "Pedestrian"){
        x0 <- exp(predict(mod_PE, var15))
        ori$expFatalVeh <- round(x0,0)
        for(name in pe){
          for(i in 1:51){
            validate(need(input[[paste0(name,"-slid")]], " "))
            var15Re[i,name]<-(input[[paste0(name,"-slid")]]*ori[i,name]-mean(total4.17[,name]))/sd(total4.17[,name])
          }}
        ori$expFatalVeh2 <- round(exp(predict(mod_PE, var15Re)),0)
        outTable0<- c("National", sum(total4.17$fatality_ped[total4.17$Year==2015]),
                      sum(v2016Raw$fatality_ped ))
        x2 <-ori$expFatalVeh2-ori$expFatalVeh
        x2<-round(x2,0)
        x3=5672
        outTable2<-cbind(as.character(label),  ori$fatality_ped, v2016Raw$fatality_ped, ori$fatality_ped+x2, x2)
      }
      if(model == "Intersection"){
        x0 <- exp(predict(mod_IS, var15))
        ori$expFatalVeh <- round(x0,0)
        for(name in pis){
          for(i in 1:51){
            validate(need(input[[paste0(name,"-slid")]], " "))
            var15Re[i,name]<-(input[[paste0(name,"-slid")]]*ori[i,name]-mean(total4.17[,name]))/sd(total4.17[,name])
          }}
        ori$expFatalVeh2 <- round(exp(predict(mod_IS, var15Re)),0)
        outTable0<- c("National", sum(total4.17$fatality_crossing2[total4.17$Year==2015]),
                      sum(v2016Raw$fatality_crossing2 ))
        x2 <-ori$expFatalVeh2-ori$expFatalVeh
        x2<-round(x2,0)
        x3=5693
        outTable2<-cbind(as.character(label), ori$fatality_crossing2, v2016Raw$fatality_crossing2, ori$fatality_crossing2+x2, x2)
      }
      if(model == "Motorcycle"){
        x0 <- exp(predict(mod_MO, var15))
        ori$expFatalVeh <- round(x0,0)
        for(name in mo){
          for(i in 1:51){
            if(varAll$range[varAll$variable==name]==1){
              if(input[[paste0(name,"-slid")]]==0.5){
                validate(need(input[[paste0(name,"-slid")]], " "))
                var15Re[i,name]<-var15Re[i,name]
              }else{
                validate(need(input[[paste0(name,"-slid")]], " "))
                var15Re[i,name]<-input[[paste0(name,"-slid")]]}
            }else{
              validate(need(input[[paste0(name,"-slid")]], " "))
              var15Re[i,name]<-(input[[paste0(name,"-slid")]]*ori[i,name]-mean(total4.17[,name]))/sd(total4.17[,name])
            }}}
        ori$expFatalVeh2 <- round(exp(predict(mod_MO, var15Re)),0)
        outTable0<- c("National", sum(total4.17$motor_fatal[total4.17$Year==2015]),
                      sum(v2016Raw$motor_fatal))
        x2 <-ori$expFatalVeh2-ori$expFatalVeh
        x2<-round(x2,0)
        x3=5178
        outTable2<-cbind(as.character(label), ori$motor_fatal, v2016Raw$motor_fatal,
                         ori$motor_fatal+x2, x2)
      }
      
      
      outTable2 <- as.data.frame(outTable2)
      outTable2[] <- lapply(outTable2, as.character)
      outTable22 <- outTable2
      outTable2[,5] <-paste0(outTable2[,5]," (",round(100*as.numeric(outTable2[,5])/as.numeric(outTable2[,3]),1),"%)")
      outTable0 <- c(outTable0, as.numeric(outTable0[2])+sum(x2), sum(x2), as.numeric(outTable0[2])+sum(x2)-x3)
      outTable0[5] <-paste0(outTable0[5]," (",round(100*as.numeric(outTable0[5])/as.numeric(outTable0[2]),1),"%)")   
      # outTable0[6] <- paste0("<strong><span style='color:", 
      #                        ifelse(outTable0[6] <0, "LimeGreen'>",ifelse(outTable0[6]>0,"red'>", "black'>")),
      #                        outTable0[6], "</span></strong>")   
      
      outTable3 <- outTable2[outTable2[1]==region,]
       outTable3 <- rbind(#c("<strong>Region</strong>","<strong>2015</strong>",
      #                      "<strong>2016</strong>",
      #                      "<strong>2016<br>Forecasted</strong>",
      #                      "<strong>Change from<br>2015 to 2016</strong>",
      #                      "<strong>Difference<br>vs. forecasted</strong>"),
                           outTable0, outTable3)
      
      outTable0 <- rbind(#c("<strong>Region</strong>","<strong>2015</strong>",
                           # "<strong>2016</strong>",
                           # "<strong>2016<br>Forecasted</strong>",
                           # "<strong>Change from<br>2015 to 2016</strong>",
                           # "<strong>Difference<br>vs. forecasted</strong>"),
                          outTable0)
      
      outTable0<-as.data.frame(outTable0)
      outTable3<-as.data.frame(outTable3)
      
      
      tabTitle2 <- paste0(#"<strong>",
                          rep(c("State", "Change: 2015 to est.2016"),3),
                          #"</strong>",
                          sep="")
      outTable22 <- cbind(outTable22[,1], outTable2[,5],outTable22[,5])
      #outTable4<- cbind(outTable2[1:17,c(1,5)],outTable2[18:34,c(1,5)],outTable2[35:51,c(1,5)])
      outTable4<- cbind(outTable22[1:17,],outTable22[18:34,],outTable22[35:51,])
      #outTable4 <- rbind( tabTitle2, outTable4)
      outTable4<-as.data.frame(outTable4) 
      
      if(output=="national") {
        return(outTable3)} else if(output=="state"){
          return(outTable4)
        } else if(output=="plot"){
          return(outTable2)
        }
      
    }
    
     
    #table output for national
    #table output for selected state
    output$table2 = DT:: renderDataTable (
        datatable(tableCal(input$ModelInput,input$stateInput,"national"),rownames= FALSE,
                  callback = JS("var tips = ['Geographic Region', 'Acutal 2015 fatalities', 'Acutal 2016 fatalities',
                             'Forecasted 2016 fatalities', 'Acutal 2015 fatalities vs. Forested 2016 fatalities',
                                'Current forecasted 2016 fatalities (based on slider) vs. Original forecasted 2016 fatalities (based on 2016 variables)'],
                                header = table.columns().header();
                                for (var i = 0; i < tips.length; i++) {
                                $(header[i]).attr('title', tips[i]);}"), 
                  colnames=c("Region","2015","2016","2016 Forecasted", "Change from 2015 to 2016", "Impact of Attribute Changes")[1:ncol(tableCal(input$ModelInput,input$stateInput,"national"))],
                  options=list(dom = 't',columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
                       formatStyle(6, valueColumns=6,
                       color = JS("value < 0 ? 'green' : value > 0 ? 'red' :'grey'"),
                       fontWeight = JS("value < 0 ? 'bold' : value > 0 ? 'bold' :'normal'")),
         server = FALSE, selection = 'none' )
      #E}
      # include.rownames=FALSE,include.colnames=FALSE,sanitize.text.function=function(x){x})

#####################################################################################################################################################        
    #table output for all state
    output$table3 = DT:: renderDataTable (
                              datatable(tableCal(input$ModelInput,input$stateInput,"state"),
                              callback = JS("var tips = ['State Name', 'Forecasted 2016 fatalities vs. Actual 2015 fatalities', ' ',
                                                       'State Name', 'Forecasted 2016 fatalities vs. Actual 2015 fatalities', ' ',
                                                       'State Name', 'Forecasted 2016 fatalities vs. Actual 2015 fatalities', ' '],
                                                        header = table.columns().header();
                                                        for (var i = 0; i < tips.length; i++) {
                                                        $(header[i]).attr('title', tips[i]);}"),
                              colnames= rep(c("State", "Change: 2015 to est.2016",""),3),rownames= FALSE,
                             options=list(dom = 't',pageLength = 17,autoWidth  = T,
                                          columnDefs = list(list(visible=FALSE, targets=c(2,5,8))))) %>%
                              formatStyle(1:3, valueColumns=3, backgroundColor = styleInterval(brks,clrs)) %>%
                               formatStyle(4:6, valueColumns=6, backgroundColor = styleInterval(brks,clrs)) %>%
                               formatStyle(7:9, valueColumns=9, backgroundColor = styleInterval(brks,clrs)) %>%
                               formatStyle(c(1,4,7), fontWeight='bold'),
                             server = FALSE, selection = 'none'
                             )
      #,include.rownames=FALSE,include.colnames=FALSE,sanitize.text.function=function(x){x})
    
#####################################################################################################################################################

    output$more <-  renderUI({

      boxList <- function(name){
        
        if(input$stateInput == "National"){
          validate(need(input[[paste0(name,"-slid")]], " "))
          if(name=="h0"& input[[paste0(name,"-slid")]]==0.5){
            boxValue <- ""
          }else{
            pcnt_chng <- 
              ifelse(input[[paste0(name,"-slid")]]==1 || name=="h0",
                     "",
                     paste0(" (",
                            round((input[[paste0(name,"-slid")]]*100)-100,0)
                            ,"%)")
                     )
            boxValue <- paste0(
              ifelse(varAll$range[varAll$variable==name]==1,
                             input[[paste0(name,"-slid")]],
                             showPct(input[[paste0(name,"-slid")]]*
                               varAll$value15[varAll$variable==name],
                      varAll$label[varAll$variable==name]))
            , pcnt_chng)
          }
        }else{
          pcnt_chng <- 
            ifelse(input[[paste0(name,"-slid")]]==1 || name=="h0",
                   "",
                   paste0(" (",
                          round((input[[paste0(name,"-slid")]]*100)-100,0)
                          ,"%)")
            )
          if(name=="h0" & input[[paste0(name,"-slid")]]==0.5){
            validate(need(input[[paste0(name,"-slid")]], " "))
            boxValue <- paste0(
              showPct(sldRange$value15[sldRange$variable==name & 
                                         sldRange$state==input$stateInput],
                      varAll$label[varAll$variable==name])
              ,pcnt_chng)
            
          }else{
            boxValue <- paste0(
              showPct(ifelse(varAll$range[varAll$variable==name] ==1, 
                             input[[paste0(name,"-slid")]],
                             input[[paste0(name,"-slid")]]*
                               sldRange$value15[sldRange$variable==name & 
                                                  sldRange$state==input$stateInput]),
                      varAll$label[varAll$variable==name])
              ,pcnt_chng)
          }}
        renderValueBox({
          valueBox(
                   value=tags$p(boxValue, 
                                style = "width:100%;
                                         font-size: 0.35em;
                                         text-align: center;
                                "),
                   width=NULL
                   # ,subtitle = NULL
                   ,subtitle = varAll$units[varAll$variable==name]
                    )
        })
      }
      
      tagList(
        switch(input$ModelInput,
               "Passenger Vehicle - IIHS" = lapply(pv1,boxList),
               "Passenger Vehicle - ESC" = lapply(pv2,boxList),
               "Passenger Vehicle - NCAP" = lapply(pv3,boxList),
               "Pedestrian" = lapply(pe, boxList),
               "Motorcycle" = lapply(mo, boxList),
               "Intersection" = lapply(pis, boxList)
        )
      )
    })

##########################
    rv <- reactiveValues(lastBtn = character())
    
    lapply(ModelList, FUN=function(i){ ns <- NS(i)
    observeEvent(input[[ns("select")]], {
      if (input[[ns("select")]] > 0) {
        rv$lastBtn = i 
      }
    })
    }
    )

    
    lapply(ModelList, FUN=function(i){ ns <- NS(i)
    observeEvent(input$ModelInput, {
      rv$lastBtn <- ""
      updateSelectInput(session,"stateInput",selected = "National")
      output$plot.ui <- renderUI({plotOutput("plot_before")})
      })
    })
    

########################## plot before select
    
    output$tab_name <- renderText({input$stateInput})
    # statename <- list_states$StateName[list_states$label==input$stateInput]
    output$tab_full_name <- renderText(
      ifelse(input$stateInput=="National","National",
             as.character(list_states$StateName[list_states$label==input$stateInput])
             ))

    plot_title <- 
      ifelse(grepl("Passenger", input$ModelInput, ignore.case = T), 
             "Passenger Vehicle Occupant",
             ifelse(grepl("Pedestrian", input$ModelInput, ignore.case = T),
                    "Pedestrian",
                    ifelse(grepl("Motorcycle", input$ModelInput, ignore.case = T),
                           "motor_fatal", 
                           ifelse(grepl("Intersection", input$ModelInput, ignore.case = T),
                                  "fatality_crossing2", 
                                  "Please check the data"))))

    
    plot_total <- 
      ifelse(grepl("Passenger", input$ModelInput, ignore.case = T), 
             "fatality_veh",
             ifelse(grepl("Pedestrian", input$ModelInput, ignore.case = T),
                    "fatality_ped",
                    ifelse(grepl("Motorcycle", input$ModelInput, ignore.case = T),
                           "motor_fatal", 
                           ifelse(grepl("Intersection", input$ModelInput, ignore.case = T),
                                  "fatality_crossing2", 
                                  "Please check the data"))))

        output$plot_before<-renderPlot({
          
        plotTable <-  tableCal(input$ModelInput,input$stateInput,"plot")
        #################    
        if(input$stateInput == "National"){
          
          validate(need(input$stateInput, " "))
          
          nal_tot_src <- total4.17[,c("Year", plot_total)]
          names(nal_tot_src) <- c("Year", "Total")
          nal_tot <- aggregate(Total ~ Year, nal_tot_src, sum)
          nal_tot <- rbind(nal_tot,c(2016,sum(as.numeric(plotTable[,4]))))
          
          nal_tot$color <- ifelse(nal_tot$Year==2016, 2, 1)
          
          
          ggplot(nal_tot, aes(x = Year, y = Total)) +
            geom_bar(aes (fill = as.factor(color)), position = "dodge", stat = "identity") +
            scale_fill_manual(values = c(rgb(51, 120, 162, maxColorValue = 255),
                                         rgb(134, 134, 134, maxColorValue = 255))) +
            theme(legend.position="none") +
            geom_text(data=nal_tot,
                      aes(x = Year, y = Total, label = comma(Total)),
                      vjust = -0.5, size = 5) +
            scale_x_continuous(breaks = c(sort(unique(nal_tot$Year)),
                                          length(unique(nal_tot$Year)))) +
            scale_y_continuous(labels = comma, expand = c(0.075, 0)) +
            labs(title = 
                   paste0(plot_title, 
                          " Fatality Count in the United States, Years 2005-2016 (2016 Forecasted)" )
                 , x = "Year", y = "Number of Fatalities") +
            theme(axis.text = element_text(hjust = 0.5, size = 12),
                  axis.title=element_text(size=14,face="bold"),
                  axis.title.x = element_text(margin = unit(c(15, 0, 0, 0), "pt")),
                  plot.title = element_text(hjust=0.5, size = 15, face = "bold",
                                            margin = margin(t = 10, b = 10, unit = "pt")),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank())
          
          
          
        }else{
          validate(need(input$stateInput, " "))
          
          states_tot_src <- unique(total4.17[total4.17$label==input$stateInput
                                             ,c("Year", plot_total, "StateName")])
          
          names(states_tot_src) <- c("Year", "fatality", "StateName")
          states_tot <- aggregate(fatality ~ Year, states_tot_src, sum)
          
          states_tot <- 
            rbind(states_tot,c(2016,as.numeric(plotTable[plotTable[,1]==input$stateInput,4])))
          
          states_tot$color <- ifelse(states_tot$Year==2016, 2, 1)
          
          ggplot(states_tot, aes(x = Year, y = fatality)) +
            geom_bar(aes (fill = as.factor(color)), position = "dodge", stat = "identity") +
            scale_fill_manual(values = c(rgb(51, 120, 162, maxColorValue = 255),
                                         rgb(134, 134, 134, maxColorValue = 255))) +
            theme(legend.position="none") +
            geom_text(data=states_tot,aes(x = Year, y = fatality, 
                                          label = comma(fatality)),vjust = -0.5, size = 5) +
            scale_x_continuous(breaks = c(sort(unique(states_tot$Year)),
                                          length(unique(states_tot$Year)))) +
            scale_y_continuous(labels = comma, expand = c(0.075, 0)) +
            labs(title = paste0(plot_title, " Fatality Count in State",
                                ", Years 2005-2016 (2016 Forecasted)")
                 , x = "Year", y = "Number of Fatalities", fill = NULL) +
            theme(axis.text = element_text(hjust = 0.5, size = 12),
                  axis.title=element_text(size=14,face="bold"),
                  axis.title.x = element_text(margin = unit(c(15, 0, 0, 0), "pt")),
                  plot.title = element_text(hjust=0.5, size = 15, face = "bold",
                                            margin = margin(t = 10, b = 10, unit = "pt")
                  ),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        }
        })
    
########################## plot before select    
    output$GvisPlot = renderGvis({
      if(input$stateInput == "National"){
      gvis_nal_tot <- unique(total4.18[,c("Year", "Total")])
      gvisColumnChart(gvis_nal_tot, 
                      options=list(
                        height = 500,
                        legend = "none",
                        vAxes="[{minValue:0}]",
                        colors ="['#2F86D1']"))
      }else{
        gvis_states_tot <- unique(total4.18[total4.18$label==input$stateInput
                                       ,c("Year", "fatality")])
        gvisColumnChart(gvis_states_tot, 
                        options=list(
                          height = 500,
                          legend = "none",
                          vAxes ="[{minValue:0}]",
                          colors ="['#2F86D1']"))
      }
    })
    
########################## plot after select
    
    output$plot<-renderPlot({
      new_label <- unique(varAll[,c("variable","label")])
      if(input$stateInput == "National"){
      validate(
        need(rv$lastBtn, " "),
        need(input$stateInput, " ")
      )
        plot_main <- varAll[varAll$variable==rv$lastBtn,"label_long"]
        plot_label <- varAll[varAll$variable==rv$lastBtn,"label_short"]
        plot_unit <- varAll[varAll$variable==rv$lastBtn,"units"]
        
        plot_full_label <- paste0(plot_label," (", plot_unit,")")
        
        plot_multiplier <- 
          ifelse(grepl("%",varAll$units[varAll$variable==rv$lastBtn]), 
                 ifelse(varAll$value15[varAll$variable==rv$lastBtn] <= 1, 100, 1), 
                 ifelse(grepl("Billion",varAll$units[varAll$variable==rv$lastBtn]), 
                        0.001, 1))

        tt_n <- ifelse(varAll$value15[varAll$variable==rv$lastBtn] <0.01,5,
                       ifelse(varAll$value15[varAll$variable==rv$lastBtn]<0.1,4, 
                              ifelse(varAll$value15[varAll$variable==rv$lastBtn]<1,3,2)))
        
        if(plot_multiplier==1) {
          tt <- round(aggregate(x = total4.18[,colnames(total4.18)==rv$lastBtn],
                                by = list(total4.18$Year), FUN = "mean"),tt_n)
          names(tt) <- c("Year","agg_val")
        }else{
          tt <- round(aggregate(x = total4.18[,colnames(total4.18)==rv$lastBtn],
                                by = list(total4.18$Year), FUN = "mean"),tt_n)
          names(tt) <- c("Year","agg_val")
          tt$agg_val <- round(tt$agg_val*plot_multiplier, 3)
        }

        xyplot(tt$agg_val ~ tt$Year,
             type=c("l","p"),
             tt,
             main=list(plot_main,cex=1),
             ylab= list(plot_full_label,cex=1),
             xlab=list("Year",cex=1),
             scales=list(cex=1,x=list(cex = 1, rot = 45),y=list(cex = 1)
                         ,tick.number =
                           length(unique(tt$Year)))
             )
      }else{
        validate(
          need(rv$lastBtn, " "),
          need(input$stateInput, " ")
        )
        total_var <- total4.18[total4.18$label==input$stateInput,]
        
        plot_main <- varAll[varAll$variable==rv$lastBtn,"label_long"]
        plot_label <- varAll[varAll$variable==rv$lastBtn,"label_short"]
        plot_unit <- varAll[varAll$variable==rv$lastBtn,"units"]
        
        plot_full_label <- paste0(plot_label," (", plot_unit,")")
        
        plot_multiplier <- 
          ifelse(grepl("%",varAll$units[varAll$variable==rv$lastBtn]), 
                 ifelse(varAll$value15[varAll$variable==rv$lastBtn] <= 1, 100, 1),
                 ifelse(grepl("Billion",varAll$units[varAll$variable==rv$lastBtn]), 
                        0.001, 1))
        
        
        tt_n <- ifelse(varAll$value15[varAll$variable==rv$lastBtn] <0.01,5,
                       ifelse(varAll$value15[varAll$variable==rv$lastBtn]<0.1,4, 
                              ifelse(varAll$value15[varAll$variable==rv$lastBtn]<1,3,2)))
        
        if(plot_multiplier==1) {
          state_plot <- round(total_var[,colnames(total_var)==rv$lastBtn],tt_n)
        }else{
          state_plot <- round(total_var[,colnames(total_var)==rv$lastBtn],tt_n)
          state_plot <- round(state_plot*plot_multiplier, 3)
        }
        
        
        
        
        xyplot(state_plot ~ Year,
               type=c("l","p"),
               total_var,
               main=list(plot_main,cex=1),
               ylab = list(plot_full_label,cex=1),
               xlab =list("Year",cex=1),
               scales=list(cex = 1.5, x = list(
                 cex = 1, rot = 45, tick.number =
                   length(unique(total_var[,colnames(total_var)==rv$lastBtn])))
                 ,y = list(cex = 1,rot = 45, tick.number =
                             length(unique(total_var$Year)))
                 )
               )
        }
      
    # }
      })
########################## end of observe ######################################
    })
########################## end of shinyServer ##################################
  })

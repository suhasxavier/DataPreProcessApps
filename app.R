library(shiny)
library(shinydashboard)
library(data.table)
library(shinyjs)
ui=dashboardPage(
  dashboardHeader(
    #title=imageOutput("JLLImage")
    ),
  dashboardSidebar(),
  dashboardBody(
        tabsetPanel(id="welcometab",tabPanel("Data Merge",br(),h4("This segment serves to merge and reconcile data. Please ensure the uploaded files are either a text(.txt) or a csv(.csv) file."),
                                                     fluidRow(box(h3("File 1"),width=6, height=650,fileInput("file1upload","Upload File:",accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".txt")),uiOutput("file1name"),uiOutput("column_sel1"),uiOutput("subset_check1"),uiOutput("pivot_check1"),uiOutput("pivot_cols1"),uiOutput("subsetbox1"),uiOutput("colsel1but1"),uiOutput("checkselbut1"),uiOutput("completeboxone"),uiOutput("mergeSubmit")),
                                                              box(h3("File 2"),width = 6,height = 650,uiOutput("file2upload"),uiOutput("column_sel2"),uiOutput("subset_check2"),uiOutput("pivot_check2"),uiOutput("pivot_cols2"),uiOutput("subsetbox2"),uiOutput("colsel2but1"),uiOutput("checkselbut2"))
                                                              ,uiOutput("completeboxtwo"))),
                            tabPanel("DeDuplication",fluidPage(box(br(),h4("This segment serves to deduplicate data. Please ensure the uploaded files are either a text(.txt) or a csv(.csv) file."),uiOutput("DeDeupFileSel"),uiOutput("dedup_col"),uiOutput("dedupsel_but"),uiOutput("dedup_checkbox1"),uiOutput("deDuplic")),width=15,height=650))
                    ,tabPanel("Pattern Matching"),tabPanel("Geo Coding"),shinyjs::useShinyjs()
  ) 
  
  ))
server=function(input,output)
{
#### Data Merge Logic  
data_merge_process=function(user_choice)
  {
    if(user_choice=="Data Merge")
    {
  output$JLLImage=renderImage({
    imgf="c:/users/Suhas.Xavier/Desktop/jll.png"
    return(list(src=imgf,filetype="image/png"))},deleteFile = F
  )
  options(shiny.maxRequestSize=10000000000000000000000*1024^2)
  observe({  
    if(!is.null(input$file1upload))
    {
      inpfile1=input$file1upload
      file1_data=fread(inpfile1$datapath)
      file1_dat=data.frame(file1_data)
      print("file read successfully")
      gc()
      ##print(head(file1_dat))
      file1_cols=as.character(colnames(file1_dat))
      output$file1name=renderUI(selectInput("colmer1","* Select column(s) to merge file:",choices =file1_cols,multiple = T, width=250))
      observe({
        if(!is.null(input$colmer1))
        {
          output$mergeSubmit=renderUI(actionButton("mergesub","Ok"))
          shinyjs::onclick("mergesub",{
            merge_columns=input$colmer1
            shinyjs::hide("mergesub")
          ##print(namefile1)
          ##print(namefile1)
      
      
      
      output$column_sel1=renderUI(selectInput("colsel1","* Select columns to view in output file: (Select more than 1)",choices =c("All Columns",file1_cols),multiple = T, width=250))
      #output_df1=NULL   
      observe({
        if(!is.null(input$colsel1))
        {
          output$colsel1but1=renderUI(actionButton("colsel1button1","Done"))
          print(file1_dat[,merge_columns])
          shinyjs::onclick("colsel1button1",{
          if(length(input$colsel1)==1)
          {
            if(input$colsel1=="All Columns")
            {
              output_df1=file1_dat
              #print(output_df1)
              subsetted_df1="FALSE"
            }
          }
          else{
            colvals1 = c(input$colsel1)
            col_names1=colnames(file1_dat)
           # #print(length(colvals1))
            col_index1=match(colvals1,col_names1)
           # #print(col_index1)
            output_df1=subset(file1_dat,select=col_index1)
            subsetted_df1="TRUE"
            print(subsetted_df1)
            sel_cols1=names(output_df1)
          #  #print(output_df1)
            
          }
            output$subset_check1=renderUI(checkboxInput("subcheck1","Would you like to subset this dataset with a condition?"))
            shinyjs::hide("colsel1button1")
          
          
          output$submitButtonVal=renderUI(actionButton("submitbut","Submit"))
        # once check box checked
        observe({ 
          if(!is.null(input$subcheck1))
          {
            if(input$subcheck1=="TRUE")
            {
              shinyjs::hide("box1complete")
              ##print("box was checked")
              if(subsetted_df1=="TRUE")
              {
                output$subsetbox1=renderUI(flowLayout(selectInput("subsetbox1sel","Select column: (Max 3)",choices=c(sel_cols1),multiple = T),textInput("subsetbox1text","Values(Separate by comma)")))
              }
              else
              {
                output$subsetbox1=renderUI(flowLayout(selectInput("subsetbox1sel","Select column: (Max 3)",choices=c(file1_cols),multiple = T),textInput("subsetbox1text","Values(Separate by comma)")))
              }
              
              observe({
              if(!is.null(input$subsetbox1sel))
              {
                if(!is.null(input$subsetbox1text))
                {
                output$checkselbut1=renderUI(actionButton("checkselbutton1","Done"))
                shinyjs::onclick("checkselbutton1", {
                  shinyjs::hide("checkselbutton1")
                cols_sel1=as.character(input$subsetbox1sel)
                vals1_sel1=as.character(input$subsetbox1text) 
                ##print(cols_sel1)
                ##print(vals_sel1)
                ##print(output_df1)
                print(cols_sel1)
    
                vals_sel1=strsplit(vals1_sel1,split = ",")
                print(vals_sel1)
                ## if multiple values are entered
                if(length(vals_sel1)==1)
                {
                  temp_dat=output_df1[output_df1[cols_sel1]==vals_sel1,]
                  #temp_dat=subset(output_df1,cols_sel1==vals_sel1)
                  ##print(head(temp_dat))
                  output$completeboxone=renderUI(actionButton("box1complete","Go to next file"))
                }
                if(length(vals_sel1)==2)
                {
                  print(vals_sel1[1])
                  print(vals_sel1[2])
                  temp_dat=output_df1[output_df1[cols_sel1][1]==vals_sel1[1] & output_df1[cols_sel1][2]==vals_sel1[2],]
                  #temp_dat=subset(output_df1,cols_sel1==vals_sel1)
                  ##print(head(temp_dat))
                  output$completeboxone=renderUI(actionButton("box1complete","Go to next file"))
                }
                if(length(vals_sel1)==3)
                {
                  temp_dat=output_df1[output_df1[cols_sel1][1]==vals_sel1[1] & output_df1[cols_sel1][2]==vals_sel1[2] & output_df1[cols_sel1][3]==vals_sel1[3],]
                  #temp_dat=subset(output_df1,cols_sel1==vals_sel1)
                  ##print(head(temp_dat))
                  output$completeboxone=renderUI(actionButton("box1complete","Go to next file"))
                }
                #print(temp_dat)
                })
              }
              }
            })
            }
            else
            {
              output$completeboxone=renderUI(actionButton("box1complete","Go to next file"))
            }
          }
        })
        })
       #start part 2
        shinyjs::onclick("completeboxone",{
          shinyjs::hide("box1complete")
          output$file2upload=renderUI(fileInput("fileupload2","Upload File:",accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".txt")))
          observe({
          if(!is.null(input$fileupload2))
          {
            inpfile2=input$fileupload2
            file2_dat=fread(inpfile2$datapath)
            file2_cols=as.character(colnames(file2_dat))
            print("aaahoo")
            output$column_sel2=renderUI(selectInput("colsel2","* Select columns to view in output file: (Select more than 1)",choices =c("All Columns",file2_cols),multiple = T, width=250))
            #output_df1=NULL   
            observe({
              if(!is.null(input$colsel2))
              {
                output$colsel2but1=renderUI(actionButton("colsel2button1","Done"))
                shinyjs::onclick("colsel2button1",{
                  if(length(input$colsel2)==1)
                  {
                    if(as.character(input$colsel2)=="All Columns")
                    {
                      output_df2=file2_dat
                      ##print(output_df2)
                      subsetted_df2="FALSE"
                    }
                  }
                  else{
                    colvals2 = c(input$colsel2)
                    col_names2=colnames(file2_dat)
                   # #print(length(colvals2))
                    col_index2=match(colvals2,col_names2)
                    ##print(col_index2)
                    output_df2=subset(file2_dat,select=col_index2)
                   # #print(output_df2)
                    sel_cols2=colnames(output_df2)
                    subsetted_df2="TRUE"
                                        }
                  output$subset_check2=renderUI(checkboxInput("subcheck2","Would you like to subset this dataset with a condition?"))
                  shinyjs::hide("colsel2button1")
                

                # once check box checked
                observe({ 
                  if(!is.null(input$subcheck2))
                  {
                    if(input$subcheck2=="TRUE")
                    {
                      ##print("box was checked")
                      if(subsetted_df2=="TRUE")
                      {
                        output$subsetbox2=renderUI(flowLayout(selectInput("subsetbox2sel","Select column:",choices=c(sel_cols2),multiple = T),textInput("subsetbox2text","Values(Separate by comma)")))
                      }
                      else
                      {
                        output$subsetbox2=renderUI(flowLayout(selectInput("subsetbox2sel","Select column:",choices=c(file2_cols),multiple = T),textInput("subsetbox2text","Values(Separate by comma)")))
                      }
                      observe({
                        if(!is.null(input$subsetbox2sel))
                        {
                          if(!is.null(input$subsetbox2text))
                          {
                            output$checkselbut2=renderUI(actionButton("checkselbutton2","Done"))
                            shinyjs::onclick("checkselbutton2", {
                              shinyjs::hide("checkselbutton2")
                              cols_sel2=as.character(input$subsetbox2sel)
                              vals2_sel2=as.character(input$subsetbox2text) 

                              vals_sel2=strsplit(vals2_sel2,split = ",")
                              #print(vals_sel1)
                              ## if multiple values are entered
                              if(length(vals_sel2)==1)
                              {
                                temp_dat2=output_df1[output_df1[cols_sel2]==vals_sel2,]
                                #temp_dat=subset(output_df1,cols_sel1==vals_sel1)
                                ##print(head(temp_dat))
                                output$completeboxone=renderUI(actionButton("box1complete","Go to next file"))
                              }
                              if(length(vals_sel2)==2)
                              {
                                print(vals_sel2[1])
                                print(vals_sel2[2])
                                temp_dat2=output_df1[output_df1[cols_sel2][1]==vals_sel2[1] & output_df1[cols_sel2][2]==vals_sel2[2],]
                                #temp_dat=subset(output_df1,cols_sel1==vals_sel1)
                                ##print(head(temp_dat))
                                output$completeboxone=renderUI(actionButton("box1complete","Go to next file"))
                              }
                              if(length(vals_sel2)==3)
                              {
                                temp_dat2=output_df1[output_df1[cols_sel2][1]==vals_sel2[1] & output_df1[cols_sel2][2]==vals_sel2[2] & output_df1[cols_sel2][3]==vals_sel2[3],]
                                #temp_dat=subset(output_df1,cols_sel1==vals_sel1)
                                ##print(head(temp_dat))
                                output$completeboxone=renderUI(actionButton("box1complete","Go to next file"))
                              }
                              print(temp_dat2)
                              
                            })
                          }
                        }
                      })
                      #print(tail(temp_dat2))
                      print("then this")
                     # print(tail(temp_dat))
                      print("end")
                    }
                    else
                    {
                      output$completeboxtwo=renderUI(actionButton("box2complete","Merge Data"))
                    }
                  }
                })
                })
              }
              
              
            })
            #     }
            #   }
            # })
          }
        })

        })
        #end part 2
    }
    
  })  
        })
        
        }
      })
    }
  })
    }
}

### Reroute to appropriate functions
observe({
  if(!is.null(input$welcometab))
  {
    
    tab_chosen=as.character(input$welcometab)
    print(tab_chosen)
    if(tab_chosen=="Data Merge")
    {
      print("test")
      data_merge_process("Data Merge")
    }
    if(tab_chosen=="DeDuplication")
    {
      print("test")
      data_dedup_process("Data DeDuplication")
    }
  }
})


data_dedup_process=function(user_choice)
{
  if(user_choice=="Data DeDuplication")
  {
    output$DeDeupFileSel=renderUI(fileInput("dupfilesel","Choose a file",accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".txt")))
    if(!is.null(input$dupfilesel))
    {
      inpfile3=input$dupfilesel
      file3_dat=fread(inpfile3$datapath)
      file3_data=data.frame(file3_dat)
      file3_cols=colnames(file3_data)
      output$dedup_col=renderUI(selectInput("dedupcol","Select columns to deduplicate on",choices = c(file3_cols),multiple = T,width = 250))
      observe({
        if(!is.null(input$dedupcol))
        {
          output$dedupsel_but=renderUI(actionButton("dedupselbut1","Ok"))
          shinyjs::onclick("dedupselbut1",{
            shinyjs::hide("dedupselbut1")
          dedupsel_seld=c(input$dedupcol)
          output$dedup_checkbox1=renderUI(checkboxInput("dedupcheckbox","Keep all instances of duplicates"))
          observe({
            if(!is.null(input$dedupcheckbox))
            {
              if(input$dedupcheckbox=="TRUE")
              {
                dedupcols1=match(dedupsel_seld,colnames(file3_data))
                de_dup_complete=file3_data[!duplicated(file3_data[dedupcols1]) | duplicated(file3_data[dedupcols1],fromLast = T),]
                print("good")
                print(nrow(de_dup_complete))
              }
            }
            else
            {
              dedupcols1=match(dedupsel_seld,colnames(file3_data))
              print(dedupcols1)
              output_dat_test=!duplicated(file3_data[,dedupcols1])
              de_dup_complete=file3_data[output_dat_test,]
              print(nrow(de_dup_complete))
            }
            output$deDuplic=renderUI(actionButton("dedupcomplete","Done"))
         
            
          })
          
          
          })
        }
      })
      
    }
  }
}

}
shinyApp(ui,server)
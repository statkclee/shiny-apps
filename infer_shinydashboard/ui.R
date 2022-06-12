# 0. shinyDashboard Submenu ---------------

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

ui <- dashboardPage(
  
  means_UI("module_one-mean"),
  
  dashboardHeader(title = "추론"),
  dashboardSidebar(
    sidebarMenu(
      convertMenuItem(menuItem("NHST 검정", tabName = "sub_nhst", icon = icon("map"), selected = FALSE, show = FALSE,
                               menuSubItem("평균", tabName = "nhst_workflow_mean"),
                               menuSubItem("비율", tabName = "nhst_workflow_proportion"),
                               menuSubItem("분산", tabName = "nhst_workflow_variance")), "sub_nhst")
    ),
      menuItem("평균", tabName = "test_means", icon = icon("dashboard"),
               menuSubItem("1 표본", tabName = "test_means_one"),
               menuSubItem("2 표본", tabName = "test_means_two"))    
    ),
  dashboardBody(
    
    tabItems(
      # NHST 검정작업 흐름도 ---------------------
      tabItem("nhst_workflow_mean", 
              imageOutput("nhst_workflow_mean_img")
      ),
      tabItem("nhst_workflow_proportion", 
              imageOutput("nhst_workflow_proportion_img")
      ),
      tabItem("nhst_workflow_variance", 
              imageOutput("nhst_workflow_variance_img")
      ),
      # 평균 검정 ---------------------
      tabItem("test_means_one") 
    )
  )
)

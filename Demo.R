library(data.table); library(openxlsx); library(httr) 
 
orders <- data.table::data.table(openxlsx::read.xlsx( 
  "C:/Users/SanjidRahman/Downloads/Global Superstore 2018.xlsx")) 
 
names(orders) 
dim(orders) 
summary(orders) 
 
#' Returns 
#' 
#' @param dta `data.table`, must contain at least `Order.Date`, `Sales`, `Product.ID`, and `Customer.Name` 
#' 
#' @return data.table 
#' @export 
#' 
#' @examples 
bringSegementationData <- function(dta){ 
  return(dta[, .(Recency = max(Order.Date), 
                 Frequency = length(unique(Order.Date)), 
                 Monetary = sum(Sales), 
                 Tenure = max(Order.Date) - min(Order.Date), 
                 Breadth = length(unique(Product.ID))), 
             Customer.Name] 
  ) 
} 

myPrebuildFunction <- function(x) return(sd(x))

orders[, lapply(.SD, FUN = myPrebuildFunction), by = .(Category, `Sub-Category`), .SDcols = "Sales"]
 
DBI::dbGetQuery(conn = DBI::dbConnect("my-very-creative-odbc-connection-name-on-windows", " 
MY VERY EFFICIENT MOTHER JUST SERVED US NINE PIZZAS 
                               ")) 
 
summary_data <- bringSegementationData(orders) 
 
library(ggplot2) 
ggplot2::ggplot(orders[Sales < 1000]) + ggplot2::geom_histogram(aes(Sales), bins = 50) 
 
my_tree_model <- rpart::rpart(formula = Species ~ Sepal.Length + Sepal.Width, iris)

rpart::plotcp(my_tree_model)

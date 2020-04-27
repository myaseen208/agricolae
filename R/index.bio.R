#' Biodiversity Index
#' 
#' Scientists use a formula called the biodiversity index to describe the
#' amount of species diversity in a given area.
#' 
#' method bio-diversity.  "Margalef" "Simpson.Dom" "Simpson.Div"
#' "Berger.Parker" "McIntosh" "Shannon"
#' 
#' @param data number of specimens
#' @param method Describe method bio-diversity
#' @param level Significant level
#' @param nboot size bootstrap
#' @param console output console TRUE
#' @return Index and confidence intervals.
#' @author Felipe de Mendiburu
#' @references Magurran, A.E. (1988) Ecological diversity and its measurement.
#' Princeton University Press Efron, B., Tibshirani, R. (1993) An Introduction
#' to the Boostrap. Chapman and Hall/CRC
#' @keywords univar
#' @export
#' @examples
#' 
#' library(agricolae)
#' data(paracsho)
#' # date 22-06-05 and treatment CON = application with insecticide
#' specimens <- paracsho[1:10,6]
#' output1 <- index.bio(specimens,method="Simpson.Div",level=95,nboot=100)
#' output2 <- index.bio(specimens,method="Shannon",level=95,nboot=100)
#' rbind(output1, output2)
#' 
index.bio <-
function (data, method = c("Margalef","Simpson.Dom", "Simpson.Div", "Berger.Parker",
"McIntosh","Shannon"),level = 95, nboot = 100, console=TRUE)
{
    method <- match.arg(method)
    x <- data
    if (length(x) > 1) {
    if (method == "Margalef") 
        formula1 <- expression((length(x) - 1)/log(sum(x)))
    if (method == "Simpson.Dom")
        formula1 <- expression(sum((x/sum(x))^2))
    if (method == "Simpson.Div") 
        formula1 <- expression(1 - sum((x/sum(x))^2))
    if (method == "Berger.Parker") 
        formula1 <- expression(max(x)/sum(x))
    if (method == "McIntosh") 
        formula1 <- expression((sum(x) - sqrt(sum(x^2)))/(sum(x) - 
            sqrt(sum(x))))
    if (method == "Shannon") 
        formula1 <- expression(-sum((x/sum(x)) * log((x/sum(x)),2)))
    index <- eval(formula1)
    n = length(x)
    estimador <- rep(0, n)
    for (i in 1:nboot) {
        x <- sample(data, n, replace = TRUE)
        estimador[i] <- eval(formula1)
    }
    lic = as.numeric(quantile(estimador, (1 - 0.01 * level)/2, type = 6))
    lsc = as.numeric(quantile(estimador, (1 + 0.01 * level)/2, type = 6))
    if (console) {
    cat("\nMethod:", method, "\n")
    cat("\nThe index:", index, "\n\n")
    cat(level, "percent confidence interval:\n", lic, ";", lsc, 
        "\n\n")
        }
    }
if ( length(x) > 1) return(data.frame(row.names = NULL,method=method,index=index,confidence=level,lic=lic,lsc=lsc,nboot=nboot))
else return("Error data < 2")
}

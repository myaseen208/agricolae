#' Data in greenhouse
#' 
#' Potato minituber production in greenhouse, three sets of data in potato
#' varieties with different methods: hydroponics, Aeroponic, Pots and Plant
#' beds, the unit is in grams and the number of tubers in units,
#' 
#' greenhouse is list, three tables: greenhouse1(480 obs, 5 var), yield for
#' plant, unit is grams.  greenhouse2(48 obs, 5 var), Yields of 10 plants by
#' experimental unit(grams). planting date(April 24, 2004) and harvest
#' date(July 16, 2004) and greenhouse3(480 obs, 5 var), Tubers by plants.
#' 
#' @name greenhouse
#' @docType data
#' @references - Produccion de semila de papa por hidroponia tecnica de flujo
#' continuo de una pelicula de solucion nutritiva (nft) Carlos
#' Chuquillanqui(CIP), Jorge Tenorio(CIP) and L. F. Salazar(Agdia Inc).
#' AGROENFOQUE Lima-Peru (2004) - Potato Minituber Production Using Aeroponics:
#' Effect of Plant Density and Harvesting Intervals American Journal of Potato
#' Research, Jan/Feb 2006 by Farran, Imma, Mingo-Castel, Angel M
#' @source International Potato Center(CIP). Lima-Peru. Data Kindly provided by
#' Carlos Chuquillanqui.
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(greenhouse)
#' greenhouse1 <- greenhouse$greenhouse1
#' greenhouse2 <- greenhouse$greenhouse2
#' greenhouse3 <- greenhouse$greenhouse3
#' 
NULL
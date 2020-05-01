#' @title Data of Paracsho biodiversity
#' @name   paracsho
#' @docType data
#' @keywords datasets
#' @usage data(paracsho)
#' @description
#' A locality in Peru. A biodiversity.
#' 
#' Country Peru, Deparment Junin, province Tarma, locality Huasahuasi.
#' 
#' @format 
#' A \code{data.frame} with 110 observations on the following 6 variables.
#' 
#'  @details
#'         \itemize{
#'         \item{\strong{date}} a factor with levels
#'         15-12-05, 17-11-05, 18-10-05, 20-09-05, 22-06-05, 23-08-05, and 28-07-05
#'         \item{\strong{plot}} a factor with levels PARACSHO
#'         \item{\strong{Treatment}}  a factor with levels CON and SIN
#'         \item{\strong{Orden}}  a factor with levels COLEOPTERA, DIPTERA, 
#'               HEMIPTERA, HYMENOPTERA, LEPIDOPTERA, NEUROPTERA, NEUROPTERO, and NOCTUIDAE
#'         \item{\strong{Family}} a factor with levels AGROMYZIDAE, ANTHOCORIDAE, ANTHOMYIIDAE,
#'               ANTHOMYLIDAE, BLEPHAROCERIDAE, BRACONIDAE, BROCONIDAE, CALUPHORIDAE, 
#'               CECIDOMYIDAE, CHENEUMONIDAE, CHNEUMONIDAE, CHRYOMELIDAE, CICADELLIDAE,
#'               CULICIDAE, ERIOCPAMIDAE, HEMEROBIIDAE, ICHNEUMONIDAE, LOUCHAPIDAE, MIRIDAE,
#'               MUSCIDAE, MUSICADAE, MUSLIDAE, MYCETOPHILIDAE, MYCETOPHILIIDAE, NENPHALIDAE,
#'               NOCLUIDAE, NOCTERIDAE, NOCTUIDAE, PERALIDAE, PIPUNCULIDAE, PROCTOTRUPIDAE,
#'               PSYLLIDAE, PYRALIDAE, SARCOPHAGIDAE, SARCOPILAGIDAE, SCATOPHAGIDAE, 
#'               SCATOPHOGIDAE, SCIARIDAE, SERSIDAE, SYRPHIDAE, TACHINIDAE, and TIPULIDAE
#'         \item{\strong{Number.of.specimens}}     Number.of.specimens
#'         }
#'         
#' @references 
#' International Potato Center.
#' 
#' @source 
#' Entomology dataset.
#' 
#' @keywords datasets
#' @examples
#' 
#' library(agricolae)
#' data(paracsho)
#' str(paracsho)
#' 
NULL
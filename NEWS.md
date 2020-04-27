# agricolae 1-3.2 (January 18, 2020)
## New Features 

* The examples were edited. 
* A control is set on the label limit for the functions order.group and orderPvalue.
* The plot.AMMI function uses only type = 1 and type = 2 options.

# agricolae 1-3.1 (April 4, 2019)
## New Features 

* Improvement in the formation of Duncan test groups and the confidence limits of Scheffe.test.
* Shannon index on base log 2.
* Update documentation on multiple comparison tests.

# agricolae 1-2.9 (January 4, 2019)
## New Features 

* Plot.group function include argument label size with cex=NULL
HSD.test include parameter unbalanced, equal TRUE is not equal replication

# agricolae 1-2.8 (September 12, 2017)
## New Features 

* The order.group function  is again in agricolae. It is equivalent to the orderPvalue function in functional terms.

# agricolae 1-2.7 (August 30, 2017)
## New Features 

* In the post.hoc tests, the grouping of treatments are formed according to the probability of the difference between treatments and the alpha level. The affected functions were BIB.test, DAU.test, duncan.test, durbin.test, friedman, HSD.test, kruskal, LSD.test, Median.test, PBIB.test, REGW.test,  scheffe.test, SNK. Test, waller.test and waerden.test. Now there is good correspondence between the grouping and the pvalue. 


* A new function (plot.group) is included in agricolae for the graphs of treatment groups and their variation by range, interquartil range, Standard deviation and standard error.
* The RANN package of suggestions was removed.
* Updated documentation. 

# agricolae 1-2.6 (August 4, 2017)
## New Features 

* Documentation check.

# agricolae 1-2.5 (July 20, 2017)
## New Features 
*  Add model object in output PBIB.test function.
*  procedure duncan.test is better, the limitations in convergence were corrected.
*  The influence in AMMI (type=3) is relative neighbor graph as a sub-graph.
*  The post hoc nonparametrics tests (kruskal, friedman, durbin and waerden) are using the criterium Fisher's least significant difference (LSD)

# agricolae 1-2.4 (June 12, 2016)
## New Features 

* Add suggests packages: RANN and rgeos to plot AMMI
* Concordance index in correlation function(), additional arg (method="lin").
* New function orderPvalue(). Grouping the treatments in a comparison with p.value minimum value (alpha)
* Test LSD.test and kruskal the adjust P.value (holm, hommel, hochberg, bonferroni, BH, BY, fdr). The comparison in pairs and groups give similar results.

# agricolae 1-2.3 (October 6, 2015)
## New Features 

* REGW.test(). New function for multiple comparisons of treatments. 
(Ryan, Einot and Gabriel and Welsch)
* diffograph(). New function: Mean-mean scatter plot, test: Bonferroni, Fisher, Duncan, Student-Newman-Keul 
Tukey, Kruskal-Wallis, Friedman and Waerden test.
* Changes in all comparison means, add parameters to facility function diffograph.
* Added randomization parameter (TRUE or FALSE) in all design function.
* Update Tutorial

# agricolae 1-2.2 (August 12, 2015)
## New Features 

* Now in the frequency table shows the relative frequency as a percentage, the function is table.freq or  summary( graph.freq or hist object)
* The histogram class is added to graph.freq and it can use the package HistogramTools
* The function design.bib create optimal design, use function optBlock(algDesign)
* sketch option in design: rbcd, lsd, graeco, youden, bib

# agricolae 1-2.1 (August 25, 2014)
## New Features 
* Move packages from Suggests to Imports
* AUDPS. The Area Under the Disease Progress Stairs.
* AMMI stability value (ASV) and Yield stability index (YSI)
* Design youden
* Now the PBIB.test function uses missing values.

# agricolae 1-2.0 (June 30, 2014)
## New Features 
* AMMI: aditional parameters PC=FALSE or TRUE, output principal components, check error equal cero.
* plot.AMMI: graphic aditional parameters lwd = 1.8, length = 0.1 to arrow function 
* simulation.model: aditional parameter console=FALSE or TRUE, output in console
* resampling.model: aditional parameter console=FALSE or TRUE, output in console
* stability.par: aditional parameter console=FALSE or TRUE, output in console
* stability.nonpar: aditional parameter console=FALSE or TRUE, output in console

# agricolae 1-1.9 (June 17, 2014)
## New Features 
* PBIB new parameter: group=TRUE
PBIB.test(block,trt,replication,y,k, method=c("REML","ML","VC"), 
test = c("lsd","tukey"), alpha=0.05, console=FALSE, group=TRUE)
when you have many treatments to use group=FALSE.
* design.rcbd(..., continue=FALSE) 
continue=TRUE or FALSE, continuous numbering of plot.
* Median.test. New function for multiple comparisons of treatments with Median.
* Now, AMMI function checks the minimum number of environments and genotypes.
Now use console=TRUE or FALSE to output in screen. the graphs are produced by 
the plot function.
* plot.AMMI() or plot() functions generate plot of the AMMI with others principal components.
type=1 (biplot), type=2 (triplot) and type=3 (influence genotype)
* Changed parameters by default "first = TRUE" in designs: rcbd, ab, split and lsd.
* Now vignettes in agricolae.
* change name ogive.freq by ojiva.freq, the parameters are same.
* AUDPC the evaluation parameter  now can be numeric vector. To see help(audpc)  

# agricolae 1-1.8 (February 21, 2014)
## New Features 
* zigzag(outdesign)
The new function applied to designs: rcbd, lsd, graeco, split, strip, ab, alpha, bib, cyclic,
lattice, dau. The outdesign is the output book the function design.###().
The function zigzag change the order number plots in serpentine form.
# Randomized complete design.
trt<-LETTERS[1:5]
outdesign<-design.rcbd(trt,r=4, serie=2)
book<-outdesign$book
> t(matrix(book[,1],5))
     [,1] [,2] [,3] [,4] [,5]
[1,]  101  102  103  104  105
[2,]  201  202  203  204  205
[3,]  301  302  303  304  305
[4,]  401  402  403  404  405
fieldbook <- zigzag(outdesign)
> t(matrix(fieldbook[,1],5))
     [,1] [,2] [,3] [,4] [,5]
[1,]  101  102  103  104  105
[2,]  205  204  203  202  201
[3,]  301  302  303  304  305
[4,]  405  404  403  402  401

* Now, all design functions have two output objects: parameters and book, the parameters contain initial values that will allow reproduce the design and book contain field book.
* The alpha and lattice designs have additionally two objects: statistics and 
field sketches. 
* BIB have the statistics with parameters and field book.
* Cyclic have the stetches with parameters and field book.


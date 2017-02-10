## TL;DR

This is a small update to cope with the changes in DiagrammeR 0.9.0.

## Test environments
* local Win 7 x64 install, R version 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.2.6, R 3.3.1, R-devel
* Windows on Appveyor (devel and release [3.3.2], 32 and 64 bit)
* win-builder (devel and release)
* local ArchLinux x86_64, R version 3.3.2, gcc 6.2 and gcc 7-20161204 (only when C )

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

*   checking CRAN incoming feasibility ... NOTE  
    Maintainer: 'Markus S. Wamser <r-wmisc@devel.wamser.eu>'  
    
    New submission  

    Possibly mis-spelled words in DESCRIPTION:
     API (9:85)
     Stateful (3:46)
     Wamser (3:8)
     stateful (9:106)
     tokenizer (9:16) 
  
  > This is a first time submission of the package. Spelling of these words is ok.  

* checking installed package size ... NOTE  
  installed size is  6.5Mb  
  sub-directories of 1Mb or more:  
    doc    5.0Mb  
    libs   1.4Mb  

  > The Vignette for Automat includes diagrammes produced with DiagrammeR.

## Downstream dependencies
There are no downstream dependencies.

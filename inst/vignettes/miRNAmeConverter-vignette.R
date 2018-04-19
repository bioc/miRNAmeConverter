## ----highlight = TRUE----------------------------------------------------
library(miRNAmeConverter)

## ----highlight = TRUE----------------------------------------------------
citation("miRNAmeConverter")

## ----highlight = TRUE----------------------------------------------------
ls("package:miRNAmeConverter");

## ----highlight = TRUE----------------------------------------------------
slotNames("MiRNANameConverter");

## ----highlight = TRUE----------------------------------------------------
MiRNANameConverter();

## ----highlight=TRUE------------------------------------------------------
miRNAs = c("hsa-miR-422b", "mmu-mir-872", "gra-miR157b", "cel-miR-56*");

## ----highlight = TRUE----------------------------------------------------
nc = MiRNANameConverter();

## ----highlight = TRUE----------------------------------------------------
checkMiRNAName(nc, miRNAs);

## ----highlight = TRUE----------------------------------------------------
# "RNU-6" and "bpcv1-miR-B23" are not valid
miRNAs = c("hsa-miR-422b", "RNU-6", "mmu-miR-872", "gra-miR157b", "bpcv1-miR-B23", "bpcv1-miR-B1");
nc = MiRNANameConverter();    # Create MiRNANameConverter object
checkMiRNAName(nc, miRNAs);   # check names

## ----highlight = TRUE----------------------------------------------------
nc = MiRNANameConverter();             # Create MiRNANameConverter object
assessVersion(nc, example.miRNAs);     # Assess most likely miRBase version

## ----highlight = TRUE----------------------------------------------------
miRNA.paper = "hsa-miR-422b";
nc = MiRNANameConverter();             # Create MiRNANameConverter object
translateMiRNAName(nc, miRNA.paper);   # Translate miRNA names

## ----highlight = TRUE----------------------------------------------------
miRNAs = c("hsa-miR-128b", "hsa-miR-213", "mmu-miR-302b*", 
           "mmu-miR-872", "ebv-miR-BART5", "bpcv1-miR-B23");
nc = MiRNANameConverter();              # Create MiRNANameConverter object
result = translateMiRNAName(            # Translate names
                            nc
                            ,miRNAs
                            ,sequenceFormat = 1
                            ,versions = c(8, 8.1, 18, 21)
                            );        

## ----highlight = TRUE----------------------------------------------------
result;

## ----highlight = TRUE----------------------------------------------------
attr(result, 'description');

## ----highlight = TRUE----------------------------------------------------
attr(result, 'sequence');

## ----highlight = TRUE----------------------------------------------------
result = translateMiRNAName(            # Translate names
                            nc
                            ,miRNAs
                            ,sequenceFormat = 2
                            ,versions = c(17, 19, 21)
                            );
attr(result, 'sequence');

## ----highlight = TRUE, eval=FALSE----------------------------------------
#  saveResults(nc, result);

## ----highlight = TRUE----------------------------------------------------
nc = MiRNANameConverter();   # Create MiRNANameConverter object
currentVersion(nc);          # Receive the maximum miRBase version included in the package
validVersions(nc);           # Receive all valid versions
nOrganisms(nc);              # Number of organisms
validOrganisms(nc);          # Valid organisms


# Test functions for translateMiRNAName function
#
# Author: Stefan Haunsberger
###############################################################################

require(miRNAmeConverter);

context("translateMiRNAName");

nc = MiRNANameConverter();

test_that("Checking only miRNA name return value", {

			# hsa-miR-220 is a dead entry
			# RNU-6 is not a miRNA
			# hsa-let-7a-3p and hsa-let-7a* are the same
         #  miRNA (only different version name)
			# hsa-miR-422 is now hsa-miR-378a-3p
			miRNAs = c("hsa-miR-422b", "hsa-let-7a*", "hsa-let-7a",
			           "hsa-let-7a-3p", "RNU-6", "hsa-miR-516a-3p", "hsa-miR-220");
         # miRNAs = c("hsa-miR-422b", "hsa-let-7a*",
			#            "hsa-let-7a", "hsa-let-7a-3p");
			target = c("hsa-let-7a-5p", "hsa-miR-378a-3p", "hsa-let-7a-3p");
			# target = c("hsa-let-7a-5p", "hsa-miR-378a-3p",
			#              "hsa-let-7a-3p", "RNU-6");
			# target = c("hsa-let-7a-5p", "hsa-miR-378a-3p");

			expect_equal((translateMiRNAName(nc, miRNAs, verbose = FALSE))$v21.0,
			             target);

		});

test_that("Checking attribute 'description' as well", {

			miRNAs = c("sla-miR-29b", "ebv-miR-BART3-5p", "mmu-miR-302b*",
			           "mmu-miR-872", "ebv-miR-BART5", "bpcv1-miR-B23");
			target = data.frame(mimat = c("MIMAT0002440", "MIMAT0003373",
			                              "MIMAT0003410", "MIMAT0003413",
			                              "MIMAT0004934"),
										input = c("sla-miR-29b", "mmu-miR-302b*",
										          "ebv-miR-BART3-5p", "ebv-miR-BART5",
										          "mmu-miR-872"),
										v21.0 = c("sla-miR-29b", "mmu-miR-302b-5p",
										          "ebv-miR-BART3-5p", "ebv-miR-BART5-5p",
										          "mmu-miR-872-5p"),
										stringsAsFactors = FALSE);
			rownames(target) = c("MIMAT0002440", "MIMAT0003373",
			                     "MIMAT0003410", "MIMAT0003413", "MIMAT0004934");
			attr(target, 'description') =
			   data.frame(input.miRNA = c("bpcv1-miR-B23", "ebv-miR-BART3-5p",
			                              "ebv-miR-BART5", "mmu-miR-302b*",
			                              "mmu-miR-872", "sla-miR-29b"),
			              information = c("This name is not listed in any miRBase version.",
			                              "OK", "OK", "OK", "OK", "OK"),
                     stringsAsFactors = FALSE);
			rownames(attr(target,'description')) = c("bpcv1-miR-B23",
			                                         "ebv-miR-BART3-5p",
			                                         "ebv-miR-BART5",
			                                         "mmu-miR-302b*",
			                                         "mmu-miR-872", "sla-miR-29b");
			expect_equal(translateMiRNAName(nc, miRNAs, verbose = FALSE), target);

		});

test_that("Checking attribute 'description' as well with parameter 'current'", {

			miRNAs = c("sla-miR-29b", "ebv-miR-BART3-5p", "mmu-miR-302b*",
			           "mmu-miR-872", "ebv-miR-BART5", "bpcv1-miR-B23");
			target = data.frame(mimat = c("MIMAT0002440", "MIMAT0003373",
			                              "MIMAT0003410", "MIMAT0003413",
			                              "MIMAT0004934"),
					input = c("sla-miR-29b", "mmu-miR-302b*", "ebv-miR-BART3-5p",
					          "ebv-miR-BART5", "mmu-miR-872"),
					current = c("sla-miR-29b", "mmu-miR-302b-5p", "ebv-miR-BART3-5p",
					            "ebv-miR-BART5-5p", "mmu-miR-872-5p"),
					v21.0 = c("sla-miR-29b", "mmu-miR-302b-5p", "ebv-miR-BART3-5p",
					          "ebv-miR-BART5-5p", "mmu-miR-872-5p"),
					stringsAsFactors = FALSE);
			rownames(target) = c("MIMAT0002440", "MIMAT0003373", "MIMAT0003410",
			                     "MIMAT0003413", "MIMAT0004934");
			attr(target, 'description') =
			   data.frame(input.miRNA = c("bpcv1-miR-B23", "ebv-miR-BART3-5p",
			                              "ebv-miR-BART5", "mmu-miR-302b*",
			                              "mmu-miR-872", "sla-miR-29b"),
					information = c("This name is not listed in any miRBase version.",
					                "OK", "OK", "OK", "OK", "OK"),
					stringsAsFactors = FALSE);
			rownames(attr(target,'description')) = c("bpcv1-miR-B23",
			                                         "ebv-miR-BART3-5p",
			                                         "ebv-miR-BART5",
			                                         "mmu-miR-302b*",
			                                         "mmu-miR-872", "sla-miR-29b");
			expect_equal(translateMiRNAName(nc, miRNAs, verbose = FALSE,
			                                current = TRUE), target);

		});

rm(nc);

context('html_translate_significance_thresholds')

test_that('FDR thresholds',{
  
  res = html_translate_significance_thresholds(fc = 2.5,
                                         fc_dir = 'positive', 
                                         sig_type = 'fdr', 
                                         fdr_thresh = 0.1, 
                                         pval_thresh = 0.05)
  
  #expect_equal(res$sig, list(sig = "FDR≤0.1", insig = "FDR>0.1", sig_type = 'FDR', sig_value = 0.1))
  #expect_equal(res$fc, list(sig = "log<sub>2</sub>FC&ge; 2.5", insig = "log<sub>2</sub>FC&lt; 2.5"))
  
})

test_that('p-value thresholds',{
  
  res = html_translate_significance_thresholds(fc = 2.5,
                                               fc_dir = 'positive', 
                                               sig_type = 'pvalue', 
                                               fdr_thresh = 0.1, 
                                               pval_thresh = 0.05)
  
  #expect_equal(res$sig, list(sig = "<i>P</i>-value≤0.05", insig = "<i>P</i>-value>0.05", sig_type = "<i>P</i>-value", sig_value = 0.05))
  #expect_equal(res$fc, list(sig = "log<sub>2</sub>FC&ge; 2.5", insig = "log<sub>2</sub>FC&lt; 2.5"))
  
})


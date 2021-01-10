context('get_replicate_summary_text')

test_that('basic functionality',{
  
  data = example_data %>% calc_mod_ttest() %>% id_significant_proteins()
  fake_threshold = list(fc=list(sig = 'logFC > 0', insig = 'logFC <= 0'), sig = list(sig = 'FDR < 0.1', insig = 'FDR >= 0.1'))
  reps = list(rep1.rep2 = 0.6, rep1.rep3 = 0.7, rep1.3 = 0.95)
  result = get_replicate_summary_text(fake_threshold, data, reps)
  expect_equal(mean(result$outtable$`Correlation (r)`[1:3]), 0.75)
  
})
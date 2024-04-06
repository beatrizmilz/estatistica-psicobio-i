# 8 -------

tibble::tribble(
  ~sexo, ~ansiosos, ~nao_ansiosos,
  "homens", 15, 8,
  "mulheres", 6, 9
) |> 
  tibble::column_to_rownames("sexo") |> 
  chisq.test()

# Pearson's Chi-squared test with Yates' continuity correction
# X-squared = 1.4266, df = 1, p-value = 0.2323
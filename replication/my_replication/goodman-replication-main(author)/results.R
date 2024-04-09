# 1. 先运行这个
# import methods needed for postprocessing and analysis
# 导入后处理和分析所需的方法
source("analysis_tools.R")

# run the simulation to get results (as many cases as specified in simulation.R, default is 100,000)
# 运行模拟以获得结果(如模拟中指定的许多情况)。R，默认为100,000)
source("simulation.R")

# postprocessing: add additional columns needed for evaluation
# postprocessing:添加计算所需的额外列
results$power = nominal_power(0.05, results$sigma, results$n, results$mpsd)
results$relative_mpsd = results$mpsd / results$sigma


# Replication of results from the paper
# 复制论文的结果

## Table 1
impact_of_power = calculate_impact_of_power(results, METHODS)
print(impact_of_power)
#   true_location_within_thick_null power number_simulated_cases conventional small_alpha      mesp
# 1                            TRUE     3                  23869    0.3763878   0.5325317 0.9070761
# 2                            TRUE     2                  12920    0.7630031   0.9291022 0.8264706
# 3                            TRUE     1                   8404    0.9069491   0.9844122 0.9069491
# 4                           FALSE     3                  17674    0.9939459   0.9671834 0.9244087
# 5                           FALSE     2                  14507    0.8546219   0.6616806 0.8304267
# 6                           FALSE     1                  22626    0.5626271   0.3533545 0.5625829
# distance_only interval_based thick_t_test
# 1     0.9069923      0.9969416    0.9519041
# 2     0.7876935      0.9948142    0.9506192
# 3     0.5567587      0.9852451    0.9468110
# 4     0.9244087      0.6261740    0.8578703
# 5     0.8677880      0.4292411    0.6531330
# 6     0.8800053      0.3872978    0.5055688


## Figure 1
plot_impact_of_power(results)

## Table 2
impact_of_MPSD = calculate_impact_of_MPSD(results)
print(impact_of_MPSD)
# true_location_within_thick_null relative_mpsd_decile number_simulated_cases conventional
# 1                             TRUE                    1                   1355    0.9357934
# 2                             TRUE                    2                   2459    0.9076861
# 3                             TRUE                    3                   3305    0.8538578
# 4                             TRUE                    4                   4297    0.8012567
# 5                             TRUE                    5                   5147    0.7511172
# 6                             TRUE                    6                   5633    0.6795668
# 7                             TRUE                    7                   5605    0.5834077
# 8                             TRUE                    8                   5617    0.4860246
# 9                             TRUE                    9                   5730    0.3464223
# 10                            TRUE                   10                   6045    0.1687345
# 11                           FALSE                    1                   8645    0.5457490
# 12                           FALSE                    2                   7541    0.6328073
# 13                           FALSE                    3                   6695    0.7111277
# 14                           FALSE                    4                   5703    0.7590742
# 15                           FALSE                    5                   4853    0.8133114
# 16                           FALSE                    6                   4367    0.8644378
# 17                           FALSE                    7                   4395    0.9164960
# 18                           FALSE                    8                   4383    0.9603012
# 19                           FALSE                    9                   4270    0.9843091
# 20                           FALSE                   10                   3955    0.9994943
# small_alpha      mesp distance_only interval_based thick_t_test
# 1    0.9889299 0.9357934     0.3845018      0.9749077    0.9476015
# 2    0.9833266 0.9076861     0.5465636      0.9886133    0.9459130
# 3    0.9658094 0.8583964     0.6635401      0.9888048    0.9452345
# 4    0.9483360 0.8359320     0.7412148      0.9934838    0.9527577
# 5    0.9094618 0.8515640     0.7901690      0.9935885    0.9452108
# 6    0.8583348 0.8494585     0.8075626      0.9946742    0.9529558
# 7    0.7805531 0.8636931     0.8403211      0.9951829    0.9511151
# 8    0.6580025 0.8928254     0.8814314      0.9969735    0.9528218
# 9    0.4921466 0.9160558     0.9139616      0.9959860    0.9523560
# 10   0.2574028 0.9510339     0.9508685      0.9988420    0.9526882
# 11   0.3469057 0.5457490     0.9177559      0.4393291    0.5193754
# 12   0.4360164 0.6328073     0.8969633      0.4346904    0.5700835
# 13   0.5042569 0.7103809     0.8757282      0.4176251    0.5877521
# 14   0.5714536 0.7459232     0.8537612      0.3917237    0.6002104
# 15   0.6400165 0.7716876     0.8510200      0.3824438    0.6126108
# 16   0.7185711 0.8016945     0.8465766      0.4002748    0.6409434
# 17   0.8075085 0.8379977     0.8691695      0.4607509    0.7073948
# 18   0.9060005 0.8879763     0.9000684      0.5523614    0.8008214
# 19   0.9552693 0.9252927     0.9306792      0.6459016    0.8770492
# 20   0.9919090 0.9714286     0.9714286      0.7946903    0.9590392

## Figure 2
plot_impact_of_MPSD(results)

## Table 3
error_rates = calculate_error_rates(results)
print(error_rates)
#                      conventional small_alpha      mesp distance_only interval_based thick_t_test
# accuracy                0.6915900   0.6767600 0.8106500     0.8534300    0.709860000   0.79036000
# false_positive_rate     0.4144226   0.2700639 0.1159914     0.1922422    0.005841613   0.04941031
# false_negative_rate     0.2209937   0.3670881 0.2498403     0.1089094    0.524568030   0.34176291
# false_discovery_rate    0.3049134   0.2602734 0.1130814     0.1510274    0.010030014   0.05828917
# false_omission_rate     0.3139776   0.3788389 0.2552569     0.1405330    0.390206165   0.30362614




## Table 4
impact_power_on_for_fdr = calculate_impact_of_power_on_false_discovery_and_omission_rate(results)
print(impact_power_on_for_fdr)
#   rate power cases conventional small_alpha       mesp distance_only interval_based thick_t_test
# 1  FDR     3 41543   0.38552693  0.32584094 0.09134070    0.09141554    0.004860462   0.05308792
# 2  FDR     2 27427   0.21710592  0.09677849 0.17284562    0.19656290    0.011937013   0.07029158
# 3  FDR     1 31030   0.14191559  0.04225000 0.14192515    0.33496501    0.036698869   0.09519144
# 4  FOR     3 41543   0.01583009  0.05804664 0.07692457    0.07693113    0.272712865   0.12991343
# 5  FOR     2 27427   0.16004085  0.26693520 0.17024682    0.14372349    0.364568676   0.26733774
# 6  FOR     1 31030   0.32534834  0.39645776 0.32537052    0.17730934    0.383430788   0.34305905


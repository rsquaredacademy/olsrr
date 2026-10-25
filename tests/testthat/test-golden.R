# Golden-value tests for olsrr (ticket P0-5, release 0.7.1)
#
# Every expected value below was computed independently with base R / stats --
# lm(), summary(), anova(), AIC(), BIC(), logLik(), hatvalues(),
# cooks.distance(), rstandard(), rstudent(), dfbetas(), dffits(), the closed
# form formulas quoted next to each block, or an independent base-R
# re-implementation of the selection algorithm.  olsrr's own output was never
# used to derive an expected value.  Values are hard-coded to at least six
# significant digits and the expression that produced each one is in the
# comment beside it.
#
# expect_equal() uses a relative tolerance (waldo), 1e-6 throughout, except
# where a comment says otherwise.

tol <- 1e-6

# Models -----------------------------------------------------------------------
#   surgical: lm(y ~ pindex + enzyme_test + liver_test + age + gender + alc_mod,
#                data = surgical)
#   cement:   lm(y ~ x1 + x2 + x3 + x4, data = cement)
#   fitness:  lm(oxygen ~ age + weight + runtime + restpulse + runpulse +
#                maxpulse, data = fitness)
#   auto:     lm(price ~ mpg + weight + length + turn + displacement +
#                gear_ratio, data = auto)
#   stepdata: lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = stepdata)
#   mtcars:   lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
golden_models <- list(
  surgical = lm(y ~ pindex + enzyme_test + liver_test + age + gender + alc_mod,
    data = surgical),
  cement = lm(y ~ x1 + x2 + x3 + x4, data = cement),
  fitness = lm(oxygen ~ age + weight + runtime + restpulse + runpulse + maxpulse,
    data = fitness),
  auto = lm(price ~ mpg + weight + length + turn + displacement + gear_ratio,
    data = auto),
  stepdata = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = stepdata),
  mtcars = lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
)

# Full ("universe") models used by ols_sbic() and ols_mallows_cp().
golden_full <- list(
  surgical = lm(y ~ ., data = surgical),
  cement = lm(y ~ x1 + x2 + x3 + x4, data = cement),
  fitness = lm(oxygen ~ ., data = fitness),
  auto = lm(price ~ mpg + weight + length + turn + displacement + gear_ratio,
    data = auto),
  stepdata = lm(y ~ ., data = stepdata),
  mtcars = lm(mpg ~ ., data = mtcars)
)

# Expected values ---------------------------------------------------------------
# Within each entry `m` is the model listed above, `full` the full model,
# n = nrow(model.frame(m)), p = m$rank, rss = deviance(m),
# mse = rss / df.residual(m), tss = sum(anova(m)[, 2]).
expected <- list(

  # coef(m); summary(m)$coefficients[, 2]; [, 3]; [, 4]
  # summary(m)$r.squared; summary(m)$adj.r.squared
  # deviance(m) / df.residual(m)          (MSE = RSS / (n - p - 1))
  # sqrt(deviance(m) / df.residual(m))    (RMSE)
  # summary(m)$fstatistic; pf(f, df1, df2, lower.tail = FALSE)
  # AIC(m); BIC(m)
  # SBIC (Sawa 1978, Econometrica 46:1273-1282):
  #   n * log(rss / n) + 2 * (p + 2) * q - 2 * q^2,
  #   q = n * (deviance(full) / df.residual(full)) / rss
  #   1 - ols_press(m) / tss is expr below: 1 - sum((resid / (1 - hatvalues(m)))^2) / tss
  surgical = list(
    betas = c(-686.140748477, 7.30243254945, 7.67295072003, 147.018973679, 0.334216651404, -25.3679600296, -138.185450103),
    se = c(246.378924783, 2.22197837914, 1.80182790273, 40.4244788115, 3.1644449949, 70.5913402615, 68.8673786971),
    t = c(-2.78490032815, 3.28645526798, 4.25842596199, 3.63687988074, 0.105616198715, -0.359363626411, -2.00654435696),
    p = c(0.00769343827818, 0.0019235594859, 9.78184769536e-05, 0.000683386214302, 0.916336354009, 0.72093265932, 0.0505708077154),
    rsq = 0.659682269617,
    adjr = 0.616237452973,
    mse = 60602.0475224,
    rmse = 246.174831212,
    f = 15.1843722811,
    fp = 1.41547777071e-09,
    aic = 756.400727349,
    sbc = 772.312599722,
    sbic_self = 605.19610818,
    sbic_full = 599.818557898,
    prsq = 0.520015313984,
    press = 4017241.68708,
    aic_stata = 754.400727349,
    aic_sas = 601.155365763,
    aicc_r = 759.600727349,
    aicc_stata = 756.835509958,
    aicc_sas = 660.355365763,
    sbc_stata = 768.323615675,
    sbc_sas = 615.078254089,
    apc = 0.44168896922,
    fpe = 68457.8684976,
    hsp = 1317.43581571,
    msep = 69775.3043133,
    cp = 30.1971244103,
    vif = c(1.23359296418, 1.28258941594, 1.63732232985, 1.08343272448, 1.10397643761, 1.05071287645),
    tol = c(0.810640161736, 0.779672736711, 0.610753290154, 0.92299224253, 0.905816434056, 0.951734791123),
    n = 54L, rank = 7L, dfres = 47L,
    lev3 = c(0.0566499031134, 0.0938292598344, 0.0634705864613),
    cd3 = c(0.000339665275402, 0.00332382121119, 8.60964035718e-06),
    rstud3 = c(0.196935699661, -0.470082793847, 0.0295019060739),
    rstand3 = c(0.198980931076, -0.474027699061, 0.0298205727533),
    dffits3 = c(0.0482600299223, -0.151265010907, 0.00768025466329),
    maxlev = 0.353440749096, maxlev_obs = 38L,
    maxcd = 0.483937035926, maxcd_obs = 5L,
    maxrstud = 5.8969269408, maxrstud_obs = 5L,
    maxdffits = 2.41284618328, maxdffits_obs = 5L,
    dfb_row1 = c(0.00387508179618, -0.00186026257151, 0.0103455347394, -0.000716922754699, -0.00805308871884, -0.0265846398471, 0.0285846273699),
    sumlev = 7,
    tss = 8369520.53704
  ),

  cement = list(
    betas = c(62.4053692999, 1.55110264751, 0.510167579685, 0.10190940358, -0.144061029071),
    se = c(70.0709592085, 0.744769867131, 0.723788001835, 0.754709045051, 0.709052063446),
    t = c(0.890602469337, 2.08266031692, 0.704857746179, 0.135031379639, -0.203174120065),
    p = c(0.399133563386, 0.0708216874297, 0.500901103474, 0.89592269051, 0.844071473292),
    rsq = 0.982375620408,
    adjr = 0.973563430612,
    mse = 5.98295491881,
    rmse = 2.44600795559,
    f = 111.479171821,
    fp = 4.7561817456e-07,
    aic = 65.8366897917,
    sbc = 69.2263859364,
    sbic_self = 34.4130379283,
    sbic_full = 34.4130379283,
    prsq = 0.959368120937,
    press = 110.346556904,
    aic_stata = 63.8366897917,
    aic_sas = 26.9442879283,
    aicc_r = 79.8366897917,
    aicc_stata = 72.4081183631,
    aicc_sas = 55.9442879283,
    sbc_stata = 66.661436579,
    sbc_sas = 29.7690347156,
    apc = 0.0396548540827,
    fpe = 8.28409142605,
    hsp = 0.854707845545,
    msep = 10.1250006318,
    cp = 5,
    vif = c(38.4962114906, 254.423165851, 46.8683863336, 282.512864789),
    tol = c(0.0259765821435, 0.00393045969951, 0.0213363437111, 0.00353966181593),
    n = 13L, rank = 5L, dfres = 8L,
    lev3 = c(0.550284813714, 0.333242829857, 0.576942476416),
    cd3 = c(2.06118491038e-06, 0.0572247602224, 0.30086270927),
    rstud3 = c(0.00271470565322, 0.734526653668, -1.05809320266),
    rstand3 = c(0.00290214088953, 0.756624558355, -1.05027405557),
    dffits3 = c(0.00300294746487, 0.519283016757, -1.2356357646),
    maxlev = 0.700402771034, maxlev_obs = 10L,
    maxcd = 0.393533146483, maxcd_obs = 8L,
    maxrstud = 2.0170498209, maxrstud_obs = 6L,
    maxdffits = 1.63517862878, maxdffits_obs = 8L,
    dfb_row1 = c(-0.00106437361515, 0.000772892085369, 0.00111736620522, 0.000697705223089, 0.00121792995633),
    sumlev = 5,
    tss = 2715.76307692
  ),

  fitness = list(
    betas = c(102.934479484, -0.226973796256, -0.0741774136912, -2.62865281808, -0.0215336397532, -0.36962775849, 0.303217129456),
    se = c(12.403258101, 0.0998374731522, 0.0545931595438, 0.384562197692, 0.0660542760875, 0.119852940265, 0.136495188091),
    t = c(8.29898714078, -2.27343290139, -1.35873091631, -6.83544257304, -0.325999178685, -3.08401076915, 2.22144922247),
    p = c(1.63844142172e-08, 0.0322350699328, 0.186865883032, 4.53828052757e-07, 0.747250498513, 0.00507853244694, 0.0360070420774),
    rsq = 0.84867191612,
    adjr = 0.81083989515,
    mse = 5.36824740962,
    rmse = 2.31694786511,
    f = 22.4326349574,
    fp = 9.71530501542e-09,
    aic = 148.135800599,
    sbc = 159.607698235,
    sbic_self = 64.0748059844,
    sbic_full = 64.0748059844,
    prsq = 0.77355837057,
    press = 192.78822428,
    aic_stata = 146.135800599,
    aic_sas = 58.1616115399,
    aicc_r = 154.681255144,
    aicc_stata = 151.005365816,
    aicc_sas = 97.7070660854,
    sbc_stata = 156.17371103,
    sbc_sas = 68.1995219713,
    apc = 0.239602799476,
    fpe = 6.58043230857,
    hsp = 0.233402061288,
    msep = 6.98700364114,
    cp = 7,
    vif = c(1.51283618316, 1.15532939952, 1.59086787933, 1.41558865291, 8.43727418363, 8.74384843181),
    tol = c(0.66101010217, 0.865554014651, 0.628587711771, 0.706419903794, 0.118521690565, 0.114366117825),
    n = 31L, rank = 7L, dfres = 24L,
    lev3 = c(0.142093048739, 0.243906774226, 0.256868496725),
    cd3 = c(8.56441838872e-05, 0.0390658846437, 0.0177356907483),
    rstud3 = c(0.0589010021496, -0.917678449389, -0.591130071507),
    rstand3 = c(0.0601632979423, -0.920711555653, -0.599308569902),
    dffits3 = c(0.0239711575825, -0.521212461617, -0.34754067379),
    maxlev = 0.491573646058, maxlev_obs = 21L,
    maxcd = 0.197525144943, maxcd_obs = 21L,
    maxrstud = 2.83064848451, maxrstud_obs = 8L,
    maxdffits = 1.18702293093, maxdffits_obs = 21L,
    dfb_row1 = c(-0.00845312109266, -0.000919787254412, 0.0143976247544, -0.00113244393536, 0.0100815708532, -0.000133539546307, 0.00104704928884),
    sumlev = 7,
    tss = 851.381544839
  ),

  auto = list(
    betas = c(11833.1535673, -105.932500613, 4.95511034513, -80.9532213758, -303.092591484, 10.5782627954, 2245.64265988),
    se = c(7054.2777185, 79.6262092177, 1.37309941768, 39.8506125873, 125.364393907, 7.62431705835, 1057.87566781),
    t = c(1.67744367879, -1.3303722688, 3.60870471674, -2.03141723853, -2.4176927917, 1.38743742088, 2.1227850571),
    p = c(0.0981154117782, 0.18790624444, 0.000588101245376, 0.0461821915408, 0.0183459314087, 0.169906131065, 0.0374704209199),
    rsq = 0.450795794315,
    adjr = 0.401613328134,
    mse = 5205680.39456,
    rmse = 2281.59601914,
    f = 9.16578263192,
    fp = 2.55082794427e-07,
    aic = 1363.0786517,
    sbc = 1381.51117244,
    sbic_self = 1154.51660421,
    sbic_full = 1154.51660421,
    prsq = 0.266123607908,
    press = 466059501.648,
    aic_stata = 1361.0786517,
    aic_sas = 1151.07574878,
    aicc_r = 1365.29403631,
    aicc_stata = 1362.7756214,
    aicc_sas = 1229.2911334,
    sbc_stata = 1377.20710735,
    sbc_sas = 1167.20420444,
    apc = 0.663963293441,
    fpe = 5698109.62107,
    hsp = 78873.9453721,
    msep = 5755666.28391,
    cp = 7,
    vif = c(2.97604653873, 15.970103095, 11.0411175, 4.26551560444, 6.87518997967, 3.26731487907),
    tol = c(0.33601625075, 0.0626170034128, 0.0905705423383, 0.234438246799, 0.145450526161, 0.306061716428),
    n = 74L, rank = 7L, dfres = 67L,
    lev3 = c(0.0709649056808, 0.197718980113, 0.0801430689853),
    cd3 = c(0.00957944602028, 0.146232898359, 0.0199913728567),
    rstud3 = c(-0.936077896999, -2.08853758583, -1.2732131366),
    rstand3 = c(-0.936943628529, -2.0380303263, -1.26735267997),
    dffits3 = c(-0.258712698386, -1.0368196446, -0.375814863917),
    maxlev = 0.363892497369, maxlev_obs = 7L,
    maxcd = 0.324789153977, maxcd_obs = 13L,
    maxrstud = 3.37610166777, maxrstud_obs = 13L,
    maxdffits = 1.62060592412, maxdffits_obs = 13L,
    dfb_row1 = c(0.103788736242, -0.0111038594667, -0.100351957062, 0.0341611290297, -0.0642421161157, 0.0973861795754, -0.12862291018),
    sumlev = 7,
    tss = 635065396.122
  ),

  stepdata = list(
    betas = c(-0.00532468018175, 0.255812300351, 0.249218086275, 0.253003137635, -0.00661047714587, -0.000887725498591, -0.000459739442568),
    se = c(0.00355943246139, 0.0046859462965, 0.00472307744722, 0.00468552255619, 0.00353516208762, 0.00354491862361, 0.00356371965323),
    t = c(-1.49593516368, 54.5913854244, 52.7660384696, 53.9967814905, -1.86992193909, -0.250421968132, -0.12900550192),
    p = c(0.134686336166, 0, 0, 0, 0.0615092615103, 0.802263615785, 0.897354592971),
    rsq = 0.749622970634,
    adjr = 0.749547831226,
    mse = 0.25332290701,
    rmse = 0.503311938076,
    f = 9976.42907432,
    fp = 0,
    aic = 29304.7342678,
    sbc = 29367.9621682,
    sbic_self = -27452.8021589,
    sbic_full = -27452.8021589,
    prsq = 0.74944843819,
    press = 5068.21536279,
    aic_stata = 29302.7342678,
    aic_sas = -27454.8070604,
    aicc_r = 29304.741471,
    aicc_stata = 29302.73987,
    aicc_sas = -7452.79985715,
    sbc_stata = 29358.0586807,
    sbc_sas = -27399.4826475,
    apc = 0.250552354651,
    fpe = 0.253411570028,
    hsp = 1.2671213836e-05,
    msep = 0.25341160424,
    cp = 7,
    vif = c(3.53240189652, 3.45545873862, 3.47849454561, 1.00032064837, 1.00010646189, 12.9527061665),
    tol = c(0.283093495387, 0.289397175785, 0.287480686512, 0.999679454415, 0.999893549438, 0.0772039438821),
    n = 20000L, rank = 7L, dfres = 19993L,
    lev3 = c(0.000342252715999, 0.000445859344135, 0.000573636050367),
    cd3 = c(1.26860489917e-05, 0.00014893409345, 7.97537913812e-06),
    rstud3 = c(0.509279722065, 1.52884984316, -0.311868898443),
    rstand3 = c(0.509289155377, 1.52879871142, -0.311875939542),
    dffits3 = c(0.00942332495928, 0.0322894470713, -0.0074716218586),
    maxlev = 0.00164417837809, maxlev_obs = 1575L,
    maxcd = 0.00146462713371, maxcd_obs = 10254L,
    maxrstud = 4.33205732162, maxrstud_obs = 10254L,
    maxdffits = 0.101299065603, maxdffits_obs = 10254L,
    dfb_row1 = c(0.00356925863596, 0.00441128336638, -0.00146489907856, 0.00337118028265, 0.000713758741611, 0.00550191524327, -0.00302394296798),
    sumlev = 7,
    tss = 20228.232968
  ),

  mtcars = list(
    betas = c(27.3296379667, 0.00266643109156, -0.0186662024875, -4.60912261696, 0.544160312015),
    se = c(8.63903218718, 0.0107376703373, 0.0156130473393, 1.26585130785, 0.466493157636),
    t = c(3.16350690385, 0.248324916653, -1.19555152059, -3.64112482121, 1.16649151892),
    p = c(0.00383394223754, 0.805762061086, 0.242266764449, 0.00113431967649, 0.253616069628),
    rsq = 0.835144275477,
    adjr = 0.810721205177,
    mse = 6.87538240529,
    rmse = 2.62209504124,
    f = 34.1948929937,
    fp = 3.31059761232e-10,
    aic = 159.069609415,
    sbc = 167.864024831,
    sbic_self = 70.0408080358,
    sbic_full = 70.2759875978,
    prsq = 0.7710296793,
    press = 257.831385645,
    aic_stata = 157.069609415,
    aic_sas = 66.2575432895,
    aicc_r = 162.429609415,
    aicc_stata = 159.377301722,
    aicc_sas = 103.61754329,
    sbc_stata = 164.398288929,
    sbc_sas = 73.5862228035,
    apc = 0.225913400273,
    fpe = 7.94966090611,
    hsp = 0.264437784819,
    msep = 8.18104396783,
    cp = 4.43043417543,
    vif = c(7.985439025, 5.16675830822, 6.91694157126, 3.13311910934),
    tol = c(0.125227930095, 0.193544954176, 0.144572567181, 0.319170757671),
    n = 32L, rank = 5L, dfres = 27L,
    lev3 = c(0.148302556272, 0.129923513025, 0.0631759993616),
    cd3 = c(0.0149210946147, 0.00254056563686, 0.0132481084342),
    rstud3 = c(-0.647488390154, -0.286665274441, -0.990756282365),
    rstand3 = c(-0.654566383739, -0.291665492979, -0.991094082569),
    dffits3 = c(-0.270186491775, -0.110774614829, -0.257284658673),
    maxlev = 0.531681337704, maxlev_obs = 31L,
    maxcd = 0.280114842019, maxcd_obs = 17L,
    maxrstud = 2.656702463, maxrstud_obs = 17L,
    maxdffits = 1.30951330292, maxdffits_obs = 17L,
    dfb_row1 = c(-0.240862504007, 0.0586043539132, 0.171072054084, -0.0974699134478, 0.22618617411),
    sumlev = 5,
    tss = 1126.0471875
  )
)

# Full influence vectors for the smallest model (mtcars), from
#   hatvalues(m); cooks.distance(m); rstudent(m); rstandard(m); dffits(m);
#   dfbetas(m)[, "disp"].
mtcars_vectors <- list(
  leverage = c(0.148302556272, 0.129923513025, 0.0631759993616, 0.1159846036, 0.171878688132, 0.0998674455582, 0.147200845369, 0.129126559487, 0.458390847284, 0.135971132758, 0.115411591767, 0.103430436171, 0.0468416662835, 0.055277223664, 0.205783923918, 0.210663282463, 0.195468595974, 0.0859304401288, 0.14685384975, 0.143890099067, 0.119578004198, 0.110516129169, 0.0800222940869, 0.106355924138, 0.195665136483, 0.0932414216772, 0.150936497072, 0.164675652567, 0.239785801527, 0.218637518999, 0.531681337704, 0.0795309823455),
  cooksd = c(0.0149210946147, 0.00254056563686, 0.0132481084342, 0.0004544524072, 0.000534665820757, 0.0306756091878, 0.0147217459195, 0.0135883586072, 0.052464082945, 0.000778562498782, 0.0190984398511, 0.00363604592245, 6.59417382935e-05, 0.00635100129264, 2.59275938163e-05, 0.013234256858, 0.280114842019, 0.0950897220604, 0.00852264439025, 0.155793860494, 0.0670841110651, 0.0326235169406, 0.0381002855096, 0.00448652726778, 0.0556265798441, 0.000462152265814, 0.00413785716957, 0.0501717853778, 0.0080578223779, 0.00181057693449, 0.187537335578, 0.00635592262458),
  rstudent = c(-0.647488390154, -0.286665274441, -0.990756282365, -0.129182317785, 0.111396253483, -1.18451520975, -0.64594383372, 0.669977021184, -0.549481403735, -0.1544103262, -0.85114256301, 0.390700025456, 0.0803878310443, -0.730296477442, 0.0219502029441, 0.490884764985, 2.656702463, 2.44800051777, 0.490508140828, 2.32117833521, -1.61789048349, -1.15275034607, -1.51497847828, -0.427531013863, 1.07223103561, -0.147165000066, 0.335496915284, 1.13400631405, -0.351547988814, -0.176612961212, 0.905784767546, -0.59923053599),
  rstandard = c(-0.654566383739, -0.291665492979, -0.991094082569, -0.131600927301, 0.113491197587, -1.1757713239, -0.653029634022, 0.676921386223, -0.556725448575, -0.157279641721, -0.855519384588, 0.396978969225, 0.0819089869698, -0.736690695328, 0.0223681323663, 0.497933689583, 2.40096631076, 2.24888835828, 0.497555161402, 2.15282870615, -1.57150088006, -1.14579414084, -1.47989965852, -0.434151799575, 1.06927128234, -0.149905973316, 0.341150257314, 1.1280481914, -0.35739634239, -0.179869457782, 0.908811673658, -0.606471947235),
  dffits = c(-0.270186491775, -0.110774614829, -0.257284658673, -0.0467921854059, 0.0507498331959, -0.394547574603, -0.268365403799, 0.257982456899, -0.505507927059, -0.0612541576559, -0.307437190166, 0.132701435773, 0.0178206669688, -0.176652636671, 0.0111731318882, 0.253596248352, 1.30951330292, 0.750577255921, 0.203505977415, 0.95161045397, -0.596251285979, -0.406330156332, -0.446810438413, -0.147491223113, 0.528842868307, -0.0471914304663, 0.141454112492, 0.503503622175, -0.197436917149, -0.093424039319, 0.965117052036, -0.176139809454),
  dfbetas_disp = c(0.0586043539132, 0.0469902252416, 0.0502218380974, -0.0331901157123, 0.0446380380285, -0.0732142976765, -0.114401670916, -0.0816499013795, 0.0125840323461, 0.0482279929155, 0.234944360143, -0.0863959592326, -0.00563488542308, 0.0504793510461, 0.00339174313703, 0.0002881172682, -0.269306165743, -0.112413841205, 0.0734093648046, 0.222272708303, -0.0193355412265, -0.228112625354, -0.290604967636, 0.00144991418148, 0.447486907978, -0.00464094109587, -0.00897861117758, 0.117766483508, -0.0755051337352, 0.067133821973, -0.341712816036, 0.121120191714)
)

# ols_regress() ----------------------------------------------------------------
test_that("golden: ols_regress() coefficients, standard errors, t and p match lm()", {
  for (nm in names(golden_models)) {
    m <- golden_models[[nm]]
    e <- expected[[nm]]
    fit <- ols_regress(m)

    expect_equal(unname(fit$betas), e$betas, tolerance = tol, info = nm)
    expect_equal(unname(fit$std_errors), e$se, tolerance = tol, info = nm)
    expect_equal(unname(fit$tvalues), e$t, tolerance = tol, info = nm)
    expect_equal(unname(fit$pvalues), e$p, tolerance = tol, info = nm)
  }
})

test_that("golden: ols_regress() fit statistics match base R", {
  for (nm in names(golden_models)) {
    m <- golden_models[[nm]]
    e <- expected[[nm]]
    fit <- ols_regress(m)

    # summary(m)$r.squared / $adj.r.squared
    expect_equal(fit$rsq, e$rsq, tolerance = tol, info = nm)
    expect_equal(fit$adjr, e$adjr, tolerance = tol, info = nm)

    # MSE = deviance(m) / df.residual(m) = RSS / (n - p - 1); RMSE = sqrt(MSE)
    expect_equal(fit$mse, e$mse, tolerance = tol, info = nm)
    expect_equal(fit$rmse, e$rmse, tolerance = tol, info = nm)
    expect_equal(fit$mse, deviance(m) / df.residual(m), tolerance = tol, info = nm)
    expect_equal(fit$rmse, sqrt(deviance(m) / df.residual(m)), tolerance = tol, info = nm)

    # summary(m)$fstatistic and pf(f, df1, df2, lower.tail = FALSE)
    expect_equal(fit$f, e$f, tolerance = tol, info = nm)
    expect_equal(fit$p, e$fp, tolerance = tol, info = nm)

    # AIC(m) = -2 logLik + 2 (p + 1); BIC(m) = -2 logLik + log(n) (p + 1)
    expect_equal(fit$aic, e$aic, tolerance = tol, info = nm)
    expect_equal(fit$sbc, e$sbc, tolerance = tol, info = nm)

    # SBIC of the fitted model against itself:
    # n log(rss/n) + 2 (p + 2) q - 2 q^2, q = n / (n - p)
    expect_equal(fit$sbic, e$sbic_self, tolerance = tol, info = nm)

    # predicted R2: 1 - press / tss
    expect_equal(fit$prsq, e$prsq, tolerance = tol, info = nm)
  }
})

test_that("golden: ols_regress() uses the documented sample-size and rank bookkeeping", {
  for (nm in names(golden_models)) {
    m <- golden_models[[nm]]
    e <- expected[[nm]]
    fit <- ols_regress(m)

    # model_df = p - 1, error_df = n - p, total_df = n - 1
    expect_equal(fit$model_df, e$rank - 1, tolerance = tol, info = nm)
    expect_equal(fit$error_df, e$dfres, tolerance = tol, info = nm)
    expect_equal(fit$total_df, e$n - 1, tolerance = tol, info = nm)

    # rss = regression SS, ess = error SS, tss = total SS
    expect_equal(fit$rss, e$tss - deviance(m), tolerance = tol, info = nm)
    expect_equal(fit$ess, deviance(m), tolerance = tol, info = nm)
    expect_equal(fit$tss, sum(anova(m)[, 2]), tolerance = tol, info = nm)
  }
})

# Information criteria ---------------------------------------------------------
test_that("golden: ols_aic() and ols_sbc() match their textbook formulas", {
  for (nm in names(golden_models)) {
    m <- golden_models[[nm]]
    e <- expected[[nm]]
    n <- nrow(model.frame(m))
    p <- m$rank
    ll <- as.numeric(logLik(m))
    rss <- deviance(m)

    # R:     -2 logLik + 2 (p + 1)   [= AIC(m)]
    # STATA: -2 logLik + 2 p
    # SAS:   n log(rss / n) + 2 p
    expect_equal(ols_aic(m), e$aic, tolerance = tol, info = nm)
    expect_equal(ols_aic(m, method = "STATA"), e$aic_stata, tolerance = tol, info = nm)
    expect_equal(ols_aic(m, method = "SAS"), e$aic_sas, tolerance = tol, info = nm)

    # corrected R:     -2 logLik + 2 k + 2 k (k + 1) / (n - k - 1), k = p + 1
    # corrected STATA: -2 logLik + 2 p + 2 p (p + 1) / (n - p - 1)
    # corrected SAS:   n log(rss / n) + n (n + p) / (n - p - 2)
    expect_equal(ols_aic(m, corrected = TRUE), e$aicc_r, tolerance = tol, info = nm)
    expect_equal(ols_aic(m, method = "STATA", corrected = TRUE), e$aicc_stata, tolerance = tol, info = nm)
    expect_equal(ols_aic(m, method = "SAS", corrected = TRUE), e$aicc_sas, tolerance = tol, info = nm)

    # R:     -2 logLik + log(n) (p + 1)   [= BIC(m)]
    # STATA: -2 logLik + log(n) p
    # SAS:   n log(rss / n) + p log(n)
    expect_equal(ols_sbc(m), e$sbc, tolerance = tol, info = nm)
    expect_equal(ols_sbc(m, method = "STATA"), e$sbc_stata, tolerance = tol, info = nm)
    expect_equal(ols_sbc(m, method = "SAS"), e$sbc_sas, tolerance = tol, info = nm)
  }
})

test_that("golden: ols_sbic() implements Sawa's (1978) criterion", {
  for (nm in names(golden_models)) {
    m <- golden_models[[nm]]
    full <- golden_full[[nm]]
    e <- expected[[nm]]
    n <- nrow(model.frame(m))
    p <- m$rank
    rss <- deviance(m)
    q <- n * (deviance(full) / df.residual(full)) / rss

    # SBIC = n log(rss / n) + 2 (p + 2) q - 2 q^2
    # (Sawa 1978, Econometrica 46:1273-1282; Judge et al. 1980, section 4.5)
    expect_equal(ols_sbic(m, full), e$sbic_full, tolerance = tol, info = nm)
    expect_equal(ols_sbic(m, full), n * log(rss / n) + 2 * (p + 2) * q - 2 * q^2,
      tolerance = tol, info = nm)
  }
})

test_that("golden: ols_apc(), ols_fpe(), ols_hsp(), ols_msep() match their formulas", {
  for (nm in names(golden_models)) {
    m <- golden_models[[nm]]
    e <- expected[[nm]]
    n <- nrow(model.frame(m))
    p <- m$rank
    rss <- deviance(m)
    mse <- rss / df.residual(m)
    r2 <- summary(m)$r.squared

    # Amemiya: ((n + p) / (n - p)) (1 - R2)
    expect_equal(ols_apc(m), e$apc, tolerance = tol, info = nm)
    expect_equal(ols_apc(m), ((n + p) / (n - p)) * (1 - r2), tolerance = tol, info = nm)

    # Final prediction error: ((n + p) / n) MSE
    expect_equal(ols_fpe(m), e$fpe, tolerance = tol, info = nm)
    expect_equal(ols_fpe(m), ((n + p) / n) * mse, tolerance = tol, info = nm)

    # Hocking's Sp: MSE / (n - p - 1)
    expect_equal(ols_hsp(m), e$hsp, tolerance = tol, info = nm)
    expect_equal(ols_hsp(m), mse / (n - p - 1), tolerance = tol, info = nm)

    # MSEP (Stein / Darlington): (n + 1)(n - 2) MSE / (n (n - p - 1))
    expect_equal(ols_msep(m), e$msep, tolerance = tol, info = nm)
    expect_equal(ols_msep(m), ((n + 1) * (n - 2) * mse) / (n * (n - p - 1)),
      tolerance = tol, info = nm)
  }
})

test_that("golden: ols_mallows_cp() matches SSE / MSE_full - (n - 2p)", {
  for (nm in names(golden_models)) {
    m <- golden_models[[nm]]
    full <- golden_full[[nm]]
    e <- expected[[nm]]
    n <- nrow(model.frame(m))
    p <- m$rank

    expect_equal(ols_mallows_cp(m, full), e$cp, tolerance = tol, info = nm)
    expect_equal(ols_mallows_cp(m, full),
      deviance(m) / (deviance(full) / df.residual(full)) - (n - 2 * p),
      tolerance = tol, info = nm)
  }
})

# Influence and diagnostics ----------------------------------------------------
test_that("golden: ols_leverage(), Cook's D, DFBETAs, DFFITs and residual types", {
  for (nm in names(golden_models)) {
    m <- golden_models[[nm]]
    e <- expected[[nm]]
    n <- nrow(model.frame(m))

    # hatvalues(m); leverage sums to the model rank
    expect_equal(ols_leverage(m), unname(hatvalues(m)), tolerance = tol, info = nm)
    expect_equal(sum(ols_leverage(m)), e$sumlev, tolerance = tol, info = nm)
    expect_equal(ols_leverage(m)[1:3], e$lev3, tolerance = tol, info = nm)
    expect_equal(max(ols_leverage(m)), e$maxlev, tolerance = tol, info = nm)
    expect_equal(unname(which.max(ols_leverage(m))), e$maxlev_obs, info = nm)

    # cooks.distance(m), exposed through ols_prep_cdplot_data()
    cd <- ols_prep_cdplot_data(m)
    expect_equal(cd$ckd$cd, unname(cooks.distance(m)), tolerance = tol, info = nm)
    expect_equal(cd$ckd$cd[1:3], e$cd3, tolerance = tol, info = nm)
    expect_equal(max(cd$ckd$cd), e$maxcd, tolerance = tol, info = nm)
    expect_equal(unname(which.max(cd$ckd$cd)), e$maxcd_obs, info = nm)
    # thresholds (type 1 = 4/n, type 2 = 4/(n-k-1), type 3 = 1,
    # type 4 = 1/(n-k-1), type 5 = 3 mean(D)); k counts predictors, not the
    # intercept (Cook & Weisberg 1982; Belsley, Kuh & Welsch 1980)
    k_pred <- length(coef(m)) - 1
    expect_equal(ols_prep_cdplot_data(m, type = 1)$ts, 4 / n, tolerance = tol, info = nm)
    expect_equal(ols_prep_cdplot_data(m, type = 2)$ts, 4 / (n - k_pred - 1), tolerance = tol, info = nm)
    expect_equal(ols_prep_cdplot_data(m, type = 3)$ts, 1, tolerance = tol, info = nm)
    expect_equal(ols_prep_cdplot_data(m, type = 4)$ts, 1 / (n - k_pred - 1), tolerance = tol, info = nm)
    expect_equal(ols_prep_cdplot_data(m, type = 5)$ts, 3 * mean(cooks.distance(m)),
      tolerance = tol, info = nm)

    # deleted (studentized) residuals: rstudent(m)
    ds <- ols_prep_dsrvf_data(m)
    expect_equal(ds$ds$dsr, unname(rstudent(m)), tolerance = tol, info = nm)
    expect_equal(ds$ds$dsr[1:3], e$rstud3, tolerance = tol, info = nm)
    expect_equal(max(abs(ds$ds$dsr)), e$maxrstud, tolerance = tol, info = nm)
    expect_equal(unname(which.max(abs(ds$ds$dsr))), e$maxrstud_obs, info = nm)

    # internally studentized residuals: rstandard(m)
    sc <- ols_prep_srchart_data(m)
    expect_equal(sc$sdres, unname(rstandard(m)), tolerance = tol, info = nm)
    expect_equal(sc$sdres[1:3], e$rstand3, tolerance = tol, info = nm)

    # DFFITs: dffits(m), flagged at the size-adjusted threshold 2 sqrt(k/n),
    # where k counts all coefficients (including the intercept)
    dff <- ols_plot_dffits(m, print_plot = FALSE)
    k <- length(coef(m))
    expect_equal(dff$threshold, round(2 * sqrt(k / n), 2), info = nm)
    expect_equal(dff$outliers$dffits, unname(dffits(m))[abs(dffits(m)) > 2 * sqrt(k / n)],
      tolerance = tol, info = nm)
    expect_equal(dff$outliers$observation, unname(which(abs(dffits(m)) > 2 * sqrt(k / n))),
      info = nm)
    expect_equal(unname(dffits(m))[1:3], e$dffits3, tolerance = tol, info = nm)
    expect_equal(max(abs(dffits(m))), e$maxdffits, tolerance = tol, info = nm)
    expect_equal(unname(which.max(abs(dffits(m)))), e$maxdffits_obs, info = nm)

    # DFBETAs: dfbetas(m), flagged at 2 / sqrt(n)
    dfb <- ols_plot_dfbetas(m, print_plot = FALSE)
    expect_equal(dfb$outliers[[2]]$dbetas,
      unname(dfbetas(m)[, 2])[abs(dfbetas(m)[, 2]) >= 2 / sqrt(n)],
      tolerance = tol, info = nm)
    expect_equal(dfb$outliers[[2]]$obs,
      unname(which(abs(dfbetas(m)[, 2]) >= 2 / sqrt(n))), info = nm)
    expect_equal(unname(dfbetas(m)[1, ]), e$dfb_row1, tolerance = tol, info = nm)
  }
})

test_that("golden: full influence vectors for mtcars", {
  m <- golden_models$mtcars
  e <- mtcars_vectors

  expect_equal(ols_leverage(m), e$leverage, tolerance = tol)
  expect_equal(unname(ols_prep_cdplot_data(m)$ckd$cd), e$cooksd, tolerance = tol)
  expect_equal(unname(ols_prep_dsrvf_data(m)$ds$dsr), e$rstudent, tolerance = tol)
  expect_equal(unname(ols_prep_srchart_data(m)$sdres), e$rstandard, tolerance = tol)
  expect_equal(unname(dffits(m)), e$dffits, tolerance = tol)
  expect_equal(unname(dfbetas(m)[, "disp"]), e$dfbetas_disp, tolerance = tol)
})

test_that("golden: ols_press() and ols_pred_rsq() match the PRESS formula", {
  for (nm in names(golden_models)) {
    m <- golden_models[[nm]]
    e <- expected[[nm]]
    h <- hatvalues(m)
    tss <- sum(anova(m)[, 2])

    # PRESS = sum((resid / (1 - h))^2)
    expect_equal(ols_press(m), e$press, tolerance = tol, info = nm)
    expect_equal(ols_press(m), sum((residuals(m) / (1 - h))^2), tolerance = tol, info = nm)

    # predicted R2 = 1 - PRESS / TSS
    expect_equal(ols_pred_rsq(m), e$prsq, tolerance = tol, info = nm)
    expect_equal(ols_pred_rsq(m), 1 - sum((residuals(m) / (1 - h))^2) / tss,
      tolerance = tol, info = nm)
  }
})

# Heteroskedasticity tests -----------------------------------------------------
test_that("golden: ols_test_breusch_pagan() matches the auxiliary regression", {
  # LM = (1/2) * SSreg of the regression of g = e^2 / mean(e^2) - 1 on Z
  # (Breusch & Pagan 1979, Econometrica 47:1287-1294).
  m <- golden_models$surgical
  n <- nrow(model.frame(m))
  g <- residuals(m)^2 / mean(residuals(m)^2) - 1
  ind <- residuals(m)^2 / (sum(residuals(m)^2) / n) - 1
  X <- model.matrix(m)[, -1]

  # fitted values, df = 1
  bp_fit <- ols_test_breusch_pagan(m)
  expect_equal(bp_fit$bp, 30.0579195411, tolerance = tol)
  expect_equal(bp_fit$bp, anova(lm(g ~ fitted(m)))[1, 2] / 2, tolerance = tol)
  expect_equal(bp_fit$df, 1L)
  expect_equal(bp_fit$p, pchisq(anova(lm(g ~ fitted(m)))[1, 2] / 2, 1, lower.tail = FALSE),
    tolerance = tol)

  # rhs = TRUE, df = number of predictors
  bp_rhs <- ols_test_breusch_pagan(m, rhs = TRUE)
  expect_equal(bp_rhs$bp, 46.3477669168, tolerance = tol)
  expect_equal(bp_rhs$bp, anova(lm(ind ~ X))[1, 2] / 2, tolerance = tol)
  expect_equal(bp_rhs$df, 6L)
  expect_equal(bp_rhs$p, pchisq(anova(lm(ind ~ X))[1, 2] / 2, 6, lower.tail = FALSE),
    tolerance = tol)

  # vars argument, df = 2
  bp_vars <- ols_test_breusch_pagan(m, vars = c("pindex", "enzyme_test"))
  expect_equal(bp_vars$bp, 17.5585657092, tolerance = tol)
  expect_equal(bp_vars$bp,
    anova(lm(ind ~ X[, c("pindex", "enzyme_test")]))[1, 2] / 2, tolerance = tol)
  expect_equal(bp_vars$df, 2L)
  expect_equal(bp_vars$p,
    pchisq(anova(lm(ind ~ X[, c("pindex", "enzyme_test")]))[1, 2] / 2, 2,
      lower.tail = FALSE), tolerance = tol)

  # mtcars, fitted values and rhs
  mt <- golden_models$mtcars
  mt_g <- residuals(mt)^2 / mean(residuals(mt)^2) - 1
  mt_ind <- residuals(mt)^2 / (sum(residuals(mt)^2) / nrow(model.frame(mt))) - 1
  mt_X <- model.matrix(mt)[, -1]
  expect_equal(ols_test_breusch_pagan(mt)$bp, 0.588467312195, tolerance = tol)
  expect_equal(ols_test_breusch_pagan(mt)$bp,
    anova(lm(mt_g ~ fitted(mt)))[1, 2] / 2, tolerance = tol)
  expect_equal(ols_test_breusch_pagan(mt)$p, 0.443012412328, tolerance = tol)
  expect_equal(ols_test_breusch_pagan(mt, rhs = TRUE)$bp, 2.32403593482, tolerance = tol)
  expect_equal(ols_test_breusch_pagan(mt, rhs = TRUE)$bp,
    anova(lm(mt_ind ~ mt_X))[1, 2] / 2, tolerance = tol)
  expect_equal(ols_test_breusch_pagan(mt, rhs = TRUE)$p, 0.676396431512, tolerance = tol)
})

test_that("golden: ols_test_breusch_pagan(multiple = TRUE) matches per-variable tests", {
  # Each predictor gets its own LM statistic; the last element is the joint
  # statistic over all predictors (Breusch & Pagan 1979).
  m <- golden_models$surgical
  n <- nrow(model.frame(m))
  ind <- residuals(m)^2 / (sum(residuals(m)^2) / n) - 1
  X <- model.matrix(m)[, -1]
  per_var <- sapply(colnames(X), function(v) anova(lm(ind ~ X[, v]))[1, 2] / 2)
  joint <- anova(lm(ind ~ X))[1, 2] / 2

  bp <- ols_test_breusch_pagan(m, rhs = TRUE, multiple = TRUE)
  expect_equal(bp$bp, unname(c(per_var, joint)), tolerance = tol)
  expect_equal(bp$bp,
    c(0.31156658392, 17.1281533654, 25.7187286333, 0.54596969155, 1.1967303304,
      9.68572689325, 46.3477669168), tolerance = tol)
  expect_equal(bp$p, unname(c(pchisq(per_var, 1, lower.tail = FALSE),
    pchisq(joint, 6, lower.tail = FALSE))), tolerance = tol)
  expect_equal(bp$p,
    c(0.576720454188, 3.4940518036e-05, 3.9497533584e-07, 0.459969168188,
      0.273976160565, 0.00185704907037, 2.52415303969e-08), tolerance = tol)

  # Bonferroni adjustment: pmin(1, p_i * 6) for the per-variable tests
  bp_bonf <- ols_test_breusch_pagan(m, rhs = TRUE, multiple = TRUE,
    p.adj = "bonferroni")
  expect_equal(bp_bonf$p, unname(c(pmin(1, pchisq(per_var, 1, lower.tail = FALSE) * 6),
    pchisq(joint, 6, lower.tail = FALSE))), tolerance = tol)
})

test_that("golden: ols_test_f() matches the studentized Breusch-Pagan F test", {
  # F statistic of the auxiliary regression of e^2 on the fitted values (df 1)
  # or on the predictors; scaling e^2 by a constant does not change F.
  mt <- golden_models$mtcars
  n <- nrow(model.frame(mt))
  X <- model.matrix(mt)[, -1]

  ft <- ols_test_f(mt)
  expect_equal(ft$f, 0.492061732281, tolerance = tol)
  expect_equal(ft$f, unname(summary(lm(residuals(mt)^2 ~ fitted(mt)))$fstatistic[1]),
    tolerance = tol)
  expect_equal(ft$numdf, 1L)
  expect_equal(ft$dendf, n - 2L)
  expect_equal(ft$p, 0.488415374668, tolerance = tol)

  ftr <- ols_test_f(mt, rhs = TRUE)
  expect_equal(ftr$f, 0.459469406101, tolerance = tol)
  expect_equal(ftr$f, unname(summary(lm(residuals(mt)^2 ~ X))$fstatistic[1]), tolerance = tol)
  expect_equal(ftr$numdf, 4L)
  expect_equal(ftr$dendf, n - 5L)
  expect_equal(ftr$p, 0.764727138261, tolerance = tol)

  fv <- ols_test_f(mt, vars = c("disp", "wt"))
  expect_equal(fv$f, 0.186509178782, tolerance = tol)
  expect_equal(fv$f, unname(summary(lm(residuals(mt)^2 ~ X[, c("disp", "wt")]))$fstatistic[1]),
    tolerance = tol)
  expect_equal(fv$numdf, 2L)
  expect_equal(fv$p, 0.830838488933, tolerance = tol)

  # fitness, fitted values
  fit <- golden_models$fitness
  ff <- ols_test_f(fit)
  expect_equal(ff$f, 0.300341507624, tolerance = tol)
  expect_equal(ff$numdf, 1L)
  expect_equal(ff$dendf, 29L)
  expect_equal(ff$p, 0.587862478107, tolerance = tol)
})

test_that("golden: ols_test_score() matches Koenker's studentized score test", {
  # score = n * R2 of the auxiliary regression of e^2 on the regressors
  # (Koenker 1981, Journal of Econometrics 17:107-112).
  mt <- golden_models$mtcars
  n <- nrow(model.frame(mt))
  X <- model.matrix(mt)[, -1]

  sc <- ols_test_score(mt)
  expect_equal(sc$score, 0.516395892519, tolerance = tol)
  expect_equal(sc$score, n * summary(lm(residuals(mt)^2 ~ fitted(mt)))$r.squared,
    tolerance = tol)
  expect_equal(sc$df, 1L)
  expect_equal(sc$p, 0.472383154003, tolerance = tol)
  expect_equal(sc$p, pchisq(n * summary(lm(residuals(mt)^2 ~ fitted(mt)))$r.squared, 1,
    lower.tail = FALSE), tolerance = tol)

  scr <- ols_test_score(mt, rhs = TRUE)
  expect_equal(scr$score, 2.03940403475, tolerance = tol)
  expect_equal(scr$score, n * summary(lm(residuals(mt)^2 ~ X))$r.squared, tolerance = tol)
  expect_equal(scr$df, 4L)
  expect_equal(scr$p, 0.728511379515, tolerance = tol)

  scv <- ols_test_score(mt, vars = c("disp", "wt"))
  expect_equal(scv$score, 0.406379327339, tolerance = tol)
  expect_equal(scv$score, n * summary(lm(residuals(mt)^2 ~ X[, c("disp", "wt")]))$r.squared,
    tolerance = tol)
  expect_equal(scv$df, 2L)
  expect_equal(scv$p, 0.81612343778, tolerance = tol)

  fit <- golden_models$fitness
  fs <- ols_test_score(fit)
  expect_equal(fs$score, 0.317763761693, tolerance = tol)
  expect_equal(fs$df, 1L)
  expect_equal(fs$p, 0.572954651599, tolerance = tol)
})

test_that("golden: ols_test_bartlett() matches stats::bartlett.test()", {
  # Bartlett's K-squared from bartlett.test()
  bt <- ols_test_bartlett(mtcars, "mpg", group_var = "cyl")
  expect_equal(bt$fstat, 8.393395601963, tolerance = tol)
  expect_equal(bt$df, 2L)
  expect_equal(bt$pval, 0.0150451770494, tolerance = tol)
  ref <- bartlett.test(mpg ~ factor(cyl), data = mtcars)
  expect_equal(bt$fstat, unname(ref$statistic), tolerance = tol)
  expect_equal(bt$df, unname(ref$parameter), tolerance = tol)
  expect_equal(bt$pval, ref$p.value, tolerance = tol)

  # variables interface: bartlett.test(list(auto$price, auto$mpg))
  bt2 <- ols_test_bartlett(auto, "price", "mpg")
  expect_equal(bt2$fstat, 803.466869733598, tolerance = tol)
  expect_equal(bt2$df, 1L)
  expect_equal(bt2$pval, bartlett.test(list(auto$price, auto$mpg))$p.value, tolerance = tol)
})

test_that("golden: ols_test_normality() matches the reference tests", {
  # mtcars residuals: ks.test(y, "pnorm", mean(y), sd(y)); shapiro.test(y);
  # goftest::cvm.test(y); nortest::ad.test(y)
  nt <- ols_test_normality(golden_models$mtcars)
  expect_equal(unname(nt$kolmogorv$statistic), 0.115238409562, tolerance = tol)
  expect_equal(nt$kolmogorv$p.value, 0.746376839491, tolerance = tol)
  expect_equal(unname(nt$shapiro$statistic), 0.936613845275, tolerance = tol)
  expect_equal(nt$shapiro$p.value, 0.0600389289932, tolerance = tol)
  expect_equal(unname(nt$cramer$statistic), 2.81221616686, tolerance = tol)
  expect_equal(nt$cramer$p.value, 1.335145694e-08, tolerance = tol)
  expect_equal(unname(nt$anderson$statistic), 0.585919246391, tolerance = tol)
  expect_equal(nt$anderson$p.value, 0.118796346828, tolerance = tol)

  # cross-check against the reference implementations themselves
  r <- residuals(golden_models$mtcars)
  expect_equal(unname(nt$kolmogorv$statistic),
    unname(ks.test(r, "pnorm", mean(r), sd(r))$statistic), tolerance = tol)
  expect_equal(nt$shapiro$p.value, shapiro.test(r)$p.value, tolerance = tol)
  expect_equal(unname(nt$cramer$statistic), unname(goftest::cvm.test(r)$statistic),
    tolerance = tol)
  expect_equal(unname(nt$anderson$statistic), unname(nortest::ad.test(r)$statistic),
    tolerance = tol)

  # fitness residuals
  nt2 <- ols_test_normality(golden_models$fitness)
  expect_equal(unname(nt2$kolmogorv$statistic), 0.13665783653, tolerance = tol)
  expect_equal(nt2$shapiro$p.value, 0.460321491288, tolerance = tol)
  expect_equal(unname(nt2$cramer$statistic), 2.35893180597, tolerance = tol)
  expect_equal(unname(nt2$anderson$statistic), 0.513678819837, tolerance = tol)
  expect_equal(nt2$anderson$p.value, 0.178641898632, tolerance = tol)
})

test_that("golden: ols_coll_diag() VIF and tolerance vs 1 / (1 - R2_j)", {
  # Tolerance_j = 1 - R2 of regressing x_j on the other model-matrix columns;
  # VIF_j = 1 / Tolerance_j (Belsley, Kuh & Welsch 1980).
  for (nm in names(golden_models)) {
    m <- golden_models[[nm]]
    e <- expected[[nm]]
    X <- model.matrix(m)[, -1]
    r2 <- vapply(seq_len(ncol(X)), function(j) {
      summary(lm(X[, j] ~ X[, -j, drop = FALSE]))$r.squared
    }, numeric(1))
    vif <- 1 / (1 - r2)
    vt <- ols_coll_diag(m)$vif_t

    expect_equal(vt$VIF, e$vif, tolerance = tol, info = nm)
    expect_equal(vt$VIF, unname(vif), tolerance = tol, info = nm)
    expect_equal(vt$Tolerance, e$tol, tolerance = tol, info = nm)
    expect_equal(vt$Tolerance, unname(1 / vif), tolerance = tol, info = nm)
  }
})

test_that("golden: ols_pure_error_anova() matches the lack-of-fit ANOVA", {
  # anova(small, big) reports the lack-of-fit comparison on the row of the big
  # model: Df = number of distinct x - 2, Sum of Sq = RSS(small) - RSS(big),
  # F = (lack of fit MS) / (pure error MS); the big model's RSS is the pure
  # error SS with df = n - number of distinct x.
  m <- lm(mpg ~ wt, data = mtcars)
  a <- anova(m, lm(mpg ~ factor(wt), data = mtcars))
  pe <- ols_pure_error_anova(m)

  expect_equal(pe$lackoffit, 277.070270877, tolerance = tol)   # a[2, "Sum of Sq"]
  expect_equal(pe$lackoffit, a[2, "Sum of Sq"], tolerance = tol)
  expect_equal(pe$pure_error, 1.25166666667, tolerance = tol)  # a[2, "RSS"]
  expect_equal(pe$pure_error, a[2, "RSS"], tolerance = tol)
  expect_equal(pe$df_lof, 27L)                                 # a[2, "Df"]
  expect_equal(pe$df_lof, a[2, "Df"])
  expect_equal(pe$df_error, 3L)                                # a[2, "Res.Df"]
  expect_equal(pe$df_error, a[2, "Res.Df"])
  expect_equal(pe$lf, 24.59567429, tolerance = tol)            # a[2, "F"]
  expect_equal(pe$lf, a[2, "F"], tolerance = tol)
  expect_equal(pe$pl, 0.0111795870971, tolerance = tol)        # a[2, "Pr(>F)"]
  expect_equal(pe$pl, a[2, "Pr(>F)"], tolerance = tol)
  # lf can also be written from the two mean squares
  expect_equal(pe$lf,
    (a[2, "Sum of Sq"] / a[2, "Df"]) / (a[2, "RSS"] / a[2, "Res.Df"]),
    tolerance = tol)
  # overall regression F uses the error mean square: summary(m)$fstatistic
  expect_equal(pe$rf, 91.3753250038, tolerance = tol)
  expect_equal(pe$rf, unname(summary(m)$fstatistic[1]), tolerance = tol)
  expect_equal(pe$pr, 1.29395870135e-10, tolerance = tol)
  expect_equal(pe$rms, 847.725249957, tolerance = tol)         # anova(m)[1, 2]
  expect_equal(pe$ems, 9.27739791811, tolerance = tol)         # anova(m)[2, 3]
})

# Variable selection -----------------------------------------------------------
# Independent base-R re-implementation of the p-value stepwise algorithms:
# forward entry at p < 0.3 (ties broken by the largest partial F), backward
# elimination of the term with the largest p > 0.3.  Partial p-values come from
# the t statistic of each added/removed term, which equals car::Anova()'s
# type II partial F p-value for additive numeric models.
fwd_p_base <- function(data, response, p_enter = 0.3) {
  preds <- character(0)
  repeat {
    cand <- setdiff(names(data)[names(data) != response], preds)
    if (!length(cand)) break
    pv <- fv <- numeric(length(cand))
    for (j in seq_along(cand)) {
      m <- lm(reformulate(c(preds, cand[j]), response), data = data)
      pv[j] <- summary(m)$coef[cand[j], 4]
      fv[j] <- summary(m)$coef[cand[j], 3]^2
    }
    ord <- order(pv, -fv)
    if (pv[ord[1]] > p_enter) break
    preds <- c(preds, cand[ord[1]])
  }
  preds
}

bwd_p_base <- function(data, response, p_remove = 0.3) {
  preds <- setdiff(names(data), response)
  repeat {
    if (!length(preds)) break
    m <- lm(reformulate(preds, response), data = data)
    pv <- summary(m)$coef[-1, 4]
    ord <- order(pv, decreasing = TRUE)
    if (pv[ord[1]] <= p_remove) break
    preds <- preds[-ord[1]]
  }
  preds
}

test_that("golden: ols_step_best_subset() selects the maximum-R2 subset at each size", {
  # Expected models from an exhaustive base-R search:
  #   combn(predictors, k) -> lm() -> summary()$r.squared -> which.max()
  cases <- list(
    surgical = list(
      data = surgical, response = "y", max_order = 8L,
      predictors = c(
        "liver_test",
        "liver_test alc_heavy",
        "bcs pindex enzyme_test",
        "bcs pindex enzyme_test alc_heavy",
        "bcs pindex enzyme_test liver_test alc_heavy",
        "bcs pindex enzyme_test liver_test age alc_heavy",
        "bcs pindex enzyme_test liver_test age gender alc_heavy",
        "bcs pindex enzyme_test liver_test age gender alc_mod alc_heavy"
      ),
      rsquare = c(0.454538878271, 0.566740920874, 0.684127504935, 0.771356523512,
        0.780905436725, 0.781416915271, 0.781770315292, 0.781838672227),
      aic = c(771.875327221, 761.43936225, 746.37551082, 730.924076135,
        730.620409851, 732.494198909, 734.406822319, 736.389905038)
    ),
    stepdata = list(
      data = stepdata, response = "y", max_order = 6L,
      predictors = c("x6", "x1 x6", "x1 x2 x3", "x1 x2 x3 x4",
        "x1 x2 x3 x4 x5", "x1 x2 x3 x4 x5 x6"),
      rsquare = c(0.691446262957, 0.699718878247, 0.749578114092, 0.749621977261,
        0.749622762217, 0.749622970634),
      aic = c(33473.296609, 32931.7584503, 29302.3170664, 29300.8136178,
        29302.750916, 29304.7342678)
    )
  )

  for (nm in names(cases)) {
    cs <- cases[[nm]]
    d <- cs$data
    k <- ols_step_best_subset(lm(reformulate(setdiff(names(d), cs$response),
      cs$response), data = d))

    expect_equal(k$metrics$n, seq_len(cs$max_order), info = nm)
    expect_equal(k$metrics$predictors, cs$predictors, ignore_attr = TRUE, info = nm)
    expect_equal(k$metrics$rsquare, cs$rsquare, tolerance = tol, info = nm)
    expect_equal(k$metrics$aic, cs$aic, tolerance = tol, info = nm)

    # independent exhaustive check: each selected subset maximises R2 at its size
    candidates <- setdiff(names(d), cs$response)
    for (size in seq_len(cs$max_order)) {
      combos <- combn(candidates, size)
      r2 <- apply(combos, 2, function(v) {
        summary(lm(reformulate(v, cs$response), data = d))$r.squared
      })
      best <- sort(combos[, which.max(r2)])
      selected <- sort(strsplit(cs$predictors[size], " ")[[1]])
      expect_equal(selected, best, info = paste(nm, "size", size))
    }

    # the selected model's R2 and AIC match a direct lm() of those terms
    for (size in seq_len(cs$max_order)) {
      fit <- lm(reformulate(strsplit(cs$predictors[size], " ")[[1]],
        cs$response), data = d)
      expect_equal(summary(fit)$r.squared, cs$rsquare[size], tolerance = tol,
        info = paste(nm, "size", size))
      expect_equal(AIC(fit), cs$aic[size], tolerance = tol,
        info = paste(nm, "size", size))
    }
  }
})

test_that("golden: ols_step_forward_p()/ols_step_backward_p() match base R", {
  cases <- list(
    surgical = list(
      data = surgical,
      forward = c("liver_test", "alc_heavy", "enzyme_test", "pindex", "bcs"),
      backward = c("bcs", "pindex", "enzyme_test", "liver_test", "alc_heavy"),
      rsquare = 0.780905436725,   # summary(lm(y ~ liver_test + alc_heavy +
                                  #   enzyme_test + pindex + bcs, surgical))$r.squared
      aic = 730.620409851        # AIC(lm(y ~ liver_test + alc_heavy +
                                  #   enzyme_test + pindex + bcs, surgical))
    ),
    stepdata = list(
      data = stepdata,
      forward = c("x6", "x1", "x3", "x2", "x4"),
      backward = c("x1", "x2", "x3", "x4"),
      rsquare = 0.749622185287,   # summary(lm(y ~ x6 + x1 + x3 + x2 + x4, stepdata))$r.squared
      aic = 29302.7970008         # AIC(lm(y ~ x6 + x1 + x3 + x2 + x4, stepdata))
    )
  )

  for (nm in names(cases)) {
    cs <- cases[[nm]]
    d <- cs$data
    full <- lm(reformulate(setdiff(names(d), "y"), "y"), data = d)

    fwd <- ols_step_forward_p(full)
    expect_equal(names(coef(fwd$model))[-1], cs$forward, info = nm)
    expect_equal(names(coef(fwd$model))[-1], fwd_p_base(d, "y"), info = nm)
    expect_equal(summary(fwd$model)$r.squared, cs$rsquare, tolerance = tol, info = nm)
    expect_equal(AIC(fwd$model), cs$aic, tolerance = tol, info = nm)

    bwd <- ols_step_backward_p(full)
    expect_equal(sort(names(coef(bwd$model))[-1]), sort(cs$backward), info = nm)
    expect_equal(sort(names(coef(bwd$model))[-1]), sort(bwd_p_base(d, "y")), info = nm)
    # backward elimination ends on the same R2/AIC as the direct refit
    fit <- lm(reformulate(sort(cs$backward), "y"), data = d)
    expect_equal(summary(bwd$model)$r.squared, summary(fit)$r.squared, tolerance = tol,
      info = nm)
    expect_equal(AIC(bwd$model), AIC(fit), tolerance = tol, info = nm)
  }
})

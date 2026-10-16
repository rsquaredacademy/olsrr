# Studentized residual vs leverage plot data

Generates data for studentized resiudual vs leverage plot.

## Usage

``` r
ols_prep_rstudlev_data(model, threshold = NULL)
```

## Arguments

- model:

  An object of class `lm`.

- threshold:

  Threshold for detecting outliers. Default is 2.

## Examples

``` r
model <- lm(read ~ write + math + science, data = hsb)
ols_prep_rstudlev_data(model)
#> $levrstud
#>     obs    leverage      rstudent              color          fct_color
#> 1     1 0.016663013  1.5615178576             normal             normal
#> 2     2 0.015958467  1.5674686935             normal             normal
#> 3     3 0.055990789 -0.8622668267           leverage           leverage
#> 4     4 0.013278027  2.1255029597            outlier            outlier
#> 5     5 0.007234223 -1.0086194988             normal             normal
#> 6     6 0.018982871 -1.5536180050             normal             normal
#> 7     7 0.028207407 -0.0033083160             normal             normal
#> 8     8 0.013469959 -1.4291604750             normal             normal
#> 9     9 0.007794713  1.0654028866             normal             normal
#> 10   10 0.006109328  0.7204677404             normal             normal
#> 11   11 0.009636149  1.3900119349             normal             normal
#> 12   12 0.027302624 -0.1190230101             normal             normal
#> 13   13 0.026214458  1.3622240361             normal             normal
#> 14   14 0.011567201 -0.4650712801             normal             normal
#> 15   15 0.049225149 -0.1522931868           leverage           leverage
#> 16   16 0.012197431 -0.7369695795             normal             normal
#> 17   17 0.005225105 -0.5538375471             normal             normal
#> 18   18 0.008282313 -0.1206769984             normal             normal
#> 19   19 0.016057765  1.2094871126             normal             normal
#> 20   20 0.031691152  0.5875672266             normal             normal
#> 21   21 0.079987477  2.1249199600 outlier & leverage outlier & leverage
#> 22   22 0.038690269 -0.8885924544             normal             normal
#> 23   23 0.024796396  0.3989106927             normal             normal
#> 24   24 0.012184872  0.5148854800             normal             normal
#> 25   25 0.013774377 -0.9253782818             normal             normal
#> 26   26 0.021954211 -1.1784121774             normal             normal
#> 27   27 0.016656157  0.3009278833             normal             normal
#> 28   28 0.007015637 -1.3949833362             normal             normal
#> 29   29 0.023947729  0.0687139016             normal             normal
#> 30   30 0.014919053  0.3301869369             normal             normal
#> 31   31 0.060332209 -0.7746718907           leverage           leverage
#> 32   32 0.022532977  2.3453042406            outlier            outlier
#> 33   33 0.018797715  0.1825653880             normal             normal
#> 34   34 0.068933532 -2.2528504213 outlier & leverage outlier & leverage
#> 35   35 0.017763012 -0.4927644910             normal             normal
#> 36   36 0.014536648  0.0173928975             normal             normal
#> 37   37 0.049206361  0.4119453403           leverage           leverage
#> 38   38 0.020025025  0.0813920657             normal             normal
#> 39   39 0.035928203  1.1535955907             normal             normal
#> 40   40 0.031561577  0.0790182178             normal             normal
#> 41   41 0.013572369  0.4986514768             normal             normal
#> 42   42 0.010027638 -0.5841201617             normal             normal
#> 43   43 0.006334353 -0.4439399459             normal             normal
#> 44   44 0.017061392 -0.4647805161             normal             normal
#> 45   45 0.035564567  1.2103260683             normal             normal
#> 46   46 0.021088934 -1.1152833455             normal             normal
#> 47   47 0.022605754  0.9846953174             normal             normal
#> 48   48 0.024263530  0.2037596196             normal             normal
#> 49   49 0.014153579 -1.5841455905             normal             normal
#> 50   50 0.014341980  0.5046561856             normal             normal
#> 51   51 0.009980292 -0.8428749146             normal             normal
#> 52   52 0.033699644 -0.2244305253             normal             normal
#> 53   53 0.027622072 -1.2704393116             normal             normal
#> 54   54 0.013124733  1.4031499379             normal             normal
#> 55   55 0.031840490  0.8388462618             normal             normal
#> 56   56 0.044295731  0.1444303526           leverage           leverage
#> 57   57 0.026517891 -0.2679828969             normal             normal
#> 58   58 0.038734380 -0.4398184906             normal             normal
#> 59   59 0.007307716  1.0105795250             normal             normal
#> 60   60 0.009862399 -0.1085065460             normal             normal
#> 61   61 0.021828959  0.0643870692             normal             normal
#> 62   62 0.020458503 -2.2867946914            outlier            outlier
#> 63   63 0.022694326  0.6230980698             normal             normal
#> 64   64 0.028589146 -0.6392045195             normal             normal
#> 65   65 0.024840216  0.8728884281             normal             normal
#> 66   66 0.021137375 -0.0139993151             normal             normal
#> 67   67 0.009172486  0.5386382740             normal             normal
#> 68   68 0.017134182  0.4464274622             normal             normal
#> 69   69 0.040440609  0.9891610642           leverage           leverage
#> 70   70 0.033437870  0.8974746735             normal             normal
#> 71   71 0.014044304  0.0266382273             normal             normal
#> 72   72 0.008020965  0.5191148106             normal             normal
#> 73   73 0.025324219  1.0321756789             normal             normal
#> 74   74 0.036634172  0.8269809015             normal             normal
#> 75   75 0.021462694  0.7654046005             normal             normal
#> 76   76 0.014695884  0.5012495411             normal             normal
#> 77   77 0.013395096  0.1647799838             normal             normal
#> 78   78 0.012650645  1.3992743502             normal             normal
#> 79   79 0.023877061 -1.5838362422             normal             normal
#> 80   80 0.023944531 -0.0027914606             normal             normal
#> 81   81 0.007259335  0.9613788128             normal             normal
#> 82   82 0.015743305 -2.7157286565            outlier            outlier
#> 83   83 0.027927049  0.4544767500             normal             normal
#> 84   84 0.035564567  0.9176207661             normal             normal
#> 85   85 0.015802654  1.8373806393             normal             normal
#> 86   86 0.013593984 -1.0111400482             normal             normal
#> 87   87 0.018450590  0.4099709140             normal             normal
#> 88   88 0.018152864 -1.3353363732             normal             normal
#> 89   89 0.034064291  1.9065087673             normal             normal
#> 90   90 0.020048187 -0.3183460774             normal             normal
#> 91   91 0.033632340 -0.8765367429             normal             normal
#> 92   92 0.033055971  1.2869743107             normal             normal
#> 93   93 0.020615606  0.5489294217             normal             normal
#> 94   94 0.013566003 -1.2341012351             normal             normal
#> 95   95 0.009552626 -0.3135792639             normal             normal
#> 96   96 0.012218735 -1.5716965630             normal             normal
#> 97   97 0.026586251  0.6272069017             normal             normal
#> 98   98 0.026388503 -0.6031937785             normal             normal
#> 99   99 0.015834328 -1.0867225368             normal             normal
#> 100 100 0.020602109 -0.0814710621             normal             normal
#> 101 101 0.020879765  0.6722859307             normal             normal
#> 102 102 0.017775829 -1.7650237091             normal             normal
#> 103 103 0.028718728  0.4855645788             normal             normal
#> 104 104 0.005818563  1.5805326069             normal             normal
#> 105 105 0.012942231 -0.6547693963             normal             normal
#> 106 106 0.045440580  0.0510560203           leverage           leverage
#> 107 107 0.029142127  0.1004793698             normal             normal
#> 108 108 0.033450547 -1.1587167698             normal             normal
#> 109 109 0.010222737 -0.3520283143             normal             normal
#> 110 110 0.023366186  1.1354139618             normal             normal
#> 111 111 0.040884122  1.0243345531           leverage           leverage
#> 112 112 0.018492440  0.0937148829             normal             normal
#> 113 113 0.016203307  1.6459429024             normal             normal
#> 114 114 0.013839516  1.2554157147             normal             normal
#> 115 115 0.009016837  2.3559909773            outlier            outlier
#> 116 116 0.019111139 -0.7635191658             normal             normal
#> 117 117 0.005578793 -1.6323381935             normal             normal
#> 118 118 0.017013644  1.7411403007             normal             normal
#> 119 119 0.013153495 -1.0581953789             normal             normal
#> 120 120 0.016962138 -0.1806400874             normal             normal
#> 121 121 0.006236051  1.2960038448             normal             normal
#> 122 122 0.008782123  0.1456903754             normal             normal
#> 123 123 0.020346175  0.2354572097             normal             normal
#> 124 124 0.008896642 -0.3195889441             normal             normal
#> 125 125 0.053141001  0.1128426801           leverage           leverage
#> 126 126 0.007048024 -0.1901603269             normal             normal
#> 127 127 0.012809844  0.8320896124             normal             normal
#> 128 128 0.005753302 -1.9104540241             normal             normal
#> 129 129 0.011374502 -0.1665475040             normal             normal
#> 130 130 0.035901817 -0.4084949452             normal             normal
#> 131 131 0.009524264  0.4765839564             normal             normal
#> 132 132 0.037321670 -0.8872732015             normal             normal
#> 133 133 0.013901672  1.8610380414             normal             normal
#> 134 134 0.009466933 -0.9967369630             normal             normal
#> 135 135 0.018295317  1.5921286954             normal             normal
#> 136 136 0.017749057  2.0424695837            outlier            outlier
#> 137 137 0.008931979 -1.3005825705             normal             normal
#> 138 138 0.012892579 -0.6612591381             normal             normal
#> 139 139 0.028011360 -0.0105387554             normal             normal
#> 140 140 0.008262128  0.6269023965             normal             normal
#> 141 141 0.010509552 -2.4187322569            outlier            outlier
#> 142 142 0.006236051 -1.2875918541             normal             normal
#> 143 143 0.022735873  0.2004370441             normal             normal
#> 144 144 0.015310602 -0.3081322343             normal             normal
#> 145 145 0.008407498 -0.5838887690             normal             normal
#> 146 146 0.012906681 -0.0362707248             normal             normal
#> 147 147 0.040590038 -0.1383922701           leverage           leverage
#> 148 148 0.013672069 -0.2963394272             normal             normal
#> 149 149 0.015201278  0.5174614703             normal             normal
#> 150 150 0.033936080  0.1178180142             normal             normal
#> 151 151 0.018936946  0.7337200979             normal             normal
#> 152 152 0.010741826  1.0762759940             normal             normal
#> 153 153 0.028214013 -0.7091009803             normal             normal
#> 154 154 0.007457168 -0.6418437597             normal             normal
#> 155 155 0.010519730 -0.0742232793             normal             normal
#> 156 156 0.009781974  1.4675437974             normal             normal
#> 157 157 0.007454320 -0.0566833495             normal             normal
#> 158 158 0.010328810 -1.3193821602             normal             normal
#> 159 159 0.016107870 -0.0004592609             normal             normal
#> 160 160 0.019679472  0.3224538910             normal             normal
#> 161 161 0.012649547 -1.1362533725             normal             normal
#> 162 162 0.005661751  0.9570892807             normal             normal
#> 163 163 0.021174819 -1.4292598994             normal             normal
#> 164 164 0.027963108 -1.0817808039             normal             normal
#> 165 165 0.014880784 -0.1273570734             normal             normal
#> 166 166 0.015152319  0.3100580108             normal             normal
#> 167 167 0.012942231 -0.9430087540             normal             normal
#> 168 168 0.024401363 -1.0004205310             normal             normal
#> 169 169 0.013796169 -0.3266552443             normal             normal
#> 170 170 0.011318388  0.4779826970             normal             normal
#> 171 171 0.015692634 -1.1225943231             normal             normal
#> 172 172 0.017221984  0.4290093591             normal             normal
#> 173 173 0.031361961  0.5760876827             normal             normal
#> 174 174 0.010754402  0.0048134988             normal             normal
#> 175 175 0.022042606  0.1356998665             normal             normal
#> 176 176 0.017049213  0.5377568859             normal             normal
#> 177 177 0.020075549  0.6534943450             normal             normal
#> 178 178 0.005119677 -2.0759807456            outlier            outlier
#> 179 179 0.019170721 -0.1408588779             normal             normal
#> 180 180 0.014247870 -1.0289602984             normal             normal
#> 181 181 0.018523463  1.0827262779             normal             normal
#> 182 182 0.014704534 -0.2525935618             normal             normal
#> 183 183 0.020139498 -1.2382939110             normal             normal
#> 184 184 0.019967920 -0.0686983378             normal             normal
#> 185 185 0.019450495 -2.1645087644            outlier            outlier
#> 186 186 0.015595131  0.5376914283             normal             normal
#> 187 187 0.005334097 -0.2505785670             normal             normal
#> 188 188 0.012309618  0.5464032574             normal             normal
#> 189 189 0.010387939  0.9146555195             normal             normal
#> 190 190 0.010301379 -0.1569159057             normal             normal
#> 191 191 0.021343052 -0.4596571444             normal             normal
#> 192 192 0.022523544 -1.8397072164             normal             normal
#> 193 193 0.006063512 -0.4454613699             normal             normal
#> 194 194 0.046608465 -0.4936746801           leverage           leverage
#> 195 195 0.021062171 -1.5018059729             normal             normal
#> 196 196 0.021220962  0.6428872251             normal             normal
#> 197 197 0.022121805  0.2346403284             normal             normal
#> 198 198 0.027120625  0.7210780094             normal             normal
#> 199 199 0.009824188 -0.4691841142             normal             normal
#> 200 200 0.022259921  0.4105220868             normal             normal
#> 
#> $lev_thrsh
#> [1] 0.04
#> 
#> $minx
#> [1] 0.005119677
#> 
#> $miny
#> [1] -5.715729
#> 
#> $maxx
#> [1] 0.07998748
#> 
#> $maxy
#> [1] 5.355991
#> 
ols_prep_rstudlev_data(model, threshold = 3)
#> $levrstud
#>     obs    leverage      rstudent    color fct_color
#> 1     1 0.016663013  1.5615178576   normal    normal
#> 2     2 0.015958467  1.5674686935   normal    normal
#> 3     3 0.055990789 -0.8622668267 leverage  leverage
#> 4     4 0.013278027  2.1255029597   normal    normal
#> 5     5 0.007234223 -1.0086194988   normal    normal
#> 6     6 0.018982871 -1.5536180050   normal    normal
#> 7     7 0.028207407 -0.0033083160   normal    normal
#> 8     8 0.013469959 -1.4291604750   normal    normal
#> 9     9 0.007794713  1.0654028866   normal    normal
#> 10   10 0.006109328  0.7204677404   normal    normal
#> 11   11 0.009636149  1.3900119349   normal    normal
#> 12   12 0.027302624 -0.1190230101   normal    normal
#> 13   13 0.026214458  1.3622240361   normal    normal
#> 14   14 0.011567201 -0.4650712801   normal    normal
#> 15   15 0.049225149 -0.1522931868 leverage  leverage
#> 16   16 0.012197431 -0.7369695795   normal    normal
#> 17   17 0.005225105 -0.5538375471   normal    normal
#> 18   18 0.008282313 -0.1206769984   normal    normal
#> 19   19 0.016057765  1.2094871126   normal    normal
#> 20   20 0.031691152  0.5875672266   normal    normal
#> 21   21 0.079987477  2.1249199600 leverage  leverage
#> 22   22 0.038690269 -0.8885924544   normal    normal
#> 23   23 0.024796396  0.3989106927   normal    normal
#> 24   24 0.012184872  0.5148854800   normal    normal
#> 25   25 0.013774377 -0.9253782818   normal    normal
#> 26   26 0.021954211 -1.1784121774   normal    normal
#> 27   27 0.016656157  0.3009278833   normal    normal
#> 28   28 0.007015637 -1.3949833362   normal    normal
#> 29   29 0.023947729  0.0687139016   normal    normal
#> 30   30 0.014919053  0.3301869369   normal    normal
#> 31   31 0.060332209 -0.7746718907 leverage  leverage
#> 32   32 0.022532977  2.3453042406   normal    normal
#> 33   33 0.018797715  0.1825653880   normal    normal
#> 34   34 0.068933532 -2.2528504213 leverage  leverage
#> 35   35 0.017763012 -0.4927644910   normal    normal
#> 36   36 0.014536648  0.0173928975   normal    normal
#> 37   37 0.049206361  0.4119453403 leverage  leverage
#> 38   38 0.020025025  0.0813920657   normal    normal
#> 39   39 0.035928203  1.1535955907   normal    normal
#> 40   40 0.031561577  0.0790182178   normal    normal
#> 41   41 0.013572369  0.4986514768   normal    normal
#> 42   42 0.010027638 -0.5841201617   normal    normal
#> 43   43 0.006334353 -0.4439399459   normal    normal
#> 44   44 0.017061392 -0.4647805161   normal    normal
#> 45   45 0.035564567  1.2103260683   normal    normal
#> 46   46 0.021088934 -1.1152833455   normal    normal
#> 47   47 0.022605754  0.9846953174   normal    normal
#> 48   48 0.024263530  0.2037596196   normal    normal
#> 49   49 0.014153579 -1.5841455905   normal    normal
#> 50   50 0.014341980  0.5046561856   normal    normal
#> 51   51 0.009980292 -0.8428749146   normal    normal
#> 52   52 0.033699644 -0.2244305253   normal    normal
#> 53   53 0.027622072 -1.2704393116   normal    normal
#> 54   54 0.013124733  1.4031499379   normal    normal
#> 55   55 0.031840490  0.8388462618   normal    normal
#> 56   56 0.044295731  0.1444303526 leverage  leverage
#> 57   57 0.026517891 -0.2679828969   normal    normal
#> 58   58 0.038734380 -0.4398184906   normal    normal
#> 59   59 0.007307716  1.0105795250   normal    normal
#> 60   60 0.009862399 -0.1085065460   normal    normal
#> 61   61 0.021828959  0.0643870692   normal    normal
#> 62   62 0.020458503 -2.2867946914   normal    normal
#> 63   63 0.022694326  0.6230980698   normal    normal
#> 64   64 0.028589146 -0.6392045195   normal    normal
#> 65   65 0.024840216  0.8728884281   normal    normal
#> 66   66 0.021137375 -0.0139993151   normal    normal
#> 67   67 0.009172486  0.5386382740   normal    normal
#> 68   68 0.017134182  0.4464274622   normal    normal
#> 69   69 0.040440609  0.9891610642 leverage  leverage
#> 70   70 0.033437870  0.8974746735   normal    normal
#> 71   71 0.014044304  0.0266382273   normal    normal
#> 72   72 0.008020965  0.5191148106   normal    normal
#> 73   73 0.025324219  1.0321756789   normal    normal
#> 74   74 0.036634172  0.8269809015   normal    normal
#> 75   75 0.021462694  0.7654046005   normal    normal
#> 76   76 0.014695884  0.5012495411   normal    normal
#> 77   77 0.013395096  0.1647799838   normal    normal
#> 78   78 0.012650645  1.3992743502   normal    normal
#> 79   79 0.023877061 -1.5838362422   normal    normal
#> 80   80 0.023944531 -0.0027914606   normal    normal
#> 81   81 0.007259335  0.9613788128   normal    normal
#> 82   82 0.015743305 -2.7157286565   normal    normal
#> 83   83 0.027927049  0.4544767500   normal    normal
#> 84   84 0.035564567  0.9176207661   normal    normal
#> 85   85 0.015802654  1.8373806393   normal    normal
#> 86   86 0.013593984 -1.0111400482   normal    normal
#> 87   87 0.018450590  0.4099709140   normal    normal
#> 88   88 0.018152864 -1.3353363732   normal    normal
#> 89   89 0.034064291  1.9065087673   normal    normal
#> 90   90 0.020048187 -0.3183460774   normal    normal
#> 91   91 0.033632340 -0.8765367429   normal    normal
#> 92   92 0.033055971  1.2869743107   normal    normal
#> 93   93 0.020615606  0.5489294217   normal    normal
#> 94   94 0.013566003 -1.2341012351   normal    normal
#> 95   95 0.009552626 -0.3135792639   normal    normal
#> 96   96 0.012218735 -1.5716965630   normal    normal
#> 97   97 0.026586251  0.6272069017   normal    normal
#> 98   98 0.026388503 -0.6031937785   normal    normal
#> 99   99 0.015834328 -1.0867225368   normal    normal
#> 100 100 0.020602109 -0.0814710621   normal    normal
#> 101 101 0.020879765  0.6722859307   normal    normal
#> 102 102 0.017775829 -1.7650237091   normal    normal
#> 103 103 0.028718728  0.4855645788   normal    normal
#> 104 104 0.005818563  1.5805326069   normal    normal
#> 105 105 0.012942231 -0.6547693963   normal    normal
#> 106 106 0.045440580  0.0510560203 leverage  leverage
#> 107 107 0.029142127  0.1004793698   normal    normal
#> 108 108 0.033450547 -1.1587167698   normal    normal
#> 109 109 0.010222737 -0.3520283143   normal    normal
#> 110 110 0.023366186  1.1354139618   normal    normal
#> 111 111 0.040884122  1.0243345531 leverage  leverage
#> 112 112 0.018492440  0.0937148829   normal    normal
#> 113 113 0.016203307  1.6459429024   normal    normal
#> 114 114 0.013839516  1.2554157147   normal    normal
#> 115 115 0.009016837  2.3559909773   normal    normal
#> 116 116 0.019111139 -0.7635191658   normal    normal
#> 117 117 0.005578793 -1.6323381935   normal    normal
#> 118 118 0.017013644  1.7411403007   normal    normal
#> 119 119 0.013153495 -1.0581953789   normal    normal
#> 120 120 0.016962138 -0.1806400874   normal    normal
#> 121 121 0.006236051  1.2960038448   normal    normal
#> 122 122 0.008782123  0.1456903754   normal    normal
#> 123 123 0.020346175  0.2354572097   normal    normal
#> 124 124 0.008896642 -0.3195889441   normal    normal
#> 125 125 0.053141001  0.1128426801 leverage  leverage
#> 126 126 0.007048024 -0.1901603269   normal    normal
#> 127 127 0.012809844  0.8320896124   normal    normal
#> 128 128 0.005753302 -1.9104540241   normal    normal
#> 129 129 0.011374502 -0.1665475040   normal    normal
#> 130 130 0.035901817 -0.4084949452   normal    normal
#> 131 131 0.009524264  0.4765839564   normal    normal
#> 132 132 0.037321670 -0.8872732015   normal    normal
#> 133 133 0.013901672  1.8610380414   normal    normal
#> 134 134 0.009466933 -0.9967369630   normal    normal
#> 135 135 0.018295317  1.5921286954   normal    normal
#> 136 136 0.017749057  2.0424695837   normal    normal
#> 137 137 0.008931979 -1.3005825705   normal    normal
#> 138 138 0.012892579 -0.6612591381   normal    normal
#> 139 139 0.028011360 -0.0105387554   normal    normal
#> 140 140 0.008262128  0.6269023965   normal    normal
#> 141 141 0.010509552 -2.4187322569   normal    normal
#> 142 142 0.006236051 -1.2875918541   normal    normal
#> 143 143 0.022735873  0.2004370441   normal    normal
#> 144 144 0.015310602 -0.3081322343   normal    normal
#> 145 145 0.008407498 -0.5838887690   normal    normal
#> 146 146 0.012906681 -0.0362707248   normal    normal
#> 147 147 0.040590038 -0.1383922701 leverage  leverage
#> 148 148 0.013672069 -0.2963394272   normal    normal
#> 149 149 0.015201278  0.5174614703   normal    normal
#> 150 150 0.033936080  0.1178180142   normal    normal
#> 151 151 0.018936946  0.7337200979   normal    normal
#> 152 152 0.010741826  1.0762759940   normal    normal
#> 153 153 0.028214013 -0.7091009803   normal    normal
#> 154 154 0.007457168 -0.6418437597   normal    normal
#> 155 155 0.010519730 -0.0742232793   normal    normal
#> 156 156 0.009781974  1.4675437974   normal    normal
#> 157 157 0.007454320 -0.0566833495   normal    normal
#> 158 158 0.010328810 -1.3193821602   normal    normal
#> 159 159 0.016107870 -0.0004592609   normal    normal
#> 160 160 0.019679472  0.3224538910   normal    normal
#> 161 161 0.012649547 -1.1362533725   normal    normal
#> 162 162 0.005661751  0.9570892807   normal    normal
#> 163 163 0.021174819 -1.4292598994   normal    normal
#> 164 164 0.027963108 -1.0817808039   normal    normal
#> 165 165 0.014880784 -0.1273570734   normal    normal
#> 166 166 0.015152319  0.3100580108   normal    normal
#> 167 167 0.012942231 -0.9430087540   normal    normal
#> 168 168 0.024401363 -1.0004205310   normal    normal
#> 169 169 0.013796169 -0.3266552443   normal    normal
#> 170 170 0.011318388  0.4779826970   normal    normal
#> 171 171 0.015692634 -1.1225943231   normal    normal
#> 172 172 0.017221984  0.4290093591   normal    normal
#> 173 173 0.031361961  0.5760876827   normal    normal
#> 174 174 0.010754402  0.0048134988   normal    normal
#> 175 175 0.022042606  0.1356998665   normal    normal
#> 176 176 0.017049213  0.5377568859   normal    normal
#> 177 177 0.020075549  0.6534943450   normal    normal
#> 178 178 0.005119677 -2.0759807456   normal    normal
#> 179 179 0.019170721 -0.1408588779   normal    normal
#> 180 180 0.014247870 -1.0289602984   normal    normal
#> 181 181 0.018523463  1.0827262779   normal    normal
#> 182 182 0.014704534 -0.2525935618   normal    normal
#> 183 183 0.020139498 -1.2382939110   normal    normal
#> 184 184 0.019967920 -0.0686983378   normal    normal
#> 185 185 0.019450495 -2.1645087644   normal    normal
#> 186 186 0.015595131  0.5376914283   normal    normal
#> 187 187 0.005334097 -0.2505785670   normal    normal
#> 188 188 0.012309618  0.5464032574   normal    normal
#> 189 189 0.010387939  0.9146555195   normal    normal
#> 190 190 0.010301379 -0.1569159057   normal    normal
#> 191 191 0.021343052 -0.4596571444   normal    normal
#> 192 192 0.022523544 -1.8397072164   normal    normal
#> 193 193 0.006063512 -0.4454613699   normal    normal
#> 194 194 0.046608465 -0.4936746801 leverage  leverage
#> 195 195 0.021062171 -1.5018059729   normal    normal
#> 196 196 0.021220962  0.6428872251   normal    normal
#> 197 197 0.022121805  0.2346403284   normal    normal
#> 198 198 0.027120625  0.7210780094   normal    normal
#> 199 199 0.009824188 -0.4691841142   normal    normal
#> 200 200 0.022259921  0.4105220868   normal    normal
#> 
#> $lev_thrsh
#> [1] 0.04
#> 
#> $minx
#> [1] 0.005119677
#> 
#> $miny
#> [1] -5.715729
#> 
#> $maxx
#> [1] 0.07998748
#> 
#> $maxy
#> [1] 5.355991
#> 

```

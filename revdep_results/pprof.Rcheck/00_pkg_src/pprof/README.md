# pprof

The `pprof` software package provides a variety of risk-adjusted models for provider profiling, efficiently handling large-scale provider data. It includes standardized measure calculations, hypothesis testing, and visualization tools for evaluating the performance of healthcare providers and identifying significant deviations from expected standards.

## Introduction

Provider profiling involves assessing and comparing the performance of healthcare providers by evaluating specific metrics that reflect quality of care, efficiency, and patient outcomes. To achieve this, it is essential to fit statistical models and design appropriate measures. We developed the `pprof` package that facilitates fitting a variety of risk-adjusted models, each of which includes tools for calculating standardized measures, conducting statistical inference, and visualizing results, thereby offering a comprehensive tool for provider profiling.

This package addresses key limitations in existing R functions for provider profiling, which often suffer from computational inefficiency when applied to large-scale provider data. For the logistic fixed effect model, the serial blockwise inversion Newton (SerBIN) algorithm is implemented, which leverages the block structure of the information matrix. For linear fixed effect models, a profile-based method is used. These, along with parallel computing capabilities, improve computational speed significantly. `pprof` handles diverse outcomes (e.g. binary and continuous) and offers both direct and indirect standardization. pprof provides a comprehensive and user-friendly tool for provider profiling, enabling users to fit risk-adjusted models, calculate standardized measures, perform hypothesis tests, and visualize results.

## Installation

**Note:** *The package is still in the early stages of development, so please don't hesitate to report any problems you may experience.*

You can install 'pprof' via CRAN or github:

```         
require("devtools")
require("remotes")
remotes::install_github("UM-KevinHe/pprof", ref = "main")
```

## Getting Help

If you encounter any problems or bugs, please contact us at: [xhliuu\@umich.edu](mailto:xhliuu@umich.edu){.email}, [lfluo\@umich.edu](mailto:lfluo@umich.edu){.email}, [kevinhe\@umich.edu](mailto:kevinhe@umich.edu){.email}.

## Contributing

We welcome contributions to the `pprof` package. Please see our [CONTRIBUTING.md](https://github.com/UM-KevinHe/pprof/blob/main/.github/CONTRIBUTING.md) file for detailed guidelines of how to contribute.

## References

$$1$$ Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. Journal of Statistical Software, 67(1), 1-48. <https://doi.org/10.18637/jss.v067.i01>

$$2$$ He, K., Kalbfleisch, J. D., Li, Y., & Li, Y. (2013). Evaluating hospital readmission rates in dialysis facilities; adjusting for hospital effects. Lifetime Data Analysis, 19, 490-512. <https://link.springer.com/article/10.1007/s10985-013-9264-6>

$$3$$ He, K. (2019). Indirect and direct standardization for evaluating transplant centers. Journal of Hospital Administration, 8(1), 9-14. <https://www.sciedupress.com/journal/index.php/jha/article/view/14304>

$$4$$ Hsiao, C. (2022). Analysis of panel data (No. 64). Cambridge University Press.

$$5$$ Wu, W., Kuriakose, J. P., Weng, W., Burney, R. E., & He, K. (2023). Test-specific funnel plots for healthcare provider profiling leveraging individual- and summary-level information. Health Services and Outcomes Research Methodology, 23(1), 45-58. <https://pubmed.ncbi.nlm.nih.gov/37621728/>

$$6$$ Wu, W., Yang, Y., Kang, J., & He, K. (2022). Improving large‐scale estimation and inference for profiling health care providers. Statistics in Medicine, 41(15), 2840-2853. <https://onlinelibrary.wiley.com/doi/full/10.1002/sim.938>

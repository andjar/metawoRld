# Example data: maternal IL-6 in preeclampsia

Small worked example for the metawoRld documentation: circulating
interleukin-6 (IL-6) in women with preeclampsia compared with normotensive
pregnant women. **This is not a systematic review.** The four studies were
chosen because they are open access and report IL-6 in different ways (mean
and SD, mean and SE, median and range, several groups and subgroups, two
time points).

Values were transcribed from the published articles (open-access full texts
and the journals' web pages) on 2026-11-13 by Claude (an AI model) for this
documentation and checked against the source text; every value carries its
location in the paper (`source`). The people in `people.yml` of the example
project are example personas. Check the original articles before reusing any
value.

| Study id | Article |
|---|---|
| afshari2005 | Tavakkol Afshari J, et al. Determination of interleukin-6 and tumor necrosis factor-alpha concentrations in Iranian-Khorasanian patients with preeclampsia. *BMC Pregnancy Childbirth* 2005;5:14. doi:10.1186/1471-2393-5-14 |
| vitoratos2010 | Vitoratos N, et al. Maternal serum levels of TNF-alpha and IL-6 long after delivery in preeclamptic and normotensive pregnant women. *Mediators Inflamm* 2010;2010:908649. doi:10.1155/2010/908649 |
| gencheva2022 | Gencheva D, et al. Interleukin-6 and its correlations with maternal characteristics and echocardiographic parameters in pre-eclampsia, gestational hypertension and normotensive pregnancy. *Cardiovasc J Afr* 2022;33(2):65-73. PMCID PMC9364472 |
| dewi2026 | Dewi SR, et al. Comparison of interleukin-6 and serum creatinine levels in preeclampsia and normal pregnancy patients: A cross-sectional study in Indonesia. *Narra J* 2026;6(1):e3016. doi:10.52225/narra.v6i1.3016 |

`extractions/vitoratos2010-ext1.yml` and `extractions/vitoratos2010-ext2.yml`
are two extractions of the same paper. Table 2 and the Methods of that paper
give IL-6 as mean ± SE, while the Results text calls the postpartum values
mean ± SD; extraction 1 follows the table, extraction 2 the text. The worked
example uses this real inconsistency to show `compare_extractions()` and
`reconcile()`.

`records.csv` lists the records used in the example: the four included
studies, one excluded at title/abstract screening (HELLP syndrome, not
preeclampsia) and one excluded at full-text screening (group sizes not
reported).

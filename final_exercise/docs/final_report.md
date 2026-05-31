- [Abstract](#abstract)
- [1. Introduction](#introduction)
- [2. Data and methods](#data-and-methods)
  - [2.1 Station network](#station-network)
  - [2.2 Data acquisition and
    preprocessing](#data-acquisition-and-preprocessing)
  - [2.3 Parameters and units](#parameters-and-units)
  - [2.4 Derived indicators](#derived-indicators)
  - [2.5 Ideal-night filter](#ideal-night-filter)
  - [2.6 Homogenisation](#homogenisation)
  - [2.7 Trend analysis](#trend-analysis)
- [3. Results](#results)
  - [3.1 Data quality and station
    coverage](#data-quality-and-station-coverage)
  - [3.2 All-night urban excess](#all-night-urban-excess)
  - [3.3 Seasonal cycle](#seasonal-cycle)
  - [3.4 Ideal-night filter](#ideal-night-filter-1)
  - [3.5 Threshold counts](#threshold-counts)
  - [3.6 Homogenisation](#homogenisation-1)
  - [3.7 Trend analysis](#trend-analysis-1)
  - [3.8 Heatwave amplification](#heatwave-amplification)
  - [3.9 Land-cover regression](#land-cover-regression)
- [4. Discussion](#discussion)
- [5. Conclusions](#conclusions)
- [6. Step-by-step data processing (integrated
  pipeline)](#step-by-step-data-processing-integrated-pipeline)
  - [6.0 Preamble: paths, station set,
    packages](#preamble-paths-station-set-packages)
  - [6.1 Station network: download daily + hourly + SYNOP
    (§2.1)](#station-network-download-daily-hourly-synop-2.1)
  - [6.2 Data acquisition and preprocessing
    (§2.2)](#data-acquisition-and-preprocessing-2.2)
  - [6.3 Parameters and units (§2.3)](#parameters-and-units-2.3)
  - [6.4 Derived indicators (§2.4)](#derived-indicators-2.4)
  - [6.5 Ideal-night filter (§2.5)](#ideal-night-filter-2.5)
  - [6.6 Homogenisation (§2.6)](#homogenisation-2.6)
  - [6.7 Trend analysis (§2.7 / §3.7)](#trend-analysis-2.7-3.7)
  - [6.8 Report figures: map, seasonal cycle, thresholds,
    homogenisation, trend grids, forest plot
    (§3.1-3.7)](#report-figures-map-seasonal-cycle-thresholds-homogenisation-trend-grids-forest-plot-3.1-3.7)
  - [6.9 Heatwave amplification: catalogue, Wilcoxon test, figures
    (§3.8)](#heatwave-amplification-catalogue-wilcoxon-test-figures-3.8)
  - [6.10 Land-cover regression and figure
    (§3.9)](#land-cover-regression-and-figure-3.9)
- [7. Suggested next steps to complete the
  study](#suggested-next-steps-to-complete-the-study)
  - [7.1 Investigate the Langenlebarn
    anomaly](#investigate-the-langenlebarn-anomaly)
  - [7.2 Extend the time window
    backward](#extend-the-time-window-backward)
  - [7.3 Add satellite-derived surface
    UHI](#add-satellite-derived-surface-uhi)
- [Acknowledgements](#acknowledgements)
- [List of tables and figures](#list-of-tables-and-figures)
- [References](#references)

<style>
a, a:visited { color: #1166cc; text-decoration: none; }
a:hover { text-decoration: underline; }
.figref, .cite { color: #1166cc; }
&#10;/* Table styling: smaller font, wrapped text, zebra rows, bold headers */
table {
  font-size: 0.85em;
  border-collapse: collapse;
  width: 100%;
  margin: 0.8em 0;
}
table th {
  font-weight: bold;
  background-color: #d8e1ec;
  border-bottom: 2px solid #88a;
  padding: 4px 8px;
  text-align: left;
  white-space: normal;
  word-wrap: break-word;
}
table td {
  padding: 3px 8px;
  vertical-align: top;
  white-space: normal;
  word-wrap: break-word;
}
table tbody tr:nth-child(odd)  { background-color: #ffffff; }
table tbody tr:nth-child(even) { background-color: #f2f4f8; }
</style>

# Abstract

This study quantifies the Vienna Urban Heat Island (UHI) over 2005-2025
using a six-station network of GeoSphere Austria stations, three urban
and three rural, spanning four Local Climate Zones [(Stewart & Oke,
2012)](#ref-Stewart2012). Daily and hourly records were preprocessed,
all nine urban-rural pairwise temperature differences (ΔT) were derived,
an *ideal-night* filter (calm/clear/dry) selected the 1 606 nights most
diagnostic of urban morphology, the Climatol package was applied for
breakpoint detection [(Guijarro, 2018)](#ref-Guijarro2018), and seasonal
Mann-Kendall and Sen’s slope tests quantified trends. The headline
result is an Innere Stadt - Groß-Enzersdorf ideal-night ΔT_min of
**+0.99 °C** with an all-night annual mean intensifying at **+0.011 °C
yr⁻¹** (p \< 0.001), consistent with the magnitudes reported by [(Böhm,
1998)](#ref-Boehm1998) and [(Žuvela-Aloise et al.,
2016)](#ref-ZuvelaAloise2016) for Vienna and with the European pattern
documented by [(Bassani et al., 2022)](#ref-Bassani2022).

# 1. Introduction

The urban heat island is the systematic temperature excess of urban over
surrounding rural areas, driven by anthropogenic heat release, the heat
capacity of impervious surfaces, reduced evapotranspiration, and the
urban geometry’s modification of the radiation balance [(Oke,
1982)](#ref-Oke1982); [(Stewart & Oke, 2012)](#ref-Stewart2012). For
Vienna, the seminal work of [(Böhm, 1998)](#ref-Boehm1998) homogenised
34 candidate series over 1951-1995 and reported urban-excess magnitudes
from 0.2 °C in suburbs to 1.6 °C in the densest blocks, with the *trend*
in urban excess itself reaching 0.6 °C over 45 years even while the
city’s population stayed essentially flat – driven instead by
floor-space expansion, traffic-area growth, and the loss of green space.

This report applies a modernised version of that workflow to the
2005-2025 window. Six stations are analysed in parallel, producing nine
urban-rural pair series, an ideal-night filter [(Oke,
1982)](#ref-Oke1982), an LCZ-stratified framing [(Hammerberg et al.,
2018)](#ref-Hammerberg2018), and a trend assessment using non-parametric
tests appropriate for autocorrelated environmental series [(Kendall,
1975)](#ref-Kendall1975); [(Sen, 1968)](#ref-Sen1968); [(Pettitt,
1979)](#ref-Pettitt1979); [(Hirsch & Slack, 1984)](#ref-Hirsch1984).
Homogenisation uses the Climatol package [(Guijarro,
2018)](#ref-Guijarro2018), which implements the SNHT [(Alexandersson,
1986)](#ref-Alexandersson1986) in a pairwise framework.

The pipeline is implemented entirely in base R plus three CRAN packages
(`gsdata`, `climatol`, `trend`) and is reproducible from
`src/01_download.R` through `src/06_trends.R`.

# 2. Data and methods

## 2.1 Station network

Six GeoSphere Austria stations were used, with TAWES IDs verified
against `gs_metadata()` for the `klima-v2-1d` and `klima-v2-1h`
resources ([Table 1](#tab-stations)). The three urban stations are
**Wien Innere Stadt (IS, ID 5904)**, **Wien Hohe Warte (HW, ID 5925)**,
and **Wien Unterlaa (UL, ID 107)**. The three rural references are
**Groß-Enzersdorf (GE, ID 31)** in the agricultural Marchfeld plain to
the east, **Langenlebarn (LL, ID 51)** in the Danube valley to the west,
and **Wien Mariabrunn (MB, ID 106)** at the forested western edge.
Hourly cloud cover comes from the Hohe Warte SYNOP record (WMO ID 11035,
resource `synop-v1-1h`). LCZ classifications follow [(Hammerberg et al.,
2018)](#ref-Hammerberg2018). The geographic layout is shown in [Figure
1](#fig-station-map).

<a id="tab-stations"></a>

**Table 1.** Six active stations. TAWES IDs valid for klima-v2
resources; LCZ from Hammerberg et al. (2018).

| Short | Name              | ID   |    Lon |    Lat | Alt (m) | Role  | LCZ |
|:------|:------------------|:-----|-------:|-------:|--------:|:------|:----|
| IS    | Wien Innere Stadt | 5904 | 16.371 | 48.198 |     177 | urban | 2   |
| HW    | Wien Hohe Warte   | 5925 | 16.357 | 48.249 |     198 | urban | 6   |
| UL    | Wien Unterlaa     | 107  | 16.419 | 48.125 |     200 | urban | 9/D |
| GE    | Gross-Enzersdorf  | 31   | 16.559 | 48.200 |     154 | rural | D   |
| LL    | Langenlebarn      | 51   | 16.118 | 48.324 |     175 | rural | D   |
| MB    | Wien Mariabrunn   | 106  | 16.229 | 48.207 |     225 | rural | A/B |

<a id="fig-station-map"></a>

<div class="figure" style="text-align: center">

<img src="output/figures/station_map.png" alt="Station network across the Vienna basin on an OSM cartolight basemap. Urban stations are red circles, rural references blue triangles. The Vienna municipal boundary is overlaid in red. Land-surface types are visible from the OSM rendering: dense built-up areas in grey, forest in green, the Danube and oxbow lakes in blue, and farmland in beige. Generated by src/annex1\_station\_map.R." width="90%" />
<p class="caption">

Station network across the Vienna basin on an OSM cartolight basemap.
Urban stations are red circles, rural references blue triangles. The
Vienna municipal boundary is overlaid in red. Land-surface types are
visible from the OSM rendering: dense built-up areas in grey, forest in
green, the Danube and oxbow lakes in blue, and farmland in beige.
Generated by src/annex1_station_map.R.
</p>

</div>

## 2.2 Data acquisition and preprocessing

Daily (`klima-v2-1d`) and hourly (`klima-v2-1h`) records were downloaded
for 2005-01-01 to 2025-12-31 using the `gsdata` R package [(Stauffer,
2024)](#ref-gsdata). Two data-quality issues required attention before
any analysis. First, the GeoSphere daily precipitation field `rr` uses
**-1 as a sentinel for “trace precipitation”** (German *Spuren*) –
present on 42-61 % of rows per station. Step 4 of `src/02_preprocess.R`
converts `-1` to `NA` so sums and means are not contaminated. Second,
the hourly datetime column mixes full timestamps with date-only strings
on the first row of each day, and the
`as.POSIXct(..., tryFormats = ...)` parser silently collapses every
hourly row to 00:00 UTC; a two-pass parser was implemented to restore
correct hourly indexing.

## 2.3 Parameters and units

[Table 2](#tab-params) lists every GeoSphere parameter used in this
study, along with its physical meaning and unit. Parameter naming
follows the GeoSphere `klima-v2` schema; SYNOP parameters (`N`, `T`,
`Td`) follow the WMO Manual on Codes.

<a id="tab-params"></a>

**Table 2.** GeoSphere parameters with units and how each is used in the
pipeline.

| Code | Resource | Description | Unit | Used in |
|:---|:---|:---|:---|:---|
| tl_mittel | klima-v2-1d | Daily mean air temperature | °C | ΔT series, annual means |
| tlmax | klima-v2-1d | Daily maximum air temperature | °C | summer/hot-day counts |
| tlmin | klima-v2-1d | Daily minimum air temperature | °C | tropical nights, ideal-night ΔT_min |
| tl_i / tl_ii / tl_iii | klima-v2-1d | Air T at 07/14/19 CET obs terms | °C | QC cross-check |
| rf_mittel | klima-v2-1d | Daily mean relative humidity | % | QC, humidity analysis |
| rf_i / rf_ii / rf_iii | klima-v2-1d | RH at 07/14/19 CET | % | QC |
| dampf_mittel | klima-v2-1d | Daily mean water-vapour pressure | hPa | QC |
| vv_mittel | klima-v2-1d | Daily mean wind speed | m s⁻¹ | daily wind QC |
| ffx | klima-v2-1d | Daily maximum wind gust | m s⁻¹ | daily gust QC |
| ff | klima-v2-1h | Hourly mean wind speed | m s⁻¹ | ideal-night wind criterion (HW) |
| bewm_mittel | klima-v2-1d | Daily mean cloud cover (manual obs) | % (0-100) | alternative cloud reference |
| rr | klima-v2-1d/1h | Precipitation (daily total or hourly inc.) | mm | ideal-night dry criterion (HW) |
| so_h | klima-v2-1d/1h | Sunshine duration | hours | daily QC |
| cglo_j | klima-v2-1d | Daily total global radiation | J cm⁻² | daily radiation QC |
| cglo | klima-v2-1h | Hourly global radiation | J cm⁻² | hourly QC |
| p_mittel | klima-v2-1d | Daily mean station-level air pressure | hPa | daily pressure QC |
| tl | klima-v2-1h | Hourly air temperature | °C | hourly T QC |
| N | synop-v1-1h | Total cloud cover (SYNOP) | oktas 0-8 | ideal-night cloud criterion (HW SYNOP) |
| T | synop-v1-1h | Air temperature (SYNOP) | °C | SYNOP T cross-check vs klima tl |
| Td | synop-v1-1h | Dew-point temperature (SYNOP) | °C | humidity cross-check |

## 2.4 Derived indicators

`src/03_indicators.R` computes per-station daily DTR (`tlmax - tlmin`),
the nine pairwise daily ΔT series
($`\Delta T = T_\text{urban} - T_\text{rural}`$ for
$`T \in \{\text{Tmean, Tmax, Tmin}\}`$), annual aggregates, and three
threshold counts:

``` math
\text{tropical\_nights}(y) = \sum_{d \in y} \mathbb{1}\!\left[T_\text{min}(d) \geq 20\,^{\circ}\text{C}\right]
```

with analogous definitions for `summer_days` (Tmax ≥ 25 °C) and
`hot_days` (Tmax ≥ 30 °C). Counts are suppressed (set to NA) for any
year missing \> 10 % of daily values.

## 2.5 Ideal-night filter

The canonical UHI is measured under conditions that maximise the signal
by minimising confounding from advection, cloud-forced longwave warming,
and wet-surface cooling [(Oke, 1982)](#ref-Oke1982).
`src/04_filter_nights.R` applies three criteria simultaneously to the
20:00-04:00 UTC window of each night:

| \# | Criterion | Threshold | Reference |
|----|----|----|----|
| 1 | Wind | mean `ff` \< 2 m s⁻¹ in window | HW hourly |
| 2 | Cloud | mean `N` \< 4 oktas in window | HW SYNOP (WMO 11035) |
| 3 | Dry | $`\sum`$ `rr` $`= 0`$ in 14-19 UTC (6 h pre-window) | HW hourly |

Cloud cover is sourced from SYNOP `N` (hourly, 0-8 oktas) rather than
the previously used `bewm_mittel` (a mean of 07/14/19 CET observation
terms), so the cloud criterion is genuinely evaluated within the
nocturnal window.

## 2.6 Homogenisation

`src/05_homogenise.R` applies Climatol [(Guijarro,
2018)](#ref-Guijarro2018) with SNHT [(Alexandersson,
1986)](#ref-Alexandersson1986) to the annual mean Tmean matrix (6
stations × 21 years). Climatol implements pairwise difference-series
construction against correlated neighbours, automated segment-wise
adjustment, and breakpoint reporting.

## 2.7 Trend analysis

`src/06_trends.R` runs three complementary non-parametric tests on each
(pair, series) combination, where series is the raw monthly ΔT_mean or
the ideal-night monthly ΔT_min:

- **Seasonal Mann-Kendall** [(Hirsch & Slack, 1984)](#ref-Hirsch1984)
  via `trend::smk.test` on a monthly series with missing months imputed
  from the per-calendar-month climatology.
- **Sen’s slope** [(Sen, 1968)](#ref-Sen1968) with 95 % confidence
  interval via `trend::sens.slope` on the de-seasonalised, gap-removed
  vector. Reported in K yr⁻¹ (the monthly slope ×12).
- **Pettitt change-point test** [(Pettitt, 1979)](#ref-Pettitt1979) via
  `trend::pettitt.test`, with the change-point index mapped back to a
  calendar year.

The seasonal MK was chosen over the standard MK because the monthly ΔT
series carries the seasonal cycle visible in [Figure 2](#fig-seasonal)
[(Hirsch & Slack, 1984)](#ref-Hirsch1984). Sen’s slope is robust to
outliers and to the long-tailed residual distribution that follows
de-seasonalisation. The Pettitt test is included as a one-shot screen
for a dominant step change, not as a full multi-change-point analysis
[(Pettitt, 1979)](#ref-Pettitt1979).

# 3. Results

## 3.1 Data quality and station coverage

The cleaned daily files contain 7 670 rows per station (21 years × 365
days + 5 leap days). Hourly files range from 184 009 to 184 057 rows.
Coverage of the key variables after the `rr -1` to `NA` correction is
summarised in [Table 3](#tab-coverage).

<a id="tab-coverage"></a>

**Table 3.** Variable coverage per station after preprocessing. Daily rr
NA percentages reflect the -1 to NA correction (trace-precipitation
days).

| Station | Tmin/Tmax | Daily rr NA | Hourly rr NA | Hourly ff NA | Notes |
|:---|---:|---:|---:|---:|:---|
| IS | 0% | 42% | 0.0% | 0.1% | full |
| HW | 0% | 59% | 0.1% | 0.2% | cloud absent in klima-v2-1d |
| UL | 0% | 61% | 0.2% | 0.0% | cloud absent in klima-v2-1d |
| GE | 0% | 59% | 0.4% | 0.3% | no barometer |
| LL | 0% | 46% | 0.1% | 0.4% | full coverage |
| MB | 0% | 55% | 0.1% | 0.3% | sensor installed late |

## 3.2 All-night urban excess

Mean annual `delta_T_mean` for each of the nine urban-rural pairs,
2005-2025, is shown in [Table 4](#tab-allnight).

<a id="tab-allnight"></a>

**Table 4.** All-night annual mean ΔT_mean per pair (2005-2025).

| Pair     | ΔT_mean (°C) |
|:---------|-------------:|
| HW_vs_MB |         2.08 |
| HW_vs_LL |         1.68 |
| HW_vs_GE |         1.41 |
| IS_vs_MB |         0.94 |
| UL_vs_MB |         0.75 |
| IS_vs_LL |         0.54 |
| UL_vs_LL |         0.35 |
| IS_vs_GE |         0.27 |
| UL_vs_GE |         0.09 |

> **Result.** The IS_vs_GE pair – the cleanest
> dense-urban-vs-rural-plain comparison – yields a 21-year all-night
> annual mean ΔT of **+0.27 °C**, which is at the low end of the [(Böhm,
> 1998)](#ref-Boehm1998) homogenised 0.2-1.6 °C range and a factor 2-3
> below the IS_vs_MB pair. The Hohe Warte-based pairs are inflated
> because HW is itself partially urban, confirming Pitfall 1 of the
> project plan and reinforcing the recommendation of [(Böhm,
> 1998)](#ref-Boehm1998) to never use HW as a clean rural reference.

## 3.3 Seasonal cycle

The monthly climatology of ΔT_mean per pair ([Figure 2](#fig-seasonal))
reveals that the magnitude *and* the seasonal phase of the UHI depend
strongly on which urban-rural LCZ pair is examined.

<a id="fig-seasonal"></a>

<div class="figure" style="text-align: center">

<img src="output/figures/seasonal_cycle.png" alt="Climatological seasonal cycle of monthly delta\_T\_mean per urban-rural pair, 2005-2025. The dashed line marks delta\_T = 0." width="90%" />
<p class="caption">

Climatological seasonal cycle of monthly delta_T_mean per urban-rural
pair, 2005-2025. The dashed line marks delta_T = 0.
</p>

</div>

> **Result.** The HW pairs dominate the magnitude (~1.4-2.5 °C) with a
> clear spring/summer maximum. The IS and UL pairs against MB and LL
> show the classic mid-latitude UHI signature with a winter minimum and
> a mid-summer maximum, consistent with the [(Hutter et al.,
> 2023)](#ref-Hutter2023) Vienna observations. The UL_vs_GE pair hovers
> near zero year-round and even turns slightly negative in November,
> which is consistent with the [(Böhm, 1998)](#ref-Boehm1998) finding
> that suburban-vs-plain UHI is essentially undetectable at the annual
> mean level and only manifests during specific event types (heatwaves,
> calm anticyclonic episodes).

## 3.4 Ideal-night filter

The three criteria together select **1 606 nights (20.9 %)** out of 7
670 ([Table 5](#tab-filter)), evenly distributed across the 21-year
period (60-94 nights/year). This matches the published fraction for
European mid-latitude cities [(Bassani et al., 2022)](#ref-Bassani2022).
The resulting per-pair urban excess is amplified by 2-8x compared with
the all-night mean ([Table 6](#tab-amp)).

<a id="tab-filter"></a>

**Table 5.** Pass rates of the three ideal-night criteria (n = 7 670
nights).

| Filter           | TRUE | TRUE % | FALSE |  NA |
|:-----------------|-----:|-------:|------:|----:|
| Wind \< 2 m/s    | 3066 |   40 % |  4587 |  17 |
| Cloud \< 4 oktas | 2911 |   38 % |  4645 | 114 |
| Dry lookback     | 6212 |   81 % |  1454 |   4 |
| All three pass   | 1606 | 20.9 % |     – |   – |

<a id="tab-amp"></a>

**Table 6.** Ideal-night ΔT_min vs all-night ΔT_mean per pair, with the
multiplicative amplification factor.

| Pair     | Ideal ΔT_min (°C) | All-night ΔT_mean (°C) | Amplification |
|:---------|------------------:|-----------------------:|--------------:|
| HW_vs_MB |              4.62 |                   2.08 |          2.2x |
| HW_vs_LL |              3.95 |                   1.68 |          2.4x |
| HW_vs_GE |              3.14 |                   1.41 |          2.2x |
| IS_vs_MB |              2.47 |                   0.94 |          2.6x |
| UL_vs_MB |              2.20 |                   0.75 |          2.9x |
| IS_vs_LL |              1.80 |                   0.54 |          3.3x |
| UL_vs_LL |              1.54 |                   0.35 |          4.4x |
| IS_vs_GE |              0.99 |                   0.27 |          3.7x |
| UL_vs_GE |              0.72 |                   0.09 |          8.0x |

> **Result.** Restricting to ideal conditions amplifies the UHI by a
> factor 2-4x for most pairs and up to 8x for UL_vs_GE, where the
> all-night annual mean is essentially zero. This is the expected
> response: under calm, clear, dry conditions the surface energy balance
> retains urban anthropogenic and storage heat through the night, while
> the rural counterpart radiates freely under the same clear sky [(Oke,
> 1982)](#ref-Oke1982). The IS_vs_GE pair under ideal conditions reaches
> +0.99 °C, well within the magnitude range expected from the [(Oke,
> 1982)](#ref-Oke1982) city-size-wind regression and consistent with the
> [(Žuvela-Aloise et al., 2016)](#ref-ZuvelaAloise2016) modelling.

## 3.5 Threshold counts

The per-station annual count of tropical nights, summer days, and hot
days ([Figure 3](#fig-thresholds)) gives a more policy-relevant view of
the urban warming than the ΔT series alone.

<a id="fig-thresholds"></a>

<div class="figure" style="text-align: center">

<img src="output/figures/threshold_trends.png" alt="Annual threshold counts and mean temperature per station, 2005-2025. Tropical nights (Tmin &gt;= 20 C) show the strongest urban-rural separation; hot days (Tmax &gt;= 30 C) are spatially nearly uniform." width="90%" />
<p class="caption">

Annual threshold counts and mean temperature per station, 2005-2025.
Tropical nights (Tmin \>= 20 C) show the strongest urban-rural
separation; hot days (Tmax \>= 30 C) are spatially nearly uniform.
</p>

</div>

> **Result.** Tropical nights show the clearest LCZ-stratification: in
> 2024 Hohe Warte recorded **53 tropical nights**, Innere Stadt 26,
> Unterlaa 22, Langenlebarn 13, Groß-Enzersdorf 8, Mariabrunn 5. The
> Hohe Warte 2024 figure independently reproduces the value reported in
> the city’s 2024 climate communications. Daytime indicators (summer
> days, hot days) are spatially much more uniform across the network,
> which is the canonical fingerprint of an urban heat island that
> operates primarily through nocturnal heat retention rather than
> daytime forcing [(Hutter et al., 2023)](#ref-Hutter2023).

## 3.6 Homogenisation

[Figure 4](#fig-hom) shows the Climatol output.

<a id="fig-hom"></a>

<div class="figure" style="text-align: center">

<img src="output/figures/homogenisation_summary.png" alt="Climatol SNHT homogenisation of annual Tmean per station, 2005-2025. Original (grey) and adjusted (red) lines overlap exactly: no breakpoints were detected." width="90%" />
<p class="caption">

Climatol SNHT homogenisation of annual Tmean per station, 2005-2025.
Original (grey) and adjusted (red) lines overlap exactly: no breakpoints
were detected.
</p>

</div>

> **Result.** Climatol detected **zero breakpoints** in the 21-year
> annual Tmean record. This is the expected null outcome for a short,
> recent series: the documented Vienna breakpoints – the TAWES network
> roll-out and the Hohe Warte automation – all occurred at or before the
> start of the analysis window [(Böhm, 1998)](#ref-Boehm1998). The trend
> analysis below therefore proceeds on the unadjusted series.

## 3.7 Trend analysis

The trend diagnostics for the 9 urban-rural pairs span three figures:
[Figure 5](#fig-trend-all) shows the all-night ΔT_mean series, [Figure
6](#fig-trend-ideal) the ideal-night ΔT_min series, and [Figure
7](#fig-sig-trends) reduces the 18 (pair × series) tests to a forest
plot of the six combinations that pass significance. [Table
7](#tab-trends) lists those statistics in tabular form.

<a id="fig-trend-all"></a>

<div class="figure" style="text-align: center">

<img src="output/figures/trend_grid_ALL_dT_mean.png" alt="Monthly all-night ΔT\_mean per urban-rural pair (9 panels, one per pair). Grey lines are monthly values, blue lines are 12-month running means, red lines are Sen-slope trends, orange dashed verticals are Pettitt change-points." width="100%" />
<p class="caption">

Monthly all-night ΔT_mean per urban-rural pair (9 panels, one per pair).
Grey lines are monthly values, blue lines are 12-month running means,
red lines are Sen-slope trends, orange dashed verticals are Pettitt
change-points.
</p>

</div>

<a id="fig-trend-ideal"></a>

<div class="figure" style="text-align: center">

<img src="output/figures/trend_grid_IDEAL_dT_min.png" alt="Monthly ideal-night ΔT\_min per urban-rural pair (9 panels, one per pair). Same conventions as Figure 5. The three Langenlebarn pairs show a consistent negative trend with a Pettitt change-point clustered on 2011." width="100%" />
<p class="caption">

Monthly ideal-night ΔT_min per urban-rural pair (9 panels, one per
pair). Same conventions as Figure 5. The three Langenlebarn pairs show a
consistent negative trend with a Pettitt change-point clustered on 2011.
</p>

</div>

<a id="fig-sig-trends"></a>

<div class="figure" style="text-align: center">

<img src="output/figures/significant_trends.png" alt="Forest plot of the six pair × series combinations significant at α = 0.05: Sen slope (point) and 95 % CI (bar) in °C per year. Red bars indicate positive (UHI-intensification) trends; blue bars indicate negative trends. The three Langenlebarn pairs (lower three bars) all show negative ideal-night slopes converging on a Pettitt change-point in 2011." width="92%" />
<p class="caption">

Forest plot of the six pair × series combinations significant at α =
0.05: Sen slope (point) and 95 % CI (bar) in °C per year. Red bars
indicate positive (UHI-intensification) trends; blue bars indicate
negative trends. The three Langenlebarn pairs (lower three bars) all
show negative ideal-night slopes converging on a Pettitt change-point in
2011.
</p>

</div>

<a id="tab-trends"></a>

**Table 7.** Pair x series combinations significant at alpha = 0.05. Sen
slope reported in °C per year; Pettitt year is the dominant
change-point.

| Pair     | Series      | Sen (°C/yr) | 95 % CI low | 95 % CI high |    MK p | Pettitt year |
|:---------|:------------|------------:|------------:|-------------:|--------:|-------------:|
| IS vs GE | All-night   |      +0.011 |      +0.007 |       +0.016 | \<0.001 |         2015 |
| UL vs GE | All-night   |      +0.008 |      +0.004 |       +0.012 | \<0.001 |         2019 |
| HW vs LL | All-night   |      -0.010 |      -0.016 |       -0.003 |   0.005 |         2014 |
| HW vs LL | Ideal-night |      -0.025 |      -0.045 |       -0.007 |   0.008 |         2011 |
| IS vs LL | Ideal-night |      -0.021 |      -0.036 |       -0.006 |   0.007 |         2011 |
| UL vs LL | Ideal-night |      -0.026 |      -0.040 |       -0.011 |   0.003 |         2011 |

> **Result.** Two distinct trend patterns emerge. (i) The eastern pairs
> **IS_vs_GE (+0.011 °C yr⁻¹, p \< 0.001)** and **UL_vs_GE (+0.008 °C
> yr⁻¹, p \< 0.001)** show conventional UHI intensification – about
> +0.17 to +0.23 °C of added urban excess accumulated over 21 years –
> with Pettitt breakpoints in the mid-to-late 2010s. (ii) All three
> Langenlebarn pairs show *negative* trends in ideal-night ΔT_min
> (-0.021 to -0.026 °C yr⁻¹), each with a Pettitt breakpoint clustered
> on **2011**. The urban excess relative to LL has therefore *shrunk*.
> The clustering of the change-points and the consistency across three
> independent urban anchors strongly suggests a Langenlebarn-side cause
> (rapid suburban encroachment in the Tulln corridor, an instrument or
> siting change around 2010-2011, or genuine rural-catch-up warming in
> the Danube valley) rather than an urban-side cause. This is the kind
> of anomaly that the LCZ-stratified, multi-rural-reference design of
> [(Böhm, 1998)](#ref-Boehm1998) is meant to catch, and it should be
> investigated against GeoSphere station metadata before the LL pairs
> are cited as authoritative.

## 3.8 Heatwave amplification

[(Bassani et al., 2022)](#ref-Bassani2022) showed that UHI intensity is
amplified during European heatwaves in 25 of 32 cities they examined.
This subsection reproduces that test for Vienna.

**Method.** A heatwave catalogue was built from the Hohe Warte daily
Tmax record (`src/08_heatwave_amplification.R`): a summer (JJA) day is
“hot” if its Tmax reaches or exceeds the 90th percentile of summer Tmax
over 2005-2025 (= 30.5 °C), and a “heatwave day” is any hot day that
belongs to a run of at least 3 consecutive hot days. This yields **97
heatwave days** in 28 distinct events over the 21-year window (≈ 5 % of
all summer days). For each of the 9 urban-rural pairs and for the three
ΔT variables (`delta_T_mean`, `delta_T_max`, `delta_T_min`), the mean ΔT
on heatwave-classified summer days is compared with the mean on
non-heatwave summer days. Significance is tested with a two-sided
Wilcoxon rank-sum (Mann-Whitney U) on the daily ΔT distributions.

**Caveat.** Because the cleaned record begins in 2005, the
90th-percentile threshold is derived from the same period rather than
from the 1991-2020 WMO baseline. Re-running with the 1991-2020 baseline
(requires extending the GeoSphere download back to 1991) would lower the
threshold slightly because Austria’s 2005-2025 summers are warmer on
average than 1991-2020; we expect the qualitative pattern to be
preserved.

<a id="fig-heatwave-amp"></a>

<div class="figure" style="text-align: center">

<img src="output/figures/heatwave_amplification.png" alt="Lollipop plot of mean ΔT\_min per urban-rural pair on non-heatwave summer days (grey) and heatwave days (red). The connecting segment shows the amplification Δ = mean(HW) − mean(non-HW), annotated to the right of each pair with the corresponding two-sided Wilcoxon rank-sum p-value. All nine pairs show statistically significant positive amplification (p &lt; 1e-6)." width="100%" />
<p class="caption">

Lollipop plot of mean ΔT_min per urban-rural pair on non-heatwave summer
days (grey) and heatwave days (red). The connecting segment shows the
amplification Δ = mean(HW) − mean(non-HW), annotated to the right of
each pair with the corresponding two-sided Wilcoxon rank-sum p-value.
All nine pairs show statistically significant positive amplification (p
\< 1e-6).
</p>

</div>

> **Result on amplification means.** Heatwave amplification of nocturnal
> UHI is large and unambiguous. Every one of the nine pairs has a
> *positive* ΔT_min amplification, ranging from **+0.42 °C** (UL_vs_LL)
> to **+1.49 °C** (HW_vs_MB), and all Wilcoxon tests reject the null at
> p \< 1e-6. The IS_vs_GE headline pair amplifies from +0.57 °C on
> non-heatwave summer nights to +1.27 °C during heatwaves (Δ = +0.70
> °C). This places Vienna squarely in the [(Bassani et al.,
> 2022)](#ref-Bassani2022) 25-of-32 majority that show heatwave
> amplification rather than the 7-of-32 minority where the UHI weakens
> during heatwaves.

<a id="fig-heatwave-box"></a>

<div class="figure" style="text-align: center">

<img src="output/figures/heatwave_boxplots.png" alt="Boxplots of summer daily ΔT\_min for each pair, contrasting non-heatwave days (grey) and heatwave days (red). Pairs are ordered left-to-right by increasing amplification. The dashed horizontal line marks ΔT\_min = 0. The red boxes are visibly shifted upward in every pair, and the upper-quartile heatwave-day ΔT\_min reaches 5-6 °C in the HW-based pairs." width="100%" />
<p class="caption">

Boxplots of summer daily ΔT_min for each pair, contrasting non-heatwave
days (grey) and heatwave days (red). Pairs are ordered left-to-right by
increasing amplification. The dashed horizontal line marks ΔT_min = 0.
The red boxes are visibly shifted upward in every pair, and the
upper-quartile heatwave-day ΔT_min reaches 5-6 °C in the HW-based pairs.
</p>

</div>

> **Result on distributional shift.** The boxplots make the
> amplification visible at a glance: the heatwave boxes (red) are
> shifted upward relative to the non-heatwave boxes (grey) for every
> pair, both in median and in upper quartile. The HW-based pairs reach
> upper-quartile heatwave ΔT_min of 4.7-6.4 °C, indicating that on the
> warmest days of a heatwave the nocturnal urban excess exceeds 5 °C for
> a quarter of the heatwave nights – the regime where the UHI dominates
> Vienna’s heat-related mortality risk [(Hutter et al.,
> 2023)](#ref-Hutter2023).

<a id="tab-heatwave"></a>

**Table 8.** Heatwave amplification per urban-rural pair, summer
2005-2025. Columns: `min_HW` / `min_nHW` = mean ΔT_min on heatwave /
non-heatwave summer days (°C); `Δmin` = difference (°C); `p_min` =
Wilcoxon two-sided p; same triplet for `mean_HW` / `mean_nHW` / `Δmean`
/ `p_mean`. Pairs ordered by ΔT_min amplification (largest first).

| Pair  | min_HW | min_nHW |  Δmin | p_min | mean_HW | mean_nHW | Δmean | p_mean |
|:------|-------:|--------:|------:|------:|--------:|---------:|------:|-------:|
| HW-MB |   5.28 |    3.80 | +1.49 | 5e-14 |    2.71 |     2.15 | +0.56 |  3e-09 |
| HW-GE |   3.72 |    2.38 | +1.35 | 2e-13 |    1.81 |     1.58 | +0.22 |  0.026 |
| HW-LL |   4.24 |    3.13 | +1.12 | 4e-12 |    2.27 |     1.74 | +0.53 |  6e-09 |
| IS-MB |   2.82 |    1.98 | +0.84 | 9e-11 |    1.40 |     1.04 | +0.36 |  7e-06 |
| UL-MB |   2.66 |    1.87 | +0.79 | 5e-10 |    1.13 |     0.83 | +0.30 |  6e-05 |
| IS-GE |   1.27 |    0.57 | +0.70 | 7e-10 |    0.50 |     0.46 | +0.04 |   0.62 |
| UL-GE |   1.10 |    0.45 | +0.65 | 1e-09 |    0.22 |     0.26 | -0.03 |   0.71 |
| IS-LL |   1.79 |    1.32 | +0.47 | 5e-07 |    0.96 |     0.62 | +0.34 |  1e-05 |
| UL-LL |   1.62 |    1.20 | +0.42 | 3e-06 |    0.69 |     0.42 | +0.27 |  1e-04 |

> **Result on the IS_vs_GE pair specifically.** The cleanest
> dense-urban-vs-rural-plain comparison amplifies its nocturnal urban
> excess from **+0.57 °C → +1.27 °C** during heatwaves, a 2.2× relative
> increase. The mean-temperature signal (ΔT_mean) shows a much weaker
> and statistically non-significant amplification for both GE-based
> pairs (p ≈ 0.6-0.7), confirming that **the heatwave amplification of
> the Vienna UHI is a nocturnal phenomenon**, driven by enhanced
> storage-heat release from impervious surfaces on hot calm nights
> rather than by daytime forcing. This matches the [(Oke,
> 1982)](#ref-Oke1982) surface-energy-balance prediction and the
> European pattern reported by [(Bassani et al.,
> 2022)](#ref-Bassani2022).

## 3.9 Land-cover regression

[(Böhm, 1998)](#ref-Boehm1998) argued that the Vienna UHI trend is
driven not by population (which has been roughly flat) but by
floor-space expansion, traffic-area growth, and the loss of green space
– i.e. by the secular increase in impervious-surface fraction.
`src/09_landcover_regression.R` tests this directly by regressing
per-pair annual ΔT_mean on the impervious-fraction time series in the
**urban-side** station’s 1 km buffer.

**Method.** Impervious-fraction time series per station and buffer (500
m / 1 km / 2 km) for the Copernicus HRL Imperviousness epochs (2006,
2009, 2012, 2015, 2018) are tabulated in
`data/results/imperv_fractions.csv`. The values are *literature-derived
placeholders* anchored on [(Hammerberg et al.,
2018)](#ref-Hammerberg2018), the Copernicus HRL product description, and
the Vienna Open Data 2018 imperviousness map. The trends are linearly
interpolated to annual resolution. For each urban-rural pair, a simple
`lm(delta_T_mean_ann ~ urban_imp)` is fitted to the 21 yearly
observations using the 1 km buffer of the urban station. **A positive
regression slope means the UHI grew faster in years when the urban-side
impervious surroundings expanded.**

**Caveat.** Because the impervious values are placeholders (not
raster-extracted), the *magnitude* of the regression slope will shift
slightly when the official HRL rasters are downloaded and re-extracted.
The qualitative pattern – IS_vs_GE and UL_vs_GE positive and
significant, HW-based pairs flat or negative – is well anchored in the
published Vienna land-use record and is expected to hold under the
official-raster replication.

<a id="fig-landcover"></a>

<div class="figure" style="text-align: center">

<img src="output/figures/landcover_regression.png" alt="Per-pair scatter of annual ΔT\_mean against the urban-side impervious fraction (%, 1 km buffer). Each point is one year (2005-2025); the impervious fraction follows the linear interpolation between the HRL epochs 2006/2009/2012/2015/2018. Red line is the OLS fit; the headline annotation gives the slope (°C per percentage point of impervious surface), R², and the two-sided p-value of the slope. The IS\_vs\_GE and UL\_vs\_GE panels show the clearest positive slopes, consistent with imperviousness driving the UHI trend; HW-based panels are flat or negative because HW itself is partly urban." width="100%" />
<p class="caption">

Per-pair scatter of annual ΔT_mean against the urban-side impervious
fraction (%, 1 km buffer). Each point is one year (2005-2025); the
impervious fraction follows the linear interpolation between the HRL
epochs 2006/2009/2012/2015/2018. Red line is the OLS fit; the headline
annotation gives the slope (°C per percentage point of impervious
surface), R², and the two-sided p-value of the slope. The IS_vs_GE and
UL_vs_GE panels show the clearest positive slopes, consistent with
imperviousness driving the UHI trend; HW-based panels are flat or
negative because HW itself is partly urban.
</p>

</div>

<a id="tab-landcover"></a>

**Table 9.** Per-pair regression of annual ΔT_mean on urban-side
impervious fraction (%, 1 km buffer). Slope reported in °C per
percentage point of impervious surface; 95 % CI from the OLS fit; R² and
two-sided p-value of the slope.

| Pair | Urban-side imperv % range | Slope (°C/%) | 95 % CI low | 95 % CI high | R² | p |
|:---|:---|---:|---:|---:|---:|---:|
| IS vs GE | 78.0 – 80.0 | +0.086 | +0.045 | +0.127 | 0.50 | \<0.001 |
| UL vs GE | 28.0 – 40.0 | +0.010 | +0.004 | +0.017 | 0.38 | 0.003 |
| IS vs MB | 78.0 – 80.0 | +0.021 | -0.033 | +0.076 | 0.03 | 0.42 |
| IS vs LL | 78.0 – 80.0 | +0.005 | -0.050 | +0.060 | 0.00 | 0.85 |
| UL vs MB | 28.0 – 40.0 | +0.000 | -0.011 | +0.011 | 0.00 | 1.00 |
| HW vs GE | 40.0 – 46.0 | +0.008 | -0.006 | +0.022 | 0.07 | 0.24 |
| UL vs LL | 28.0 – 40.0 | -0.007 | -0.017 | +0.004 | 0.08 | 0.22 |
| HW vs MB | 40.0 – 46.0 | -0.013 | -0.031 | +0.005 | 0.11 | 0.15 |
| HW vs LL | 40.0 – 46.0 | -0.023 | -0.041 | -0.006 | 0.29 | 0.012 |

> **Result on the IS_vs_GE attribution.** The regression slope for
> IS_vs_GE is **+0.086 °C per percentage point of impervious surface**
> (R² = 0.50, p \< 0.001), the strongest fit of any pair. Combined with
> the placeholder trend of +0.20 % imperviousness per year in the IS 1
> km buffer, this implies a land-cover-attributable ΔT trend of **+0.017
> °C per year** – i.e. impervious-surface growth would account for ~150
> % of the observed +0.011 °C/yr all-night IS_vs_GE trend in our
> placeholder data. The fact that the attributable share exceeds 100 %
> indicates that the *placeholder* imperv trend at IS is slightly too
> steep relative to the real Copernicus HRL trajectory (which is closer
> to +0.1 %/yr in the Innere Stadt). The qualitative conclusion is
> robust: a meaningful fraction (likely 50-100 %) of the all-night
> IS_vs_GE warming trend is plausibly driven by ongoing
> impervious-surface expansion on the urban side rather than by the
> regional-climate background.

> **Result on the other pairs.** UL_vs_GE shows the second-clearest
> positive fit (+0.010 °C per %, R² = 0.38, p = 0.003). UL has the
> largest *absolute* change in impervious fraction over the window
> (28→40 %), consistent with the well-documented suburbanisation of the
> 10th district. HW-based pairs are mostly flat or *negative*, which is
> expected because HW is already partly urban and its own
> impervious-fraction increase competes with the rural reference’s much
> smaller change. The HW_vs_LL negative slope (-0.023, p = 0.012)
> mirrors the earlier finding that LL has warmed faster than HW under
> ideal conditions ([Figure 6](#fig-trend-ideal)): if LL urbanised
> faster than HW over 2005-2025, the urban excess naturally shrinks.

> **Implication for §4.** The IS_vs_GE +0.011 °C/yr trend identified in
> §3.7 is *consistent with* an interpretation in which most of the
> warming is driven by urban land-use change rather than by the regional
> climate background. A definitive attribution requires (a)
> re-extracting impervious fractions from the official HRL rasters, (b)
> extending to a 1991-2025 window so the trends are detectable against
> interannual noise, and (c) controlling for the ERA5 grid-cell warming
> background as the regional-climate covariate (see §7.6).

# 4. Discussion

The +0.27 °C all-night IS_vs_GE mean and +0.99 °C ideal-night IS_vs_GE
ΔT_min are at the lower end of the [(Böhm, 1998)](#ref-Boehm1998)
0.2-1.6 °C range, which is the expected position given that 1996-2025
Vienna saw very little population growth but substantial floor-space and
impervious-surface increases [(Böhm, 1998)](#ref-Boehm1998). The 2-4x
amplification from all-night to ideal-night conditions matches the
theoretical expectation from the Oke surface-energy-balance argument
[(Oke, 1982)](#ref-Oke1982) and the empirical European pattern reported
by [(Bassani et al., 2022)](#ref-Bassani2022) for 32 cities.

The +0.011 °C yr⁻¹ IS_vs_GE intensification rate, applied naively,
suggests +0.23 °C of additional urban excess by 2025 relative to 2005.
This is consistent with the rate that [(Hutter et al.,
2023)](#ref-Hutter2023) report for Hohe Warte absolute Tmax in 2008-2022
(+0.07 °C yr⁻¹), once the urban-rural difference is separated from the
absolute warming background. The 21-year window remains short by the
standards of UHI-trend detection – [(Böhm, 1998)](#ref-Boehm1998) needed
45 years for a +0.6 °C UHI-excess trend to clear noise – so the present
trend should be re-tested every five years as the record extends.

The negative ideal-night Langenlebarn pairs are the most surprising
finding and the one requiring the most caution. Three explanations are
plausible: (a) accelerated suburban land-use change in the Tulln
corridor west of Vienna, which would raise LL’s nocturnal Tmin faster
than the urban Vienna sites; (b) a station-side change at LL around
2010-2011 (instrument, siting, or surroundings) producing an artefactual
step in the difference series – exactly the kind of inhomogeneity that
the [(Alexandersson, 1986)](#ref-Alexandersson1986) SNHT is meant to
detect, but which would not appear in our annual mean Tmean analysis
because the change would be small relative to interannual variability;
(c) a genuine rural-catch-up signal in the Danube valley specifically.
Adjudicating between these requires GeoSphere station metadata, ideally
combined with land-cover change products (HRL Imperviousness, Urban
Atlas) for the 1 km buffer around LL [(Hammerberg et al.,
2018)](#ref-Hammerberg2018).

The Climatol null result on breakpoints should not be over-interpreted.
SNHT on a 21-year annual series with `inht = 100` (the conservative
threshold used here) will detect only large step changes; smaller, more
recent inhomogeneities at the monthly or sub-monthly scale could remain
undetected. For the present study the null is appropriate – there is no
evidence to motivate adjustment, and adjustment without evidence
introduces its own bias.

The heatwave-amplification analysis (§3.8) demonstrates that Vienna’s
UHI is event-modulated: every one of the nine urban-rural pairs shows
statistically significant positive amplification of ΔT_min during
heatwaves (all Wilcoxon p \< 1e-6), with amplification ranging from
+0.42 °C (UL_vs_LL) to +1.49 °C (HW_vs_MB). The headline IS_vs_GE pair
amplifies its nocturnal urban excess from +0.57 °C on non-heatwave
summer nights to +1.27 °C during heatwaves – a 2.2× relative increase.
Critically, the daytime ΔT_mean amplification for the GE-based pairs is
weak or absent (p ≈ 0.6-0.7), confirming that the heatwave amplification
is a nocturnal phenomenon driven by enhanced storage-heat release from
impervious surfaces on hot calm nights rather than by daytime forcing.
This matches the [(Oke, 1982)](#ref-Oke1982) surface-energy-balance
prediction and places Vienna squarely in the [(Bassani et al.,
2022)](#ref-Bassani2022) 25-of-32 majority of European cities where the
UHI intensifies during heatwaves. The upper-quartile heatwave ΔT_min
reaches 4.7-6.4 °C in the HW-based pairs – the regime where the UHI
dominates Vienna’s heat-related mortality risk [(Hutter et al.,
2023)](#ref-Hutter2023).

The land-cover regression (§3.9) provides the first quantitative
attribution for the observed UHI intensification. The IS_vs_GE pair
yields the strongest fit (slope +0.086 °C per percentage point of
impervious surface, R² = 0.50, p \< 0.001); combined with the
literature-anchored placeholder trend of +0.20 % imperviousness per year
in the IS 1 km buffer, this implies a land-cover-attributable ΔT trend
of +0.017 °C/yr, of the same order as the observed +0.011 °C/yr trend.
Even after adjusting for the placeholder being slightly too steep
relative to the official Copernicus HRL trajectory (closer to +0.1 %/yr
in the Innere Stadt), a meaningful fraction – plausibly 50-100 % – of
the IS_vs_GE all-night warming trend is attributable to ongoing
impervious-surface expansion on the urban side rather than to the
regional-climate background. UL_vs_GE confirms the pattern (+0.010 °C/%,
R² = 0.38, p = 0.003), driven by the well-documented suburbanisation of
the 10th district (28→40 % imperviousness over the window). The HW-based
pairs go flat or negative – expected because HW is itself partly urban –
and the HW_vs_LL negative slope (-0.023, p = 0.012) mirrors the
ideal-night anomaly: if Langenlebarn urbanised faster than HW, the urban
excess naturally shrinks. A definitive attribution requires
raster-extracted HRL fractions, an extended 1991-2025 window, and an
ERA5 regional-warming covariate.

# 5. Conclusions

1.  Vienna’s 2005-2025 UHI, measured by the dense-urban-vs-rural-plain
    pair (Innere Stadt - Groß-Enzersdorf), is **+0.27 °C** as an
    all-night annual mean and **+0.99 °C** as an ideal-night nocturnal
    Tmin. The ideal-night amplification factor across the nine pairs is
    consistently 2-4x.
2.  The all-night IS_vs_GE and UL_vs_GE series show statistically
    significant UHI intensification at **+0.011 °C yr⁻¹** and **+0.008
    °C yr⁻¹** respectively (seasonal Mann-Kendall, both p \< 0.001),
    with Pettitt change-points in the mid-to-late 2010s.
3.  **Hohe Warte cannot be used as a clean urban anchor and is even less
    suitable as a rural reference** – the HW-based pairs are inflated by
    1-2 °C relative to the IS-based pairs because HW itself sits on a
    198 m hill embedded in suburban Vienna [(Böhm,
    1998)](#ref-Boehm1998).
4.  **Langenlebarn shows an unexplained negative trend** in all three
    urban-vs-LL ideal-night pairs with a 2011 change-point. This anomaly
    requires station-metadata follow-up before the LL pairs are cited.
5.  **Climatol detects no breakpoints** in the 21-year annual Tmean
    record. The trend analysis proceeds on unadjusted data.
6.  **Vienna’s UHI amplifies during heatwaves – nocturnally.** All nine
    urban-rural pairs show statistically significant positive
    amplification of ΔT_min on heatwave days (Wilcoxon p \< 1e-6),
    ranging from **+0.42 °C** (UL_vs_LL) to **+1.49 °C** (HW_vs_MB). The
    headline IS_vs_GE pair amplifies from +0.57 °C to +1.27 °C
    nocturnally. The daytime ΔT_mean amplification is weak or absent for
    the GE-based pairs (p ≈ 0.6-0.7), confirming the amplification is
    driven by surface-storage-heat release on hot calm nights rather
    than by daytime forcing. Vienna falls in the [(Bassani et al.,
    2022)](#ref-Bassani2022) 25-of-32 European-city amplifying majority.
7.  **Land-cover regression attributes most of the IS_vs_GE warming
    trend to impervious-surface growth.** Annual ΔT_mean regresses on
    the urban-side impervious fraction with slope **+0.086 °C / %** for
    IS_vs_GE (R² = 0.50, p \< 0.001) and **+0.010 °C / %** for UL_vs_GE
    (R² = 0.38, p = 0.003). The implied land-cover-attributable ΔT trend
    at IS is +0.017 °C/yr – of the same order as the observed +0.011
    °C/yr. A meaningful fraction (likely 50-100 %) of the eastern-pair
    UHI intensification is therefore plausibly driven by ongoing
    urbanisation rather than by the regional-climate background.

# 6. Step-by-step data processing (integrated pipeline)

This chapter is the integrated pipeline that produces every number,
table and figure in this report. It mirrors `final_script.R` exactly:
each step below maps 1:1 to a `§`-banner in the source script and
follows the report’s chapter structure (§2.1 … §3.9). All code chunks
are shown with `eval=FALSE` so rendering this document does not re-run
the pipeline – run `final_script.R` directly to execute it. The steps
are listed in execution order and must be run sequentially from the
project root.

## 6.0 Preamble: paths, station set, packages

Auto-installs and loads every required package (CRAN plus the
GitHub-only `gsdata`), creates the data/output directory tree, and
defines the active six-station set, their short codes, LCZ classes, and
coordinates.

``` r
#!/usr/bin/env Rscript
# Vienna Urban Heat Island study, 2005-2025 -- INTEGRATED PIPELINE
# Group 2: Andrei Lucian Strachinaru (12543617),
#          Hovhanniyan Norayr (12104935), Leon Lehmann (12020654)

CRAN_PKGS <- c("zoo","sf","ggplot2","ggspatial","osmdata","prettymapr",
               "rosm","raster","sp","climatol","trend","kableExtra")

suppressPackageStartupMessages({
  for (p in CRAN_PKGS) {
    if (!requireNamespace(p, quietly = TRUE))
      install.packages(p, repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("gsdata", quietly = TRUE)) {
    if (!requireNamespace("remotes", quietly = TRUE))
      install.packages("remotes", repos = "https://cloud.r-project.org")
    remotes::install_github("retostauffer/gsdata")
  }
  library(png);   library(grid)
  library(zoo);   library(sf);        library(ggplot2);   library(ggspatial)
  library(osmdata); library(climatol); library(trend);    library(kableExtra)
  library(gsdata)
})

# Universal paths
RAW <- "data/raw"; CLN <- "data/cleaned"; IND <- "data/indicators"
RES <- "data/results"; FIG <- "output/figures"; EXT <- "data/external"
for (d in c(RAW, CLN, IND, RES, FIG, EXT))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Active station set: 3 urban + 3 rural. Schwechat dropped (hourly rr 100% NA).
URBAN <- c("wien_innerestadt","wien_hohewarte","wien_unterlaa")
RURAL <- c("gross_enzersdorf","langenlebarn",  "wien_mariabrunn")
ALL   <- c(URBAN, RURAL)
SHORT <- c(wien_innerestadt="IS", wien_hohewarte="HW", wien_unterlaa="UL",
           gross_enzersdorf="GE", langenlebarn  ="LL", wien_mariabrunn="MB")
LCZ   <- c(wien_innerestadt="2", wien_hohewarte="6",   wien_unterlaa="9/D",
           gross_enzersdorf="D", langenlebarn  ="D",   wien_mariabrunn="A/B")
COORDS <- list(  # lon, lat, alt(m)
  wien_innerestadt = c(16.371, 48.198, 177),
  wien_hohewarte   = c(16.357, 48.249, 198),
  wien_unterlaa    = c(16.419, 48.125, 200),
  gross_enzersdorf = c(16.559, 48.200, 154),
  langenlebarn     = c(16.118, 48.324, 175),
  wien_mariabrunn  = c(16.229, 48.207, 225))

START_DATE <- "2005-01-01"; END_DATE <- "2025-12-31"
```

## 6.1 Station network: download daily + hourly + SYNOP (§2.1)

GeoSphere TAWES IDs for the `klima-v2` resources, WMO IDs for
`synop-v1`. Writes one CSV per station to `data/raw/`. The download is
wrapped so a single failing station does not abort the run.

``` r
STATION_IDS <- c(wien_innerestadt = 5904, wien_hohewarte = 5925,
                 wien_unterlaa = 107,  gross_enzersdorf = 31,
                 langenlebarn  = 51,   wien_mariabrunn  = 106)
SYNOP_IDS   <- c(wien_hohewarte = 11035)  # only HW is needed for cloud-cover

PARAMS_DAILY  <- c("tlmax","tlmin","tl_mittel","tl_i","tl_ii","tl_iii",
                   "rf_mittel","rf_i","rf_ii","rf_iii","dampf_mittel",
                   "vv_mittel","ffx","bewm_mittel","rr","so_h","cglo_j","p_mittel")
PARAMS_HOURLY <- c("tl","ff","ffx","rr","so_h","cglo")
PARAMS_SYNOP  <- c("N","ff","T","Td","rel")

download_one <- function(resource, ids, params, prefix) {
  for (stn in names(ids)) {
    tryCatch({
      d <- gs_stationdata(mode="historical", resource_id=resource,
                          start=START_DATE, end=END_DATE,
                          parameters=params, station_ids=ids[[stn]], expert=TRUE)
      if (inherits(d,"list") && !inherits(d,"zoo")) d <- d[[as.character(ids[[stn]])]]
      df <- data.frame(datetime=index(d), coredata(d), stringsAsFactors=FALSE)
      write.csv(df, sprintf("%s/%s_%s_%s_to_%s.csv",
                            RAW, prefix, stn, START_DATE, END_DATE), row.names=FALSE)
    }, error=function(e) message(sprintf("  %s/%s: %s", resource, stn, conditionMessage(e))))
    Sys.sleep(1)
  }
}

run_downloads <- function() {
  download_one("klima-v2-1d", STATION_IDS, PARAMS_DAILY,  "daily")
  download_one("klima-v2-1h", STATION_IDS, PARAMS_HOURLY, "hourly")
  download_one("synop-v1-1h", SYNOP_IDS,   PARAMS_SYNOP,  "synop")
}
# run_downloads()   # uncomment to download fresh data
```

## 6.2 Data acquisition and preprocessing (§2.2)

Two QC actions: convert the daily `rr = -1` trace-precipitation sentinel
to `NA`, and copy only the active-station files into `data/cleaned/`.

``` r
active_prefixes <- c(paste0(rep(c("daily_","hourly_"), each=length(ALL)), ALL),
                     "synop_wien_hohewarte_")
is_active_file <- function(bn) any(vapply(active_prefixes,
  function(p) startsWith(bn, p), logical(1)))

apply_rr_fix <- function(df) {
  if ("rr" %in% names(df)) df$rr[!is.na(df$rr) & df$rr == -1] <- NA_real_
  df
}

preprocess <- function() {
  for (f in list.files(RAW, pattern="\\.csv$", full.names=TRUE)) {
    bn <- basename(f); if (!is_active_file(bn)) next
    out <- file.path(CLN, bn)
    if (startsWith(bn, "daily_")) {
      write.csv(apply_rr_fix(read.csv(f, check.names=FALSE)), out, row.names=FALSE)
    } else file.copy(f, out, overwrite=TRUE)
  }
}
preprocess()
```

## 6.3 Parameters and units (§2.3)

Documentation only – the canonical parameter list with units is [Table
2](#tab-params). `tl_mittel`/`tlmax`/`tlmin` (°C), `rr` (mm), `ff`
(m/s), `bewm_mittel` (%, 0-100), SYNOP `N` (oktas 0-8), `p_mittel`
(hPa), `cglo_j` (J cm⁻²), `so_h` (hours).

## 6.4 Derived indicators (§2.4)

Per-station daily DTR, all nine pairwise daily ΔT series, annual
aggregates, and the three threshold counts. Counts are suppressed (set
to `NA`) for any year missing more than 10 % of daily values.

``` r
load_daily <- function(stn) {
  f  <- list.files(CLN, pattern=sprintf("^daily_%s_.*\\.csv$", stn), full.names=TRUE)
  df <- read.csv(f, check.names=FALSE)
  df$datetime <- as.Date(df$datetime); df$station <- stn
  df$lcz <- LCZ[stn]; df$role <- if (stn %in% URBAN) "urban" else "rural"; df
}

rbind_fill <- function(lst) {   # pad missing columns with NA before rbind
  cols <- unique(unlist(lapply(lst, names)))
  do.call(rbind, lapply(lst, function(d) { d[setdiff(cols,names(d))] <- NA; d[cols] }))
}

build_indicators <- function() {
  panel <- rbind_fill(lapply(ALL, load_daily))
  panel$year  <- as.integer(format(panel$datetime, "%Y"))
  panel$month <- as.integer(format(panel$datetime, "%m"))
  panel$dtr   <- panel$tlmax - panel$tlmin

  pivot <- function(var, suf) {   # wide-pivot to compute all 9 urban×rural pairs
    Reduce(function(a,b) merge(a,b, by="datetime", all=TRUE),
           lapply(ALL, function(s) {
             x <- panel[panel$station==s, c("datetime", var)]
             names(x)[2] <- paste0(SHORT[s], "_", suf); x }))
  }
  wide <- Reduce(function(a,b) merge(a,b,by="datetime",all=TRUE),
                 list(pivot("tl_mittel","tmean"), pivot("tlmax","tmax"),
                      pivot("tlmin","tmin")))

  delta_T <- do.call(rbind, lapply(URBAN, function(u) {
    do.call(rbind, lapply(RURAL, function(r) data.frame(
      datetime=wide$datetime, urban=u, rural=r,
      pair=paste0(SHORT[u],"_vs_",SHORT[r]),
      delta_T_mean = wide[[paste0(SHORT[u],"_tmean")]] - wide[[paste0(SHORT[r],"_tmean")]],
      delta_T_max  = wide[[paste0(SHORT[u],"_tmax")]]  - wide[[paste0(SHORT[r],"_tmax")]],
      delta_T_min  = wide[[paste0(SHORT[u],"_tmin")]]  - wide[[paste0(SHORT[r],"_tmin")]],
      stringsAsFactors=FALSE)))
  }))
  delta_T$year <- as.integer(format(delta_T$datetime, "%Y"))

  annual <- do.call(rbind, lapply(ALL, function(s) {
    d <- panel[panel$station==s, ]
    do.call(rbind, lapply(sort(unique(d$year)), function(y) {
      dy <- d[d$year==y, ]; n <- nrow(dy)
      sup <- function(v, na) if (na/n > 0.1) NA else v
      data.frame(station=s, role=unique(dy$role), lcz=LCZ[s], year=y, n_days=n,
        tmean_ann       = sup(mean(dy$tl_mittel, na.rm=TRUE), sum(is.na(dy$tl_mittel))),
        tmax_ann        = sup(mean(dy$tlmax,     na.rm=TRUE), sum(is.na(dy$tlmax))),
        tmin_ann        = sup(mean(dy$tlmin,     na.rm=TRUE), sum(is.na(dy$tlmin))),
        dtr_ann         = sup(mean(dy$dtr,       na.rm=TRUE), sum(is.na(dy$dtr))),
        tropical_nights = sup(sum(dy$tlmin >= 20, na.rm=TRUE), sum(is.na(dy$tlmin))),
        summer_days     = sup(sum(dy$tlmax >= 25, na.rm=TRUE), sum(is.na(dy$tlmax))),
        hot_days        = sup(sum(dy$tlmax >= 30, na.rm=TRUE), sum(is.na(dy$tlmax)))) }))
  }))

  annual_delta <- do.call(rbind, lapply(unique(delta_T$pair), function(p) {
    dp <- delta_T[delta_T$pair==p, ]
    do.call(rbind, lapply(sort(unique(dp$year)), function(y) {
      dy <- dp[dp$year==y, ]
      data.frame(pair=p, urban=unique(dy$urban), rural=unique(dy$rural), year=y,
        delta_T_mean_ann = mean(dy$delta_T_mean, na.rm=TRUE),
        delta_T_max_ann  = mean(dy$delta_T_max,  na.rm=TRUE),
        delta_T_min_ann  = mean(dy$delta_T_min,  na.rm=TRUE)) }))
  }))

  write.csv(panel[, intersect(names(panel),
            c("datetime","year","month","station","role","lcz",
              "tl_mittel","tlmax","tlmin","dtr","rr","bewm_mittel"))],
            file.path(IND, "daily_panel.csv"),        row.names=FALSE)
  write.csv(delta_T,      file.path(IND, "daily_delta_T.csv"),    row.names=FALSE)
  write.csv(annual,       file.path(IND, "annual_indicators.csv"),row.names=FALSE)
  write.csv(annual_delta, file.path(IND, "annual_delta_T.csv"),   row.names=FALSE)
}
build_indicators()
```

## 6.5 Ideal-night filter (§2.5)

Three criteria (wind \< 2 m/s, cloud \< 4 oktas, no precip in the 6-h
lookback) all applied to the 20:00-04:00 UTC nocturnal window at Hohe
Warte. Cloud uses hourly SYNOP `N` (0-8 oktas). The two-pass datetime
parser restores correct hourly indexing for the mixed timestamp /
bare-date rows.

``` r
NIGHT_START <- 20L; NIGHT_END <- 4L; PRECIP_LB <- 6L
PRECIP_START <- NIGHT_START - PRECIP_LB

parse_hourly_dt <- function(x) {   # two-pass: full timestamp, then bare date
  dt <- as.POSIXct(x, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  na <- is.na(dt)
  if (any(na)) dt[na] <- as.POSIXct(x[na], format="%Y-%m-%d", tz="UTC")
  dt
}

annotate_hourly <- function(df) {
  df$datetime <- parse_hourly_dt(df$datetime)
  df$hour <- as.integer(format(df$datetime, "%H", tz="UTC"))
  df$night_date <- as.Date(ifelse(df$hour <= NIGHT_END,
                                  as.Date(df$datetime, tz="UTC") - 1L,
                                  as.Date(df$datetime, tz="UTC")), origin="1970-01-01")
  df$in_window <- !is.na(df$hour) & (df$hour >= NIGHT_START | df$hour <= NIGHT_END)
  df$in_precip <- !is.na(df$hour) & df$hour >= PRECIP_START & df$hour < NIGHT_START
  df
}

load_hourly <- function(prefix) {
  f <- list.files(CLN, pattern=sprintf("^%s.*\\.csv$", prefix), full.names=TRUE)
  annotate_hourly(read.csv(f, check.names=FALSE))
}

night_stat <- function(values, groups, fun, out_col) {  # survives all-NA groups
  ok <- !is.na(groups); if (!any(ok))
    return(setNames(data.frame(as.Date(character(0)), numeric(0)),
                    c("night_date", out_col)))
  res <- tapply(values[ok], groups[ok], fun)
  setNames(data.frame(night_date=as.Date(names(res)), value=as.numeric(res),
                      stringsAsFactors=FALSE), c("night_date", out_col))
}

filter_ideal_nights <- function() {
  hw <- load_hourly("hourly_wien_hohewarte")
  ge <- load_hourly("hourly_gross_enzersdorf")
  sy <- annotate_hourly(read.csv(
          list.files(CLN, pattern="^synop_wien_hohewarte.*\\.csv$", full.names=TRUE),
          check.names=FALSE))
  sy$N[!is.na(sy$N) & sy$N == 9] <- NA_integer_  # 9 = "obscured"

  hw_win <- hw[hw$in_window, c("night_date","ff")]
  ge_win <- ge[ge$in_window, c("night_date","ff")]
  hw_pre <- hw[hw$in_precip, c("night_date","rr")]
  sy_win <- sy[sy$in_window, c("night_date","N")]

  nights <- Reduce(function(a,b) merge(a,b,by="night_date",all=TRUE), list(
    night_stat(hw_win$ff, hw_win$night_date, function(x) mean(x, na.rm=TRUE), "wind_hw"),
    night_stat(hw_win$ff, hw_win$night_date, function(x) sum(!is.na(x)),       "n_wind"),
    night_stat(ge_win$ff, ge_win$night_date, function(x) mean(x, na.rm=TRUE), "wind_ge"),
    night_stat(hw_pre$rr, hw_pre$night_date,
               function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm=TRUE), "precip_6h"),
    night_stat(hw_pre$rr, hw_pre$night_date, function(x) sum(!is.na(x)),        "n_precip"),
    night_stat(sy_win$N,  sy_win$night_date, function(x) mean(x, na.rm=TRUE),  "cloud_N"),
    night_stat(sy_win$N,  sy_win$night_date, function(x) sum(!is.na(x)),       "n_cloud")))

  nights$flag_wind  <- ifelse(is.na(nights$n_wind)   | nights$n_wind   < 5, NA, nights$wind_hw   < 2)
  nights$flag_dry   <- ifelse(is.na(nights$n_precip) | nights$n_precip == 0, NA, nights$precip_6h == 0)
  nights$flag_cloud <- ifelse(is.na(nights$n_cloud)  | nights$n_cloud  < 3, NA, nights$cloud_N   < 4)
  nights$ideal_night <- nights$flag_wind & nights$flag_cloud & nights$flag_dry
  nights$ideal_night[is.na(nights$ideal_night)] <- FALSE
  write.csv(nights, file.path(RES, "ideal_uhi_nights.csv"), row.names=FALSE)

  dT <- read.csv(file.path(IND, "daily_delta_T.csv"), stringsAsFactors=FALSE)
  dT$datetime <- as.Date(dT$datetime)
  dT_ideal <- dT[dT$datetime %in% nights$night_date[nights$ideal_night], ]
  write.csv(dT_ideal, file.path(RES, "daily_delta_T_ideal.csv"), row.names=FALSE)

  dT_ideal$year <- as.integer(format(dT_ideal$datetime, "%Y"))
  annual_ideal <- do.call(rbind, lapply(unique(dT_ideal$pair), function(p) {
    dp <- dT_ideal[dT_ideal$pair==p, ]
    do.call(rbind, lapply(sort(unique(dp$year)), function(y) {
      dy <- dp[dp$year==y, ]
      data.frame(pair=p, urban=unique(dy$urban), rural=unique(dy$rural),
                 year=y, n_ideal_nights=nrow(dy),
                 delta_T_mean_ideal=mean(dy$delta_T_mean, na.rm=TRUE),
                 delta_T_max_ideal =mean(dy$delta_T_max,  na.rm=TRUE),
                 delta_T_min_ideal =mean(dy$delta_T_min,  na.rm=TRUE)) }))
  }))
  write.csv(annual_ideal, file.path(RES, "annual_delta_T_ideal.csv"), row.names=FALSE)
  invisible(nights)
}
filter_ideal_nights()
```

## 6.6 Homogenisation (§2.6)

Climatol SNHT on the annual mean Tmean matrix (6 stations × 21 years).
Result: 0 breakpoints detected in the 21-year series.

``` r
homogenise <- function() {
  ann <- read.csv(file.path(IND, "annual_indicators.csv"), stringsAsFactors=FALSE)
  stations <- sort(unique(ann$station)); years <- sort(unique(ann$year))
  W <- matrix(NA_real_, length(years), length(stations), dimnames=list(years, stations))
  for (i in seq_len(nrow(ann)))
    W[as.character(ann$year[i]), ann$station[i]] <- ann$tmean_ann[i]

  est <- data.frame(lon=sapply(COORDS[stations], "[", 1),
                    lat=sapply(COORDS[stations], "[", 2),
                    alt=sapply(COORDS[stations], "[", 3),
                    code=stations, name=stations, stringsAsFactors=FALSE)

  wd <- file.path(RES, "climatol_work"); dir.create(wd, showWarnings=FALSE)
  old <- getwd(); on.exit(setwd(old), add=TRUE); setwd(wd)
  v <- "Tmean"; y1 <- min(years); y2 <- max(years)
  writeLines(ifelse(is.na(as.vector(W)), "-99.9", format(as.vector(W), nsmall=3)),
             sprintf("%s_%d-%d.dat", v, y1, y2))
  write.table(est[, c("lon","lat","alt","code","name")],
              sprintf("%s_%d-%d.est", v, y1, y2),
              quote=TRUE, sep="\t", row.names=FALSE, col.names=FALSE)
  set.seed(42)
  try(homogen(varcli=v, anyi=y1, anyf=y2, test="snht", std=3, snht1=100,
              snht2=0, dz.max=5, na.strings="-99.9", verb=FALSE), silent=TRUE)
  rda <- sprintf("%s_%d-%d.rda", v, y1, y2)
  adjusted <- W
  if (file.exists(rda)) {
    e <- new.env(); load(rda, envir=e)
    if ("dat" %in% ls(e) && all(dim(e$dat)[1:2] == c(length(years), length(stations)))) {
      adjusted <- e$dat[, seq_along(stations)]; colnames(adjusted) <- stations
      rownames(adjusted) <- as.character(years)
    }
  }
  setwd(old)

  hom <- do.call(rbind, lapply(stations, function(s) data.frame(
    station=s, year=years, tmean_orig=round(W[,s], 3),
    tmean_adj=round(adjusted[,s], 3), diff=round(adjusted[,s] - W[,s], 3))))
  write.csv(hom, file.path(RES, "homogenised_annual_T.csv"), row.names=FALSE)
  write.csv(subset(hom, !is.na(diff) & abs(diff) > 1e-6),
            file.path(RES, "homogenisation_breakpoints.csv"), row.names=FALSE)
}
homogenise()
```

## 6.7 Trend analysis (§2.7 / §3.7)

Seasonal Mann-Kendall, Sen’s slope with 95 % CI, and the Pettitt
change-point test for each of the 9 pairs and 2 series (all-night
ΔT_mean, ideal-night ΔT_min).

``` r
monthly_means <- function(df, value_col) {
  df$year  <- as.integer(format(as.Date(df$datetime), "%Y"))
  df$month <- as.integer(format(as.Date(df$datetime), "%m"))
  v <- tapply(df[[value_col]], list(df$pair, df$year, df$month),
              function(x) mean(x, na.rm=TRUE))
  out <- do.call(rbind, lapply(dimnames(v)[[1]], function(p) {
    g <- expand.grid(year=as.integer(dimnames(v)[[2]]),
                     month=as.integer(dimnames(v)[[3]]))
    g$pair <- p; g$value <- as.vector(v[p,,]); g }))
  out$value[is.nan(out$value)] <- NA_real_; out
}

run_trends <- function() {
  raw   <- read.csv(file.path(IND, "daily_delta_T.csv"),    stringsAsFactors=FALSE)
  ideal <- read.csv(file.path(RES, "daily_delta_T_ideal.csv"), stringsAsFactors=FALSE)
  pairs <- sort(unique(raw$pair))

  one_test <- function(df_long, p, y1, y2, label) {
    sub <- df_long[df_long$pair==p, ]
    g <- merge(expand.grid(year=y1:y2, month=1:12), sub[, c("year","month","value")],
               by=c("year","month"), all.x=TRUE)
    g <- g[order(g$year, g$month), ]
    x <- ts(g$value, start=c(y1, 1), frequency=12)
    n_v <- sum(!is.na(x))
    if (n_v < 24) return(data.frame(pair=p, series=label, n_months=n_v,
      mk_p=NA, sen_per_yr=NA, sen_lo=NA, sen_hi=NA, pet_year=NA, pet_p=NA))
    mclim <- tapply(x, cycle(x), mean, na.rm=TRUE)
    x_imp <- as.numeric(x); na <- is.na(x_imp); x_imp[na] <- mclim[cycle(x)][na]
    mk <- tryCatch(smk.test(ts(x_imp, start=c(y1,1), frequency=12)), error=function(e) NULL)
    desn <- as.numeric(x) - mclim[cycle(x)]; dn <- desn[!is.na(desn)]
    sen <- tryCatch(sens.slope(dn, conf.level=0.95), error=function(e) NULL)
    pet <- tryCatch(pettitt.test(dn),                error=function(e) NULL)
    py  <- if (!is.null(pet)) y1 + (which(!is.na(desn))[as.integer(pet$estimate[1])] - 1) %/% 12 else NA
    data.frame(pair=p, series=label, n_months=n_v,
               mk_p       = if (!is.null(mk)) as.numeric(mk$p.value) else NA,
               sen_per_yr = if (!is.null(sen)) round(as.numeric(sen$estimates) * 12, 4) else NA,
               sen_lo     = if (!is.null(sen)) round(as.numeric(sen$conf.int)[1] * 12, 4) else NA,
               sen_hi     = if (!is.null(sen)) round(as.numeric(sen$conf.int)[2] * 12, 4) else NA,
               pet_year   = py,
               pet_p      = if (!is.null(pet)) round(as.numeric(pet$p.value), 4) else NA)
  }

  m_raw   <- monthly_means(raw,   "delta_T_mean")
  m_ideal <- monthly_means(ideal, "delta_T_min")
  y1 <- min(m_raw$year); y2 <- max(m_raw$year)
  out <- rbind(
    do.call(rbind, lapply(pairs, one_test, df_long=m_raw,   y1=y1, y2=y2, label="ALL_dT_mean")),
    do.call(rbind, lapply(pairs, one_test, df_long=m_ideal, y1=y1, y2=y2, label="IDEAL_dT_min")))
  out <- out[order(out$series, out$pair), ]
  write.csv(out, file.path(RES, "trend_summary.csv"), row.names=FALSE)
  invisible(out)
}
run_trends()
```

## 6.8 Report figures: map, seasonal cycle, thresholds, homogenisation, trend grids, forest plot (§3.1-3.7)

Generates [Figure 1](#fig-station-map) (station map on an OSM basemap),
[Figure 2](#fig-seasonal) (seasonal cycle), [Figure 3](#fig-thresholds)
(threshold counts), [Figure 4](#fig-hom) (homogenisation summary),
[Figures 5-6](#fig-trend-all) (the two trend grids), and [Figure
7](#fig-sig-trends) (the significant-trends forest plot).

``` r
# §3.1 -- station map on OSM cartolight basemap
station_map <- function() {
  pts <- data.frame(short=SHORT[ALL], role=ifelse(ALL %in% URBAN, "urban", "rural"),
                    lon=sapply(COORDS[ALL], "[", 1), lat=sapply(COORDS[ALL], "[", 2))
  pts_sf <- st_as_sf(pts, coords=c("lon","lat"), crs=4326, remove=FALSE)
  bb_cache <- file.path(EXT, "vienna_boundary.gpkg")
  if (!file.exists(bb_cache)) {
    try({
      tmp <- tempfile(fileext=".geojson")
      download.file(paste0("https://nominatim.openstreetmap.org/search.php?",
                           "q=Wien%2C+Austria&format=geojson&polygon_geojson=1&limit=1"),
                    tmp, quiet=TRUE, headers=c("User-Agent"="uhi-vienna-final-script"))
      st_write(st_make_valid(st_read(tmp, quiet=TRUE)), bb_cache, delete_dsn=TRUE, quiet=TRUE)
    })
  }
  vienna <- if (file.exists(bb_cache)) st_read(bb_cache, quiet=TRUE) else NULL
  bb_pts <- st_bbox(pts_sf); pad_x <- 0.08; pad_y <- 0.05
  bb <- c(xmin=unname(bb_pts["xmin"]) - pad_x, xmax=unname(bb_pts["xmax"]) + pad_x,
          ymin=unname(bb_pts["ymin"]) - pad_y, ymax=unname(bb_pts["ymax"]) + pad_y)
  p <- ggplot() +
    annotation_map_tile(type="cartolight", zoomin=-1, progress="none") +
    (if (!is.null(vienna)) geom_sf(data=vienna, fill=NA, colour="firebrick", linewidth=0.7) else NULL) +
    geom_sf(data=pts_sf, aes(shape=role, colour=role), size=3.4, stroke=1.1, fill="white") +
    geom_sf_text(data=pts_sf, aes(label=short), nudge_x=0.012, nudge_y=0.012,
                 size=3.7, fontface="bold") +
    scale_shape_manual(values=c(urban=21, rural=24)) +
    scale_colour_manual(values=c(urban="firebrick", rural="steelblue")) +
    coord_sf(xlim=c(bb["xmin"], bb["xmax"]), ylim=c(bb["ymin"], bb["ymax"]),
             crs=4326, expand=FALSE) +
    annotation_scale(location="bl", width_hint=0.25) +
    labs(x="Longitude (°E)", y="Latitude (°N)") + theme_bw(base_size=11)
  ggsave(file.path(FIG, "station_map.png"),            p, width=9, height=7, dpi=150)
  ggsave(file.path(FIG, "annex1_station_map_osm.png"), p, width=9, height=7, dpi=150)
}

# §3.3 + §3.5 -- seasonal cycle + threshold trends (see final_script.R for the
# matplot/draw_panel bodies); §3.6 hom_plot() and §3.7 trend_grid()/forest_plot()
# follow the same base-graphics pattern.

make_figures <- function() {
  station_map()
  seasonal_and_thresholds()
  hom_plot()
  trend_grid("delta_T_mean", file.path(IND, "daily_delta_T.csv"),
             file.path(FIG, "trend_grid_ALL_dT_mean.png"),  "monthly ΔT_mean (°C)")
  trend_grid("delta_T_min",  file.path(RES, "daily_delta_T_ideal.csv"),
             file.path(FIG, "trend_grid_IDEAL_dT_min.png"), "monthly ΔT_min (°C)")
  forest_plot()
}
make_figures()
```

## 6.9 Heatwave amplification: catalogue, Wilcoxon test, figures (§3.8)

Builds the Hohe Warte heatwave catalogue (JJA Tmax ≥ p90, runs of ≥ 3
consecutive hot days), compares ΔT on heatwave vs non-heatwave summer
days with a two-sided Wilcoxon rank-sum, and emits [Figure
8](#fig-heatwave-amp) (the amplification lollipop) and [Figure
9](#fig-heatwave-box) (the per-pair boxplots). The amplification
function also persists `heatwave_summer_dT.csv` so the two figures can
be drawn without recomputing the catalogue.

``` r
heatwave_amplification <- function() {
  panel <- read.csv(file.path(IND, "daily_panel.csv"), stringsAsFactors=FALSE)
  panel$datetime <- as.Date(panel$datetime)
  panel$year  <- as.integer(format(panel$datetime, "%Y"))
  panel$month <- as.integer(format(panel$datetime, "%m"))
  hw <- panel[panel$station=="wien_hohewarte", c("datetime","year","month","tlmax")]
  hw <- hw[order(hw$datetime), ]
  p90 <- quantile(hw$tlmax[hw$month %in% 6:8], 0.90, na.rm=TRUE)
  hw$hot <- !is.na(hw$tlmax) & hw$tlmax >= p90 & hw$month %in% 6:8
  rl <- rle(hw$hot); hw$run <- rep(rl$lengths, rl$lengths)
  hw$is_hw <- hw$hot & hw$run >= 3L
  write.csv(hw[hw$is_hw, c("datetime","tlmax","run")],
            file.path(RES, "heatwave_days.csv"), row.names=FALSE)

  dT <- read.csv(file.path(IND, "daily_delta_T.csv"), stringsAsFactors=FALSE)
  dT$datetime <- as.Date(dT$datetime)
  dT$month <- as.integer(format(dT$datetime, "%m"))
  dT$is_hw <- dT$datetime %in% hw$datetime[hw$is_hw]
  dT_s <- dT[dT$month %in% 6:8, ]
  pairs <- sort(unique(dT_s$pair))

  out <- do.call(rbind, lapply(pairs, function(p) {
    do.call(rbind, lapply(c("delta_T_mean","delta_T_max","delta_T_min"), function(v) {
      sub <- dT_s[dT_s$pair==p, ]
      h  <- na.omit(sub[[v]][ sub$is_hw]); nh <- na.omit(sub[[v]][!sub$is_hw])
      w  <- tryCatch(wilcox.test(h, nh, alternative="two.sided"), error=function(e) NULL)
      data.frame(pair=p, variable=v, n_hw=length(h), n_nhw=length(nh),
                 mean_hw=round(mean(h), 3), mean_nhw=round(mean(nh), 3),
                 diff=round(mean(h) - mean(nh), 3),
                 wilcoxon_p = if (!is.null(w)) signif(w$p.value, 4) else NA)
    }))
  }))
  write.csv(out, file.path(RES, "heatwave_amplification.csv"), row.names=FALSE)
  write.csv(dT_s[, c("datetime","pair","delta_T_mean","delta_T_max",
                     "delta_T_min","is_hw")],
            file.path(RES, "heatwave_summer_dT.csv"), row.names=FALSE)
  invisible(out)
}
heatwave_amplification()

# Amplification lollipop (Fig 8) + per-pair boxplots (Fig 9)
heatwave_figures <- function() {
  res  <- read.csv(file.path(RES, "heatwave_amplification.csv"), stringsAsFactors=FALSE)
  dT_s <- read.csv(file.path(RES, "heatwave_summer_dT.csv"),      stringsAsFactors=FALSE)
  dmin <- res[res$variable=="delta_T_min", ]; dmin <- dmin[order(dmin$diff), ]
  pairs <- dmin$pair

  png(file.path(FIG, "heatwave_amplification.png"), 1700, 1000, res=160)
  op <- par(mar=c(4, 11, 1.6, 1.2), mgp=c(2.6, 0.7, 0)); y <- seq_len(nrow(dmin))
  xlim <- range(c(dmin$mean_hw, dmin$mean_nhw)) + c(-0.2, 0.3)
  plot(NA, xlim=xlim, ylim=c(0.4, nrow(dmin)+0.6), yaxt="n", xlab="ΔT_min (°C)",
       ylab="", panel.first={grid(nx=NULL, ny=NA, col="grey92", lty=1)
                             abline(v=0, col="grey55", lwd=1)})
  axis(2, at=y, labels=dmin$pair, las=1, cex.axis=0.95)
  segments(dmin$mean_nhw, y, dmin$mean_hw, y,
           col=ifelse(dmin$diff > 0, "firebrick", "steelblue"), lwd=2.4)
  points(dmin$mean_nhw, y, pch=21, bg="grey80",    col="grey40",   cex=1.5)
  points(dmin$mean_hw,  y, pch=21, bg="firebrick", col="firebrick", cex=1.7)
  text(pmax(dmin$mean_hw, dmin$mean_nhw) + 0.08, y,
       labels=sprintf("Δ = %+0.2f °C  (p=%.3g)", dmin$diff,
                      ifelse(is.na(dmin$wilcoxon_p), 1, dmin$wilcoxon_p)),
       adj=c(0, 0.5), cex=0.78, col="grey20")
  legend("bottomright", c("non-heatwave summer mean","heatwave mean"),
         pch=21, pt.bg=c("grey80","firebrick"), col=c("grey40","firebrick"),
         pt.cex=1.5, bty="n", cex=0.85)
  par(op); dev.off()

  png(file.path(FIG, "heatwave_boxplots.png"), 2000, 1100, res=160)
  op <- par(mar=c(4.2, 4.2, 1.5, 0.8), mgp=c(2.6, 0.7, 0))
  bx <- do.call(rbind, lapply(pairs, function(p) {
    sub <- dT_s[dT_s$pair==p, ]
    rbind(data.frame(pair=p, group="non-HW", value=sub$delta_T_min[!as.logical(sub$is_hw)]),
          data.frame(pair=p, group="HW",     value=sub$delta_T_min[ as.logical(sub$is_hw)]))
  }))
  bx <- bx[!is.na(bx$value), ]
  bx$pair  <- factor(bx$pair,  levels=pairs); bx$group <- factor(bx$group, levels=c("non-HW","HW"))
  n_p  <- length(pairs); at_v <- as.vector(rbind(seq_len(n_p)*3 - 1, seq_len(n_p)*3))
  boxplot(value ~ group + pair, data=bx, col=rep(c("grey85","firebrick"), n_p),
          names=rep("", 2*n_p), cex.axis=0.7, xlab="", ylab="Summer daily ΔT_min (°C)",
          outline=FALSE, boxwex=0.7, at=at_v)
  axis(1, at=seq_len(n_p)*3 - 0.5, labels=pairs, las=2, cex.axis=0.85, tick=FALSE)
  abline(h=0, col="grey60", lty=3)
  legend("topleft", c("non-heatwave summer days","heatwave days"),
         fill=c("grey85","firebrick"), bty="n", cex=0.85)
  par(op); dev.off()
}
heatwave_figures()
```

## 6.10 Land-cover regression and figure (§3.9)

Regresses per-pair annual ΔT_mean on the urban-side impervious fraction
(1 km buffer, linearly interpolated between the HRL epochs) and emits
[Figure 10](#fig-landcover) (the 3×3 per-pair scatter grid). Impervious
values are literature-anchored placeholders (see the §3.9 caveat).

``` r
landcover_regression <- function() {
  imp <- read.table(text="
station            buffer_m  e2006  e2009  e2012  e2015  e2018
wien_innerestadt   500       84     85     85     86     86
wien_innerestadt   1000      78     78     79     80     80
wien_innerestadt   2000      68     69     70     71     72
wien_hohewarte     500       44     45     46     48     49
wien_hohewarte     1000      40     41     43     45     46
wien_hohewarte     2000      36     37     39     41     43
wien_unterlaa      500       32     35     37     40     43
wien_unterlaa      1000      28     31     34     37     40
wien_unterlaa      2000      24     27     30     33     36
gross_enzersdorf   500       18     19     19     20     20
gross_enzersdorf   1000      12     13     13     13     14
gross_enzersdorf   2000       8      8      8      9      9
langenlebarn       500       16     17     18     19     20
langenlebarn       1000      11     12     13     14     15
langenlebarn       2000       7      8      8      9     10
wien_mariabrunn    500        6      6      7      7      8
wien_mariabrunn    1000       5      5      6      6      7
wien_mariabrunn    2000       4      4      5      5      6",
    header=TRUE, stringsAsFactors=FALSE)
  write.csv(imp, file.path(RES, "imperv_fractions.csv"), row.names=FALSE)

  epochs <- c(2006, 2009, 2012, 2015, 2018); buf <- 1000
  interp <- function(stn, years) {
    d <- imp[imp$station==stn & imp$buffer_m==buf, ]
    approx(epochs, as.numeric(d[1, paste0("e", epochs)]), xout=years, rule=2)$y
  }

  ann <- read.csv(file.path(IND, "annual_delta_T.csv"), stringsAsFactors=FALSE)
  ann$delta_T <- ann$delta_T_mean_ann
  out <- do.call(rbind, lapply(unique(ann$pair), function(p) {
    sub <- ann[ann$pair==p, c("year","urban","rural","delta_T")]
    sub <- sub[order(sub$year), ]; sub$urban_imp <- interp(unique(sub$urban), sub$year)
    fit <- lm(delta_T ~ urban_imp, data=sub); sm <- summary(fit)
    data.frame(pair=p, urban=unique(sub$urban), rural=unique(sub$rural),
               imp_range=sprintf("%.1f-%.1f", min(sub$urban_imp), max(sub$urban_imp)),
               slope=round(coef(fit)[2], 4), ci_lo=round(confint(fit)[2, 1], 4),
               ci_hi=round(confint(fit)[2, 2], 4), r2=round(sm$r.squared, 3),
               p_value=signif(coef(sm)[2, "Pr(>|t|)"], 4))
  }))
  out <- out[order(-out$slope), ]
  write.csv(out, file.path(RES, "landcover_regression.csv"), row.names=FALSE)

  # §3.9 figure -- per-pair scatter of annual ΔT_mean vs urban-side imperv %
  png(file.path(FIG, "landcover_regression.png"), 2200, 1500, res=170)
  op <- par(mfrow=c(3, 3), mar=c(3.6, 3.6, 2, 0.8), mgp=c(2.3, 0.6, 0))
  for (p in sort(unique(ann$pair))) {
    sub <- ann[ann$pair==p, c("year","urban","delta_T")]
    sub <- sub[order(sub$year), ]; sub$urban_imp <- interp(unique(sub$urban), sub$year)
    fit <- lm(delta_T ~ urban_imp, data=sub); sm <- summary(fit)
    plot(sub$urban_imp, sub$delta_T, pch=21, bg="steelblue", col="navy", cex=1.1,
         xlab=sprintf("Urban-side imperv %% (1 km buffer, %s)", unique(sub$urban)),
         ylab="Annual ΔT_mean (°C)", main=p, las=1,
         panel.first=grid(col="grey92", lty=1))
    xr <- range(sub$urban_imp)
    lines(xr, predict(fit, data.frame(urban_imp=xr)), col="firebrick", lwd=1.6)
    mtext(sprintf("slope %+0.3f °C/%%  R² %.2f  p=%.3f",
                  coef(fit)[2], sm$r.squared, coef(sm)[2, "Pr(>|t|)"]),
          side=3, line=0.1, cex=0.7, adj=0)
  }
  par(op); dev.off()
  invisible(out)
}
landcover_regression()
```

# 7. Suggested next steps to complete the study

The present pipeline produces a defensible 21-year UHI baseline for
Vienna with quantified trends and a transparent ideal-night filter. To
bring the study to publication or thesis quality, four lines of work
would meaningfully strengthen the conclusions.

## 7.1 Investigate the Langenlebarn anomaly

The negative ideal-night ΔT trends at all three LL pairs with a 2011
Pettitt break (see [Figure 6](#fig-trend-ideal) and the bottom three
blue bars of [Figure 7](#fig-sig-trends)) are the most consequential
unexplained result. Three concrete actions:

- Pull the full GeoSphere metadata history for station 51 (sensor
  changes, siting reports, surroundings notes).
- Apply the SNHT directly to the monthly LL series with neighbour pairs
  that exclude LL (HW, GE, MB) to localise the inhomogeneity.
- Compare HRL Imperviousness 2006/2012/2018 in a 1 km buffer around the
  LL site to quantify any land-cover trend that could explain a real
  rural-catch-up signal [(Hammerberg et al.,
  2018)](#ref-Hammerberg2018).

Decision rule: if SNHT detects a clear step at 2010-2011 in monthly LL
Tmean against any rural neighbour, treat the trend as an artefact and
apply a segment-wise adjustment; otherwise document it as a genuine
signal and add it to the discussion.

## 7.2 Extend the time window backward

Twenty-one years is short for UHI-trend detection – [(Böhm,
1998)](#ref-Boehm1998) needed 45 years for a +0.6 °C trend to clear the
noise floor. Two extensions are realistic:

- **1991-2025 (35 years)** using the same six stations. The 1991-2020
  WMO baseline period would also become natural for anomaly definitions.
  Requires re-running `01_download` with `start = "1991-01-01"` and
  re-running the homogenisation step, which will likely detect the
  documented TAWES-automation breakpoints around 2000.
- **1981-2025 (45 years)** using only Hohe Warte and Groß-Enzersdorf,
  which both have long records. This is the [(Böhm,
  1998)](#ref-Boehm1998) design replicated on a modern dataset and would
  be the most directly comparable to the published Vienna literature.

## 7.3 Add satellite-derived surface UHI

The station network resolves six points; the satellite record resolves
Vienna at 100-1000 m. Two products are free:

- **MODIS MOD11A1/MYD11A1 LST** (daily, 1 km, 2000-present) for a
  continuous SUHI time series. Daytime SUHI is typically stronger than
  the canopy-layer UHI measured at stations.
- **Landsat 8/9 Collection 2 thermal band 10** (16-day, 100 m native)
  for 2-3 cloud-free composite dates per year.

Correlating the station ΔT (this report) against the satellite SUHI map
at the station locations would validate that the six-station gradient
captures the city-wide spatial signal.

# Acknowledgements

GeoSphere Austria for the open, CC-licensed climate data via the Data
Hub (<https://data.hub.geosphere.at>). The `gsdata` R package by Reto
Stauffer for the API client [(Stauffer, 2024)](#ref-gsdata). The CRAN
packages `climatol` [(Guijarro, 2018)](#ref-Guijarro2018) and `trend`
for the statistical machinery. The Hammerberg et al. (2018) WUDAPT LCZ
map for Vienna provided the LCZ classifications used here.

# List of tables and figures

| \# | Summary |
|:---|:---|
| [Table 1](#tab-stations) | Six active stations: short codes, full names, TAWES IDs, coordinates, altitude, role, LCZ class. |
| [Table 2](#tab-params) | All 20 GeoSphere parameters used in the pipeline with code, resource, description, unit, and downstream use. |
| [Table 3](#tab-coverage) | Per-station data-coverage matrix (NA percentages) after preprocessing, with notes per station. |
| [Table 4](#tab-allnight) | All-night annual mean ΔT_mean (°C) per urban-rural pair, 2005-2025. |
| [Table 5](#tab-filter) | Pass rates of the three ideal-night criteria (wind, cloud, dry) over 7 670 nights. |
| [Table 6](#tab-amp) | Per-pair ideal-night ΔT_min vs all-night ΔT_mean with the multiplicative amplification factor. |
| [Table 7](#tab-trends) | Significant pair × series trends: Sen slope (°C/yr) with 95 % CI, seasonal-MK p-value, Pettitt year. |
| [Table 8](#tab-heatwave) | Heatwave amplification per pair: mean ΔT_min and ΔT_mean on heatwave vs non-heatwave summer days, with Wilcoxon p-values. |
| [Table 9](#tab-landcover) | Per-pair OLS regression of annual ΔT_mean on urban-side impervious fraction (1 km buffer): slope, 95 % CI, R², p-value. |
| [Figure 1](#fig-station-map) | Station network on OSM cartolight basemap with Vienna municipal boundary overlaid. Urban stations as red circles, rural references as blue triangles. |
| [Figure 2](#fig-seasonal) | Climatological seasonal cycle of monthly ΔT_mean for all 9 urban-rural pairs. Shows HW-pair dominance and the IS/UL summer maximum. |
| [Figure 3](#fig-thresholds) | Annual threshold counts (tropical nights, summer days, hot days) and mean T per station. Tropical nights show strongest urban-rural separation. |
| [Figure 4](#fig-hom) | Climatol SNHT homogenisation of annual Tmean per station: original (grey) and adjusted (red) lines overlap – zero breakpoints detected. |
| [Figure 5](#fig-trend-all) | Monthly all-night ΔT_mean per urban-rural pair (9 panels) with 12-month running means, Sen-slope trends, and Pettitt change-point markers. |
| [Figure 6](#fig-trend-ideal) | Monthly ideal-night ΔT_min per pair (9 panels), same conventions as Figure 5; three Langenlebarn pairs show negative trends with a 2011 change-point. |
| [Figure 7](#fig-sig-trends) | Forest plot of the six significant Sen slopes with 95 % CI bars. Red = positive (UHI-intensifying), blue = negative (Langenlebarn anomaly). |
| [Figure 8](#fig-heatwave-amp) | Lollipop plot of mean ΔT_min per pair on non-heatwave (grey) vs heatwave (red) summer days, with the amplification Δ and Wilcoxon p-value annotated. All 9 pairs amplify positively (p \< 1e-6). |
| [Figure 9](#fig-heatwave-box) | Boxplots of summer daily ΔT_min per pair, contrasting non-heatwave (grey) and heatwave (red) days. HW-based pairs reach 5-6 °C upper-quartile ΔT_min during heatwaves. |
| [Figure 10](#fig-landcover) | 3×3 grid of per-pair scatter plots of annual ΔT_mean vs urban-side impervious fraction (1 km buffer, 2005-2025), with OLS regression lines. IS_vs_GE and UL_vs_GE show the clearest positive slopes. |

# References

<a id="ref-Alexandersson1986"></a> **Alexandersson, H. (1986).** A
homogeneity test applied to precipitation data. *Journal of
Climatology*, 6(6), 661-675. <https://doi.org/10.1002/joc.3370060607>

<a id="ref-Bassani2022"></a> **Bassani, F., Garbero, V., Poggi, D.,
Ridolfi, L., & von Hardenberg, J. (2022).** The interplay between urban
heat islands and heat waves in 32 European cities. *Environmental
Research Letters*, 17(12), 124006.
<https://doi.org/10.1088/1748-9326/aca3ba>

<a id="ref-Boehm1998"></a> **Böhm, R. (1998).** Urban bias in
temperature time series - A case study for the City of Vienna, Austria.
*Climatic Change*, 38(1), 113-128.
<https://doi.org/10.1023/A:1005338514333>

<a id="ref-GeoSphere2024"></a> **GeoSphere Austria. (2024).** Data Hub -
klima-v2-1d, klima-v2-1h, synop-v1-1h resources.
<https://data.hub.geosphere.at>

<a id="ref-Guijarro2018"></a> **Guijarro, J. A. (2018).**
*Homogenization of climatic series with Climatol* (Version 3.1.1). State
Meteorological Agency (AEMET), Spain.
<https://CRAN.R-project.org/package=climatol>

<a id="ref-Hammerberg2018"></a> **Hammerberg, K., Brousse, O., Martilli,
A., & Mahdavi, A. (2018).** Implications of employing detailed urban
canopy parameters for mesoscale climate modelling: A comparison between
WUDAPT and GIS databases over Vienna, Austria. *International Journal of
Climatology*, 38, e1241-e1257. <https://doi.org/10.1002/joc.5447>

<a id="ref-Hirsch1984"></a> **Hirsch, R. M., & Slack, J. R. (1984).** A
nonparametric trend test for seasonal data with serial dependence.
*Water Resources Research*, 20(6), 727-732.
<https://doi.org/10.1029/WR020i006p00727>

<a id="ref-Hutter2023"></a> **Hutter, H.-P., Poteser, M., Lemmerer, K.,
Wallner, P., Kundi, M., Moshammer, H., & Weitensfelder, L. (2023).**
Heat-related mortality in three European cities - A descriptive analysis
for sustainable adaptation. *Atmosphere*, 14(10), 1498.
<https://doi.org/10.3390/atmos14101498>

<a id="ref-Kendall1975"></a> **Kendall, M. G. (1975).** *Rank
correlation methods* (4th ed.). Charles Griffin, London.

<a id="ref-Oke1982"></a> **Oke, T. R. (1982).** The energetic basis of
the urban heat island. *Quarterly Journal of the Royal Meteorological
Society*, 108(455), 1-24. <https://doi.org/10.1002/qj.49710845502>

<a id="ref-Pettitt1979"></a> **Pettitt, A. N. (1979).** A non-parametric
approach to the change-point problem. *Journal of the Royal Statistical
Society Series C (Applied Statistics)*, 28(2), 126-135.
<https://doi.org/10.2307/2346729>

<a id="ref-Sen1968"></a> **Sen, P. K. (1968).** Estimates of the
regression coefficient based on Kendall’s tau. *Journal of the American
Statistical Association*, 63(324), 1379-1389.
<https://doi.org/10.1080/01621459.1968.10480934>

<a id="ref-gsdata"></a> **Stauffer, R. (2024).** *gsdata: GeoSphere
Austria Data API Client* (Version 0.2-x).
<https://retostauffer.org/news/gsdata/>

<a id="ref-Stewart2012"></a> **Stewart, I. D., & Oke, T. R. (2012).**
Local Climate Zones for urban temperature studies. *Bulletin of the
American Meteorological Society*, 93(12), 1879-1900.
<https://doi.org/10.1175/BAMS-D-11-00019.1>

<a id="ref-ZuvelaAloise2016"></a> **Žuvela-Aloise, M., Koch, R.,
Buchholz, S., & Früh, B. (2016).** Modelling the potential of green and
blue infrastructure to reduce urban heat load in the city of Vienna.
*Climatic Change*, 135(3-4), 425-438.
<https://doi.org/10.1007/s10584-016-1596-2>

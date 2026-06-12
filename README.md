# FMD Diameter Cleaner v0.2

For cleaning diameter (or any continuous physiological) data from LabChart 8 or any source.

## Requirements

- [R](https://mirror.rcg.sfu.ca/mirror/CRAN/)
- [RStudio](https://posit.co/downloads/)

All other packages are installed automatically on first run.

## How to run

Paste the following into your RStudio console and press Enter:

```r
source("https://github.com/nkcheung95/FMD-Diameter-Cleaner/blob/main/FMD-Dia-Clean-load.r?raw=TRUE")
```

## Data import

Supports **CSV**, **TXT**, and **XLSX** files of any length and sample rate.

For LabChart 8 TXT exports:
- Select the FMD portion and export selection
- Export: Diameter channel, Time (seconds), Comments
- Downsample ×100 for 1 kHz files (gives ~10 Hz; window of 11 = ~1 second)

## Smoothing methods

| Method | What it does |
|---|---|
| **Median + IQR** | Rolling median detects outliers via IQR; outliers replaced with PCHIP interpolation |
| **Butterworth Low-Pass** | Zero-phase `filtfilt` smoothing; adjustable cutoff (Hz) and filter order |
| **Median + IQR → Butterworth** | Spike removal first, then frequency-domain smoothing on the clean signal |

All methods use **reflect padding** at both ends to eliminate edge artefacts regardless of window size.

**Interpolation:** PCHIP (monotone piecewise cubic Hermite) — avoids overshoot near large gaps.

## Controls

- **Window size** — rolling median window in samples (odd numbers; ~1 s at 10 Hz = 11)
- **IQR threshold** — points beyond `threshold × IQR` from the rolling median are flagged (default 1.5; increase to be less aggressive)
- **Cutoff frequency (Hz)** — Butterworth low-pass cutoff; auto-capped at 95% of Nyquist
- **Filter order** — steeper roll-off at higher orders (default 4)
- **FMD lines** — red dashed lines at 60 s (occlusion) and 360 s (release)

## Output

Download cleaned data as CSV. Includes `outlier`, `clean_data`, `filename`, and `method` columns.

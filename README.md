# Shiny Data CleanR v0.2

For cleaning diameter (or any continuous physiological) data from LabChart 8 or any source.

## Requirements

* [R](https://mirror.rcg.sfu.ca/mirror/CRAN/)
* [RStudio](https://posit.co/downloads/)

All other packages are installed automatically on first run.

## How to run

Paste the following into your RStudio console and press Enter:

```r
source("https://github.com/nkcheung95/Shiny-Data-CleanR/blob/main/CleanR-load.r?raw=TRUE")

```

## Data Import

Supports **CSV**, **TXT**, and **XLSX** files of any length and sample rate.

* **Smart Header Selection:** The app previews the first 20 rows of your file directly in the sidebar. You can keep it on **Auto-detect Header** or manually choose the exact row containing your column names.

---

## Smoothing Methods

| Method | What it does |
| --- | --- |
| **Median + IQR** | Rolling median detects outliers via IQR; outliers replaced with PCHIP interpolation. |
| **Butterworth Low-Pass** | Zero-phase `filtfilt` smoothing; adjustable cutoff (Hz) and filter order. |
| **Median + IQR → Butterworth** | Spike removal first, then frequency-domain smoothing on the clean signal. |

> **Note on Edge Artifacts & Interpolation:** All methods use **reflect padding** at both ends to eliminate edge distortions regardless of window or filter size. Gap filling uses **PCHIP** (monotone piecewise cubic Hermite) interpolation to preserve data shape and avoid mathematical overshoots/oscillations near large data gaps.

---

## Controls

### File & Column Settings

* **Header row setting** — Dropdown menu displaying actual row contents to pick the correct column headers.
* **Column Selectors** — Auto-guesses your Time and Data channels while allowing manual re-assignment.

### Filtering Parameters

* **Window size** — Rolling median window in samples (odd numbers; e.g., ~1 s of data at 10 Hz = 11).
* **IQR threshold** — Points beyond `threshold × IQR` from the rolling median are flagged as outliers (default 1.5; increase to be less aggressive).
* **Cutoff frequency (Hz)** — Butterworth low-pass cutoff frequency. The slider automatically adjusts its maximum limit based on the file's unique sample rate to always safely stay below 95% of the Nyquist frequency.
* **Filter order** — Steeper roll-off at higher orders (default 4).

### Display & Annotation

* **Vertical line positions** — A customizable text field allowing you to input up to 5 specific time points (in seconds, comma-separated) to display red dashed milestone lines on the plots. Defaults to `60, 360` for standard FMD protocols, but can be cleared entirely or adjusted to match any protocol landmarks.

---

## Output

Download your processed data as a tidy CSV file. The exported file appends the following columns to your original dataset:

* `moving_median` — The rolling center line calculated during outlier detection.
* `outlier` — Logical flags (`TRUE`/`FALSE`) marking rows that were cleaned.
* `clean_data` — The final filtered and smoothed values ready for analysis.
* `filename` — The source file name for tracking purposes.
* `method` — The exact smoothing pipeline configuration used to generate the data.

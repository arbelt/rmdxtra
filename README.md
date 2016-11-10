---
title: rmdxtra Package
---

# Description

A package of utilities for customizing Rmarkdown output.  This package currently
includes a slightly enriched `beamer_plus` and themed `oir_beamer` document
format for use with Rmarkdown.

# Installation

Install from Github using `devtools`:

```r
if (!require("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("arbelt/rmdxtra")
```

# Usage

In the YAML header of an Rmarkdown document, specify `rmdxtra::oir_beamer` as
the output format. For example:

```yaml
output: rmdxtra::oir_beamer
```

Or, if specifying options:

```yaml
output:
  rmdxtra::oir_beamer:
    keep_tex: true
```

Since the format is derived from the default `beamer_presentation`, all options
there are valid in `beamer_plus` and `oir_beamer`.

## OIR Beamer

The `oir_beamer` format is themed and allows for some additional customization.
It automatically specifies `xelatex` as the latex engine. Furthermore, it uses
the _metropolis_ beamer theme package, so ensure that you have an up to date TeX
distribution (on MacTeX, this can be done using the _TeX Live Utility_).

It passes `fontsize` and `classoption` to pandoc. `textfont` can be one of
"FiraSans", "FiraSansCondensed", or "CooperHewitt" (perhaps more later, but
these are the ones packages in the helper package `azwmisc.fonts`).

```yaml
output:
  rmdxtra::oir_beamer:
    fontsize: 11pt
    classoption: aspectratio=1610
    textfont: FiraSans
```

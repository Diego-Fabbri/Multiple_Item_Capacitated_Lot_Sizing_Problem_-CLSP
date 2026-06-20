# Multiple Item Capacitated Lot Sizing Problem (CLSP)

A Mixed Integer Linear Programming (MILP) model in **R** for the **Multi-Item Capacitated Lot Sizing Problem**, built with the [`ompr`](https://dirkschumacher.github.io/ompr/) modeling framework and solved via the **SYMPHONY** solver (through `ROI`).

## Overview

The Capacitated Lot Sizing Problem (CLSP) is a classic production planning problem in Operations Research. Given a set of items, a planning horizon divided into discrete periods, and a shared production resource with limited capacity, the goal is to decide **how much of each item to produce in each period** in order to satisfy known demand while minimizing total cost — without exceeding capacity in any period.

This repository contains a working R implementation of the multi-item variant, where several products compete for the same capacitated resource in every time period.

## Problem Formulation

For each item *j* and period *t*, the model decides:

- **q[t, j]** — quantity of item *j* produced in period *t*
- **I[t, j]** — inventory level of item *j* at the end of period *t*
- **y[t, j]** — binary setup indicator (1 if item *j* is produced in period *t*, 0 otherwise)

**Objective** — minimize total cost, composed of:
- Setup costs (`K[j]`), incurred whenever an item is produced in a period
- Unit production costs (`c[t, j]`)
- Unit inventory holding costs (`h[t, j]`)

**Constraints**:
- Initial and final inventory levels are fixed
- Inventory balance across consecutive periods (production + opening stock = demand + closing stock)
- Production in a period is only allowed if the setup variable is activated, and is bounded by the period's available capacity
- All resource capacity limits (`C[t]`) must be respected

A detailed mathematical formulation is provided in the accompanying PDF document included in this repository.

## Repository Contents

| File | Description |
|---|---|
| `Multiple_Item_Capacitated_Lot_Sizing_Problem.R` | R script implementing and solving the CLSP instance |
| `Multy Item Capacitated Lot Sizing Problem (CLSP).pdf` | Mathematical formulation of the problem |

## Requirements

The script relies on the following R packages:

```r
install.packages(c("lpSolve", "dplyr", "ROI", "ROI.plugin.symphony", "ompr", "ompr.roi"))
```

## Usage

1. Clone or download this repository.
2. Open `Multiple_Item_Capacitated_Lot_Sizing_Problem.R` in R or RStudio.
3. Update the working directory at the top of the script if needed.
4. Run the script. It will:
   - Build the MILP model from the input data (demands, costs, capacities)
   - Solve it using the SYMPHONY solver
   - Print the solver status, the optimal objective value, and the optimal production (`q`), inventory (`I`), and setup (`y`) decisions for each item and period

## Example Instance

The script ships with a sample instance featuring:
- **3 items**
- **5 demand periods** (plus an initial period t = 0)
- Item-specific setup costs, holding costs, and a shared per-period production capacity

These parameters (`d`, `K`, `c`, `h`, `C`) can be freely modified to test other scenarios.

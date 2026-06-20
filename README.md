# Multiple Item Capacitated Lot Sizing Problem (CLSP)

A Mixed Integer Linear Programming (MILP) model in **R** for the **Multi-Item Capacitated Lot Sizing Problem**, built with the [`ompr`](https://dirkschumacher.github.io/ompr/) modeling framework and solved via the **SYMPHONY** solver (through `ROI`).

## Overview

The Capacitated Lot Sizing Problem (CLSP) is a classic production planning problem in Operations Research. Given a set of items, a planning horizon divided into discrete periods, and a shared production resource with limited capacity, the goal is to decide **how much of each item to produce in each period** in order to satisfy known demand while minimizing total cost — without exceeding capacity in any period.

This repository contains a working R implementation of the multi-item variant, where several products compete for the same capacitated resource in every time period.

## Mathematical Formulation

### Sets
- $T$ = the planning horizon (index $t = 0, 1, \dots, n$)
- $J$ = set of items (index $j = 1, \dots, m$)

### Parameters
- $d_{tj}$ = the demand forecast at time $t$ for item $j$
- $c_{tj}$ = the unit production or purchasing cost at time $t$ for item $j$
- $h_{tj}$ = the unit inventory cost at time $t$ for item $j$
- $K_j$ = the fixed setup or ordering cost for item $j$
- $C_{tj}$ = the maximum feasible lot size (capacity) at time $t$ for item $j$

### Variables
- $I_{tj}$ = inventory level at the end of period $t$ for item $j$
- $q_{tj}$ = quantity to be produced or ordered during period $t$ for item $j$
- $y_{tj} = \begin{cases} 1 & \text{if units of item } j \text{ are manufactured/ordered in period } t \\ 0 & \text{otherwise} \end{cases}$

### Objective Function

$$
\min \sum_{t=1}^{n} \sum_{j=1}^{m} \Big( K_j \cdot y_{tj} + c_{tj} \cdot q_{tj} + h_{tj} \cdot I_{tj} \Big) \tag{1}
$$

### Constraints

$$
I_{tj} = 0 \qquad t = 0 \text{ and } t = n;\ \forall j \in J \tag{2}
$$

$$
q_{tj} + I_{t-1,j} = d_{tj} + I_{tj} \qquad \forall t \in T \setminus \{0\};\ \forall j \in J \tag{3}
$$

$$
q_{tj} \le C_{tj} \cdot y_{tj} \qquad \forall t \in T \setminus \{0\};\ \forall j \in J \tag{4}
$$

$$
q_{tj} \ge 0 \qquad \forall t \in T \setminus \{0\};\ \forall j \in J \tag{5}
$$

$$
I_{tj} \ge 0 \qquad \forall t \in T \setminus \{0\};\ \forall j \in J \tag{6}
$$

$$
y_{tj} \in \{0,1\} \qquad \forall t \in T \setminus \{0\};\ \forall j \in J \tag{7}
$$

### Interpretation

The objective function (1) represents the total management costs, including production (and/or purchasing), inventory, and setup/ordering costs. Conditions (2) impose that inventory levels at the beginning and end of the planning horizon are equal to zero. Constraints (3) reproduce the demand satisfaction and inventory balance constraint for each period. Constraints (4)–(5) allow positive production (bounded between 0 and the period capacity $C_{tj}$) if and only if the setup variable $y_{tj}$ is equal to 1.

A copy of this formulation is also available as a standalone PDF in this repository.

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

## License

No license has been specified for this repository. Please contact the author before reusing this code for purposes beyond personal study or reference.

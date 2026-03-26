#!/usr/bin/env bash
# =====================================================================
# ICRAG-2 HTA Report: Compute Pipeline + Quarto Render
# =====================================================================
# Usage:  cd "Secondary Prevention Microsimulation"
#         bash report/run_all.sh
#
# Runs base case → DSA → PSA sequentially (clear progress output),
# then renders the Quarto report.
# =====================================================================

set -e
cd "$(dirname "$0")/.."   # Move to project root

echo "═══════════════════════════════════════════════════"
echo "  ICRAG-2 HTA: Compute Pipeline"
echo "═══════════════════════════════════════════════════"
echo ""

mkdir -p report/outputs

# Track start time
START=$(date +%s)

# --- Step 1: Base Case ---
echo "[Step 1/4] Base Case Analysis..."
echo "  Started: $(date '+%H:%M:%S')"
Rscript report/01_base_case.R
T1=$(date +%s)
echo "  ✓ Base case done ($(( T1 - START ))s)"
echo ""

# --- Step 2: DSA + Threshold (needs base_case.rds) ---
echo "[Step 2/4] DSA + Threshold Analysis..."
echo "  Started: $(date '+%H:%M:%S')"
Rscript report/02_dsa.R
T2=$(date +%s)
echo "  ✓ DSA done ($(( T2 - T1 ))s)"
echo ""

# --- Step 3: PSA + EVPI ---
echo "[Step 3/4] PSA + EVPI (this takes a few minutes)..."
echo "  Started: $(date '+%H:%M:%S')"
Rscript report/03_psa.R
T3=$(date +%s)
echo "  ✓ PSA done ($(( T3 - T2 ))s)"
echo ""

# --- Step 4: Render report ---
echo "[Step 4/4] Rendering Quarto report..."
echo "  Started: $(date '+%H:%M:%S')"
cd report
quarto render ICRAG2_HTA_Report.qmd --to html
cd ..

END=$(date +%s)
ELAPSED=$(( END - START ))
MINS=$(( ELAPSED / 60 ))
SECS=$(( ELAPSED % 60 ))

echo ""
echo "═══════════════════════════════════════════════════"
echo "  ✓ All done! Total time: ${MINS}m ${SECS}s"
echo "  Report: report/ICRAG2_HTA_Report.html"
echo "═══════════════════════════════════════════════════"

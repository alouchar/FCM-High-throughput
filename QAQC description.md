### Description

Quality Control procedure is a mandatory step for FAIR (Findability, Accessibility, Interoperability and Reuse) data. We propose to the user to investigate machine quality control through **beads** and **blanc** **files** prior starting analysing their data. This step is not mandatory but it is highly recommanded. You can skip this step, it will not interfer on the analytical procedure.

### QC procedure

1.  Beads file: The file contains only eight-peak beads for Photomultiplier linearity. The Coefficient of variation (CV) computed for each peak. The CV of the most sensitive peak (last peak) should be maximum 3 to 4.

2.  Blanc file: The blank file analyses the sheath fluid alone for contamination detection. Good settings should allow the visualisation of few particles. If no particles are displayed, your settings are probably not enough sensitive for the analysis. If too many particles are displayed, your sheath fluid might be contaminated. High number of particles can also reveal too sensitive settings.

For both steps above, if no particles were recorded, be sure that you turned on the laser for the analysis.

Note that control check on samples are not done in this step. See *Import* tab for this.

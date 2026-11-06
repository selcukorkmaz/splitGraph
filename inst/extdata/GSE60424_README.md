# GSE60424_samples.csv

Sample-level metadata (no expression values) for GEO series
[GSE60424](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE60424):
RNA-seq of whole blood and six sorted immune cell populations from 20 donors
(healthy controls; type 1 diabetes; amyotrophic lateral sclerosis; sepsis;
multiple sclerosis). 134 samples. Used by `vignette("case-study-gse60424")`.

## Provenance

- Retrieved 2026-09-14 with `GEOquery::getGEO("GSE60424", GSEMatrix = TRUE,
  getGPL = FALSE)`; only `Biobase::pData()` of the series matrix was kept.
- The `characteristics_ch1.*` columns were split on the first `:` into
  key/value pairs. Columns kept and renamed:

| Column | Source field | Notes |
|---|---|---|
| `geo_accession` | `geo_accession` | GSM id |
| `sample_id` | `samplename` | e.g. `44_Tempus`; unique |
| `subject_id` | `donorid` | prefixed with `D` |
| `cell_type` | `celltype` | B-cells, CD4, CD8, Monocytes, Neutrophils, NK, Whole Blood |
| `disease_status` | `diseasestatus` | as recorded, including "MS pretreatment" / "MS posttreatment" |
| `condition` | derived | `disease_status` with the MS pre/post suffix removed (5 levels) |
| `timepoint_id`, `time_index` | derived | `pre_treatment` / `post_treatment` for MS, else `baseline`. In the GEO series matrix, the six MS blood draws carry six distinct `donorid` values (`D31`, `D45`, `D55` pretreatment; `D33`, `D46`, `D56` posttreatment), even though the study protocol sampled 3 MS patients before and 24 hours after IFN-beta treatment on consecutive days (`2012-05-21`/`22`, `2012-06-28`/`29`, `2012-08-23`/`24`). Kept so vignettes can show both raw `donorid` ingestion and how to link paired pre/post draws. |
| `collection_date` | `collectiondate` | as recorded, e.g. `June 26 2012` |
| `batch_id` | derived | `collection_date` as ISO `YYYY-MM-DD`; one date per donor |
| `sex` | `gender` | `F`, `M`, or NA |
| `library_index` | `index` | sequencing library index |

- Rows are sorted by `subject_id`, `cell_type`, `timepoint_id`.
- Month names were mapped explicitly (not via the locale) when deriving
  `batch_id`.

## Terms

GEO data are public. The metadata here are reproduced solely to demonstrate
dataset-structure modelling; consult the GEO record and its linked publication
for the study itself, and cite the accession when reusing these values.

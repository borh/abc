# Parser RQ Publication Census Characterization

The production-shaped fixture chain was run twice against the pinned three-work corpus. Both canonical observations were byte-identical. The live qualification transform is mapping 0.5.0 (`aat-to-parser-ir-mapping-v2.json`); the roadmap's v1 filename was incompatible with the live AAT v2 output and failed closed before measurement.

Minimums retain every strictly positive observation among `source_identity`, `mapping_identity`, `span_coordinates`, and `gaiji_resolution`. Zero observations are not converted into requirements.

| Work | Records | Required construct minimums |
|---|---:|---|
| `000001_1` | 13 | `source_identity=4`, `mapping_identity=3`, `span_coordinates=3` |
| `000002_2` | 15 | `source_identity=4`, `mapping_identity=3`, `span_coordinates=4`, `gaiji_resolution=1` |
| `000003_3` | 16 | `source_identity=4`, `mapping_identity=3`, `span_coordinates=5`, `gaiji_resolution=1` |

The qualification-only materialization path was invoked directly because the release CLI correctly remains blocked by the repository-wide rights-containment policy.

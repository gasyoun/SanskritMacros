# SanskritMacros

_Created: 14-06-2026 · Last updated: 11-07-2026_

Editor and office-suite macros for Sanskrit text processing — EmEditor
transliteration/encoding converters, MS Excel and MS Word macros, and a few
RStudio scripts. Part of the [sanskrit-lexicon](https://github.com/sanskrit-lexicon)
ecosystem; this repo is the personal macro toolkit that predates and feeds the
lexicography pipelines (several converter scripts carry a 2013 author note).

There is no build system, package, or test suite here — each file is a
standalone macro or script you load into its host application (EmEditor, Excel,
Word, or RStudio) and run against open text.

## Layout

| Directory | What it holds |
|---|---|
| [`EmEditor Macros/`](https://github.com/gasyoun/SanskritMacros/tree/master/EmEditor%20Macros) | EmEditor `.vbee` macros — transliteration converters and tag extractors |
| [`MSExcel Macros/`](https://github.com/gasyoun/SanskritMacros/tree/master/MSExcel%20Macros) | Excel workbooks (`.xlsm`/`.xlsx`) + exported VBA (`.txt`) for counting, comparison, and DCS conversion |
| [`MSWord Macros/`](https://github.com/gasyoun/SanskritMacros/tree/master/MSWord%20Macros) | Word `Normal.dotm` template plus an exported VBA macro |
| [`RStudio Macros/`](https://github.com/gasyoun/SanskritMacros/tree/master/RStudio%20Macros) | R scripts for syllable/varga statistical analysis over DCS data |

## EmEditor Macros

### TransConverters — 27 transliteration/encoding converters

`.vbee` scripts (JavaScript-syntax EmEditor macros) that transform the current
document in place via chained `document.selection.Replace(...)` calls. Coverage
spans the scholarly transliteration schemes and several legacy font encodings:

- **Scheme-to-scheme:** SLP1 ↔ IAST ↔ Harvard-Kyoto
  ([`SLP1_IAST.vbee`](https://github.com/gasyoun/SanskritMacros/blob/master/EmEditor%20Macros/TransConverters/SLP1_IAST.vbee),
  [`IAST_SLP1.vbee`](https://github.com/gasyoun/SanskritMacros/blob/master/EmEditor%20Macros/TransConverters/IAST_SLP1.vbee),
  [`HK_SLP1.vbee`](https://github.com/gasyoun/SanskritMacros/blob/master/EmEditor%20Macros/TransConverters/HK_SLP1.vbee),
  [`SLP1_HK.vbee`](https://github.com/gasyoun/SanskritMacros/blob/master/EmEditor%20Macros/TransConverters/SLP1_HK.vbee),
  [`IAST2Harvard-Kyoto.vbee`](https://github.com/gasyoun/SanskritMacros/blob/master/EmEditor%20Macros/TransConverters/IAST2Harvard-Kyoto.vbee))
- **To/from Devanagari:** transliteration, Harvard-Kyoto, ITRANS, Velthuis, CSX,
  abbreviation source (`hk2dev`, `itrans2dev`, `velthius2dev`, `csx2dev`,
  `abbr2dev`, `trans2dev`, `dev2trans`)
- **Vedic accents / Unicode:** `vedic2accents`, `vedic2unicode`,
  `vedic2unicode-devavani`, `dvttvedic2trans`, `western2accents`,
  [`CologneAccents.vbee`](https://github.com/gasyoun/SanskritMacros/blob/master/EmEditor%20Macros/TransConverters/CologneAccents.vbee)
- **Legacy font decoders:** `xdvng2unicode`, `yogesh2unicode`,
  `sanskrit12-2-unicode`, `trans2sanskrit12`, `shreiner2dev`, `indolog2trans`,
  `ree2dev`

Provenance and contact history are in
[`readme.txt`](https://github.com/gasyoun/SanskritMacros/blob/master/EmEditor%20Macros/TransConverters/readme.txt)
(a Russian-language note dated 04.01.2013). EmEditor: <https://www.emeditor.com/>.

### ExtractTags

Three macros that pull the contents of a specific XML-style tag out of
dictionary export lines:
[`b-get.vbee`](https://github.com/gasyoun/SanskritMacros/blob/master/EmEditor%20Macros/ExtractTags/b-get.vbee),
[`key1-get.vbee`](https://github.com/gasyoun/SanskritMacros/blob/master/EmEditor%20Macros/ExtractTags/key1-get.vbee),
[`key2-get.vbee`](https://github.com/gasyoun/SanskritMacros/blob/master/EmEditor%20Macros/ExtractTags/key2-get.vbee)
(matching the `key1`/`key2` headword-key convention used across the lexicon
repos). These files are UTF-16 encoded.

## MSExcel Macros

- [`ABC_20-80/`](https://github.com/gasyoun/SanskritMacros/tree/master/MSExcel%20Macros/ABC_20-80) — headword/alphabet-frequency workbooks (`ABC_17_12_2013.xlsx`, `inventor_v1.2.xlsx`, `MW-ABC-by-hyphens.xlsx`)
- [`Arabic2Roman/`](https://github.com/gasyoun/SanskritMacros/tree/master/MSExcel%20Macros/Arabic2Roman) — Arabic-to-Roman numeral column conversion (`Arabic2RomanColumn.txt`)
- [`CountLigatures/`](https://github.com/gasyoun/SanskritMacros/tree/master/MSExcel%20Macros/CountLigatures) — `CountDevanagariLigatures_Rigveda.xlsm` and its exported VBA `Sub_CountDevanagariLigatures.txt` (counts conjunct consonants like `kh`, `gh`, `ch` …)
- [`DCS2DOC/`](https://github.com/gasyoun/SanskritMacros/tree/master/MSExcel%20Macros/DCS2DOC) — `DCS2DOC-converter.xlsm` plus a `60-books.rar` data bundle
- [`FuzzyTypoe/`](https://github.com/gasyoun/SanskritMacros/tree/master/MSExcel%20Macros/FuzzyTypoe) — list-comparison highlighter (`Sub_compare-lists-paint-green.txt`)
- [`SplitComposita/`](https://github.com/gasyoun/SanskritMacros/tree/master/MSExcel%20Macros/SplitComposita) — `countif-word-ends.xlsm`

The `.xlsm`/`.xlsx` files are binary and cannot be opened with a text editor;
the sibling `.txt` files are plain-text VBA exports of the same logic.

## MSWord Macros

- [`Normal.dotm`](https://github.com/gasyoun/SanskritMacros/blob/master/MSWord%20Macros/Normal.dotm) — Word global template carrying the macros
- [`Kruzhochki.txt`](https://github.com/gasyoun/SanskritMacros/blob/master/MSWord%20Macros/Kruzhochki.txt) — exported VBA `Sub Kruzhochki()` (find/replace formatting macro)

## RStudio Macros

R scripts that read the Digital Corpus of Sanskrit (DCS) syllable table and run
`varga` (place-of-articulation) statistics over it:
[`cccv.R`](https://github.com/gasyoun/SanskritMacros/blob/master/RStudio%20Macros/cccv.R),
[`SamskritaPropTest.r`](https://github.com/gasyoun/SanskritMacros/blob/master/RStudio%20Macros/SamskritaPropTest.r),
[`varga-comparison.R`](https://github.com/gasyoun/SanskritMacros/blob/master/RStudio%20Macros/varga-comparison.R).
They pull `syllables.dat` from the Heidelberg DCS host at runtime, so they
require network access and depend on that URL remaining reachable.

## Notes

- No LICENSE or CITATION file is present in this repository.
- CI is limited to Dependabot: weekly GitHub-Actions updates
  ([`dependabot.yml`](https://github.com/gasyoun/SanskritMacros/blob/master/.github/dependabot.yml))
  auto-merged by
  [`dependabot-auto-merge.yml`](https://github.com/gasyoun/SanskritMacros/blob/master/.github/workflows/dependabot-auto-merge.yml).
- File encodings are mixed — several EmEditor macros are UTF-16 and some VBA
  exports carry a UTF-8 BOM. Check before transforming any file in bulk.

_Dr. Mārcis Gasūns_

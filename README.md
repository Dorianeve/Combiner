# Combiner

Program that extracts CR, PR, DCM and AF combiners for ECW.

## Setup

-   Download the repo into your local system. Make sure you sync the entire ECW orange sharepoint GrantsDB locally: the combiner flow relies on the local accessibility of the database. It won't work if you only add the shortcut on your local drive. Make sure to have full rights (Owner rights) on the ECW orange sharepoint.

-   Create a `config.yml` file in the main folder with this structure and customize it according to your credentials.

    ``` yml
    # ECW orange sharepoint to be downloaded locally
    ecw_db = "C:/Users/XXXX/Education Cannot Wait/ECW Grants - Grants DB"

    # GMS
    username = "XXXXXX@unicef.org"
    password = "XXXXXX"

    # Yearly filtering variable (Combiner column)
    reporting_year = "2025"
    active_year = "Activein2025"
    old_active_year = "Activein2024"
    reporting_window_cutoffdate = "2026-01-01"
    old_exercise_cumulative = "ARR24 Cumulative"
    exercise_annual = "ARR25 Annual"
    exercise_cumulative = "ARR25 Cumulative"

    # Dynamic filter
    apply_mne_filter = FALSE # Default is TRUE / FALSE applicable if we want to skip approval (recommended for mid-term exports)

    # path to the old combiner file (recommended to put the file into # the working folder)
    old_cr_combiner = "data/arr/cr/XXXXX.csv"
    old_pr_combiner = "data/arr/pr/XXXXX.csv"

    # Reports tracker link GM (file urls and sheet names)
    report_tracker =  "C:/Users/XXXXXX/UNICEF/Education Cannot Wait - Grants Management/04 - Reports tracker/ECW Reports Tracker.xlsx"
    retired_report_tracker = "C:/Users/XXXXXXX/UNICEF/Education Cannot Wait - Grants Management/04 - Reports tracker/RETIRED - ECW Reports tracker.xlsx"
    grants_db_sheet = "Grants DB"
    results_report_sheet = "Results_Reports"

    # Reports approval MnE (file urls and sheet names)
    report_approval = "C:/Users/XXXXXX/UNICEF/Education Cannot Wait - Grants Management/04 - Reports tracker/ARR24 MnE Reports approval.xlsx"
    report_approval_sheet = "MnE reports approval"
    ```

## Combiner structure

### Load

All the load files for both ARR and Finance combiners work detecting the excel tables with specific names, within the excel workbooks. All load files iterate within the downloaded GrantsDB, looking for specific files, containing specific tables. The tables cleaning is performed working on list of data.frames, and it is merged into a bigger data.frame when all tables are correctly detected and headers consolidated (*running time approx. 20 mins for each flow).*

Each flow iterates over the GrantsDB and looks for different tables:

-   CR flow (`ecw_ar_children`, `ecw_reporting_windows`)

-   PR flow (`ecw_ar_results_framework`, `ecw_reporting_standard_indicators`, `ecw_reporting_windows`)

-   Finance flow (`ecw_leverage_funding`, `ecw_finance_dcm`, `ecw_finance_expenditures_overview`)

-   both flows load also the `gms` data from the API endpoint [`https://gms.educationcannotwait.org/api/grants-db/export`](https://gms.educationcannotwait.org/api/grants-db/export){.uri}, [`https://gms.educationcannotwait.org/api/allocations-db/export`](https://gms.educationcannotwait.org/api/allocations-db/export), [`https://gms.educationcannotwait.org/api/expenditures_refund-db/export`](https://gms.educationcannotwait.org/api/expenditures_refund-db/export){.uri}

With the current architecture, the "load" scripts are currently located into the `scripts/load/` (for CR and PR flows) and `scripts/finance/load/` (for finance) subfolders, as they do follow a similar logic.

### CR flow

After loading, the process continue with four different phases for the CR data. The CR reporting data is overwritten every year by the Grantees, this causes some consequences:

-   old data is taken from the previous year combiner, which is necessary to have cumulative reporting over the years;

-   data of inactive grants is still present in their latest report: this means that Active-in-current-year (like `Activein2024`) column becomes necessary for reporting.

Keeping this into account there are 4 scripts that transform the extracted data into the CR Combiner long-format, utilized by ECW over the years:

-   `01_cr_joins.R` - this script joins the CR extracted data with the GMS and Reporting Windows information;

-   `02_cr_processing_previous_combiner.R` - this script process last years data and creates the `Annual/ Cumulative` variable (this needs rethinking as there are some bits - like getting `ProgrammeID` that are not applicable anymore as the 2024 combiner has already the column);

-   `03_cr_pivoting.R` - this script pivot the combiner from the horizontal structure, to the vertical one. It binds the old processed combiner to the new one and it filters and saves the MnE version of the combiner.

-   (optional) `04_cr_export_subsets.R` - this script it to save two versions of the combiner when not all the grants are MnE approved and it outputs a version for Grants Management and a version with MnE "tentative" approved. It required rethinking as the report tracker was discontinued.

The final outputs (`csv`and `xlsx`) are in the folder `data/cr/cleaned/`. THe `csv` is names with the pricessing date, instear the `xlsx` is saves as `cr_cleaned.xlsx` for its utilization in the **ARR checks workflow**. The sub-outputs of each processing steps are in the folder `data/arr/cr`.

### PR flow

The PR reporting data is not straightforward. THere are three templates that have been utilized over the years: Programme Results, Standard Indicators and Results Template. They need to be harmonized before joining, and this is done with an external correspondence table `data/arr/pr/utilities/pr_columns.csv`. Due to high number of columns also, their order is hadled by an external table `data/arr/pr/utilities/pr_columns_order.csv`. Also, different from the CR, PR data is not overwritten every year. Grantees report in different reporting windows (RWs). The consequence of this is:

-   data can accidentally be overwritten over the years;

-   data of inactive grants is still present in their latest report: this means that Active-in-current-year (like `Activein2024`) column becomes necessary for reporting.

Also, for the consolidation of the old combiner names and the new ones, this is the correspondence table `data/arr/pr/utilities/pr_old_new_names.csv`.

The CR tweaks flow consists of:

-   `pr_processing_previous_combiner.R` - this script processes the previous year combiner;

-   `pr_merge.R` - this script merges the three templates (standard indicators, programme results and results template) between each other and the complete PR with the GMS;

-   `pr_pivoting.R` - This script pivots the PR in a longer format (the PR pivoting is quite different as the data structure is not straightforward);

-   `pr_cleaning_outputting.R` - This final script does some additional cleaning and saves the `xlsx` format.

The final outputs (`csv`and `xlsx`) are in the folder `data/pr/cleaned/`. The `csv` is names with the processing date, instear the `xlsx` is saves as `pr_cleaned.xlsx` for its utilization in the **ARR checks workflow**. The sub-outputs of each processing steps are in the folder `data/arr/pr`.

### Finance combiner flow (AF and DCM)

After the loading phase, the AF and DCM combiner, they only consist of one script each, where the extracted data is joined with the necessary other data (GMS, Reporting Windows).

The output of the finance combiners is saved info `data/finance combiner/` folder, respectively in `aligned funding` and `dcm`.

## Combiner functioning

The scripts are organized in modules, meaning that each combiner function as a standalone and it is sufficient to run one single script per combiner in the main folder:

-   `cr_flow.R`

-   `pr_flow.R`

-   `af_flow.R`

-   `dcm_flow.R`

# Paper-Reproduction-using-R

I reproduced the important figures and tables in Jorda, Knoll, Kuvshinov, Schularick & Taylor (2019), "The Rate of Return on Everything, 1870-2015,Quarterly Journal of Economics.

# Data

The raw datasets are not included in this repository. The official data and replication files for Jordà, Knoll, Kuvshinov, Schularick, and Taylor (2019), *The Rate of Return on Everything, 1870–2015*, are publicly available from [Moritz Schularick’s Datasets and Codes page](https://www.moritzschularick.com/academic/datasets-and-codes/).

On that page, locate **The Rate of Return on Everything** and click **Data and Replication Files**. After downloading and extracting the replication archive, create a folder named `data_raw` in the project root and place the following files inside it:

```text
data_raw/
├── rore_public_main.dta
└── rore_public_supplement.dta
```

The R scripts read these files using relative paths, so no changes to the file paths are required when the project is opened from its RStudio project file.

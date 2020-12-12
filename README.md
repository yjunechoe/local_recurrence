# Directory Structure

<pre>
root
|   .gitignore
|   analysis.R
|   make_moving_window.R
|   PbV_sims.R
|   processing.R
|
+---data
|   |   concreteness.txt
|   |   item_trajectory_table.csv
|   |   keys.csv
|   |   subtlex-us.zip
|   |
|   \---tokens_data
|       <...>
|
\---img
    |   cookie_windows.png
    |   ex_corpus_stats.png
    |   ex_nouns_stats.png
    |   freq_trajectory_plot1.png
    |   freq_trajectory_plot2.png
    |   non_repetition_trajectory_plot.png
    |   PbV_repetitions.png
    |   providence_corpus_stats.png
    |   repetition_trajectory_plot.png
</pre>

# Scripts, and how to run them

- `processing.R` is a processing script that grabs data from the CHILDES database and formats it for use in the `make_moving_window.R` script. The result of this script is provided in `data/tokens_data`, so this does not need to be ran.

- `make_moving_window.R` runs the moving window algorithm using the data in `data/tokens_data`. It outputs some images to `img/` and generates a file `data/moving_windows/moving_windows_5.rds` by default (not provided). The `5` in the name reflects the size of the moving window, which updates automatically if you provide a different number for the size of the moving window in the script. 

- `processing.R` is the main script. It takes the moving window data generated from `make_moving_window.R` and runs some analyses, mainly correlations. Figures are saved to `img/`

- `PbV_sims.R` is a standalone script that runs simulations on four variants of Propose but Verify. To run this, make sure to also download `gold_training_parsed.csv` and `rollins_training.rds` from [this other repo](https://github.com/yjunechoe/Ling570_pset1) and put them in the root


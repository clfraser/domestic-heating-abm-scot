
# Import gzip to unzip the jsonl.gz file
import gzip
import sys

# Import funcitions from abm.py script
from abm import (read_jsonlines, history_to_dataframes)

# Import package to write to feather (a format that allows interoperability between R and Python)
import pyarrow.feather as feather

# Set filename where JSON results file is stored
filename = "./influence_importance_lhs_" + sys.argv[1] + "_" + sys.argv[2] + ".jsonl.gz"

# Open file and use the read_jsonlines function defined in abm.py to read the JSON file
#with open(filename, "r") as file:
 #       history = list(read_jsonlines(file))

with gzip.open(filename, mode="r") as file:
    history = list(read_jsonlines(file))

# Use the history_to_dataframes function defined in abm.py to convert the history to dataframes
agent_history_df, model_history_df = history_to_dataframes(history)

# Save both dataframes as feather format, to read into R
feather.write_feather(agent_history_df, "./influence_importance_lhs_" + sys.argv[1] + "_" + sys.argv[2] + "_agent_history.feather")
feather.write_feather(model_history_df, "./influence_importance_lhs_" + sys.argv[1] + "_" + sys.argv[2] + "_model_history.feather")

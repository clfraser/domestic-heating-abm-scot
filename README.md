# `domestic-heating-abm-scot`
An adaptation of Centre for Net Zero's agent-based model of heat pump adoption. Details of the original model are given below.
One adapted model is on each branch of the repo.

Main: Centre for Net Zero model adapted for Scottish data. Some constants changed to Scottish values.

Green attitudes: If agents have "green" attitudes (derived from the survey data below), they are more likely to adopt a heat pump.

Social network awareness: Awareness of heat pump spreads through social diffusion

Social network HP: Agents are more likely to adopt a heat pump if their neighbours adopt one

No potential EPC: A sensitivity test, which tested whether excluding the influence of the potential EPC variable affected results

No energy efficiency: Similar to no potential EPC, but setting all of the energy efficiency of windows, roofs and doors to one, to explore results

The Centre for Net Zero instructions below contain links to obtain and process the English and Welsh data.
The Scottish data is from the 2019 Scottish Health Survey and Scottish House Condition Survey, available through the UK Data Archive: https://www.data-archive.ac.uk/

Data analysis is done in R, using the files in analysis. There is a common 'functions' file that is imported into most other files.

## Centre for Net Zero instructions

[Centre for Net Zero's](https://www.centrefornetzero.org/) agent-based model (ABM) for the electrification of domestic heating in England and Wales.

To find out more about our approach and results read our report [_Hitting the Target: interventions required to meet UK Government heat pump targets_](https://www.centrefornetzero.org/res/hitting-the-target/).

## Installation

You need Python 3.9 and [`pipenv`](https://github.com/pypa/pipenv).
If you don't have them, see [our instructions for macOS](https://gist.github.com/tomwphillips/715d4fd452ef5d52b4708c0fc5d4f30f).

1. Clone this repo.
2. `pipenv sync --dev` to install dependencies.
3. `cp .env.template .env`
4. `pipenv run pytest`

## Running the simulation

You configure and run the simulation using a command line interface.

Print the help message to see all the options:

```
python -m simulation -h
```

The simulation initialises household agents using data from a Parquet file or BigQuery query.
The simulation history is written to a file with one JSON object per line.

For more details on the datasets we combine to generate the household data and the final schema, visit [`centrefornetzero/domestic-heating-data`](https://github.com/centrefornetzero/domestic-heating-data).

### Examples

Parquet input:

```
python -m simulation households.parquet history.jsonl
```

BigQuery:

```
python -m simulation --bigquery "select * from project.prod_domestic_heating.dim_household_agents" history.jsonl
```

## Analysing the results

We collect data from the environment and agents at each timestep of the simulation and write it as a newline-delimited JSON-encoded object in the history file.

You can use [`read_jsonlines`](https://github.com/centrefornetzero/domestic-heating-abm/blob/1eabe653c19f93f831d6b72cce6249515c42030d/abm.py#L130) to read the history file and [`history_to_dataframes`](https://github.com/centrefornetzero/domestic-heating-abm/blob/1eabe653c19f93f831d6b72cce6249515c42030d/abm.py#L135) to convert it to pandas DataFrames.

## Running simulation jobs on Kubernetes

We run the simulation with different configurations, called scenarios, to see how interventions affect the choices households make about their heating systems.
We also run sensitivity tests to understand how changing each parameter affects the results.
Since the simulation is probabilistic, we run each of these configurations multiple times and compute the average outcome.

Scenarios and sensitivity tests are defined in [`k8s/job.jsonnet`](k8s/job.jsonnet).
We run all the scenarios and sensitivity tests for every commit on the `main` branch.
A [Github Action](.github/workflows/container.yaml) uses `job.jsonnet` to generate a Kubernetes job configuration and applies it to a Google Kubernetes Engine Autopilot cluster.
We monitor the jobs via a GCP Monitoring dashboard.
After the jobs complete we download the history files from Google Cloud Storage.

## ABM API design

We acknowledge [Agents.jl](https://github.com/JuliaDynamics/Agents.jl), whose API design inspiried us for [`abm.py`](abm.py).

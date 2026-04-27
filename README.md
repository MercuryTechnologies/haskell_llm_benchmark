# Haskell LLM Benchmark

This is a test harness to evaluate LLM models on their ability to consistently follow instructions to successfully edit Haskell code. 

It is a modified version of the [Aider benchmark harness](https://github.com/Aider-AI/aider/blob/main/benchmark/README.md) adapted to include a Haskell environment.

The benchmark is based on [Exercism's Haskell exercises](https://exercism.org/tracks/haskell) ([Github](https://github.com/exercism/haskell)). This benchmark evaluates how effectively a coding assistant and LLMs can translate a natural language coding request into executable code saved into files that pass unit tests. It provides an end-to-end evaluation of not just the LLM's coding ability, but also its capacity to edit existing code and format those code edits so that aider can save the edits to the local source files.

_Last updated: 2026-04-27_

![Haskell LLM Benchmark](/benchmark-result/report-2026-04-27-17-01-20/benchmark_comparison.png)

| Model | Tests | Pass % | Pass 1st Try % | Tests Passed | Passes 1st Try | Well Formed % | Errors | Sec/Test | Total Cost ($) | Cost/Test ($) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| gpt-5.5-2026-04-23 (effort=xhigh) | 112 | 95.5 | 92.0 | 107 | 103 | 100.0 | 0 | 66.9 | 13.10 | 0.1170 |
| gpt-5.4 (effort=xhigh) | 112 | 94.6 | 89.3 | 106 | 100 | 100.0 | 1 | 251.1 | 18.30 | 0.1634 |
| gemini-3.1-pro-preview | 112 | 92.0 | 80.4 | 103 | 90 | 99.1 | 3 | 85.1 | 10.42 | 0.0930 |
| claude-opus-4-7 (thinking) | 112 | 91.1 | 75.0 | 102 | 84 | 100.0 | 0 | 14.4 | 5.30 | 0.0473 |
| claude-opus-4-6 (thinking) | 112 | 90.2 | 83.0 | 101 | 93 | 100.0 | 0 | 26.6 | 6.11 | 0.0545 |
| gpt-5-pro | 112 | 90.2 | 83.0 | 101 | 93 | 100.0 | 29 | 733.3 | 75.16 | 0.6710 |
| gpt-5-high | 112 | 90.2 | 82.1 | 101 | 92 | 100.0 | 0 | 117.2 | 6.93 | 0.0619 |
| gemini-3-pro-preview | 112 | 90.2 | 80.4 | 101 | 90 | 100.0 | 4 | 105.1 | 0.00 | 0.0000 |
| claude-opus-4-5 | 112 | 90.2 | 76.8 | 101 | 86 | 100.0 | 0 | 14.9 | 0.00 | 0.0000 |
| o3-high | 112 | 88.4 | 73.2 | 99 | 82 | 100.0 | 0 | 51.7 | 19.05 | 0.1701 |
| claude-opus-4-6 | 112 | 85.7 | 73.2 | 96 | 82 | 100.0 | 0 | 14.2 | 3.98 | 0.0355 |
| gpt-5.4 | 112 | 85.7 | 63.4 | 96 | 71 | 100.0 | 0 | 10.4 | 1.82 | 0.0163 |






> **Note:** Past model runs are available in git history but removed from this main table for clarity as the SOTA frontier changes. Cached summary metrics for older runs are stored in `benchmark/cached_results.json` and can be included with `--include-cached`.

___

## Instructions

Can generally follow the instructions in the [Aider benchmark harness](https://github.com/Aider-AI/aider/blob/main/benchmark/README.md); with the following exceptions:

- clone this repo
- exercises are included in the `tmp.benchmarks` directory, no need to clone the exercises (although you are welcome to contribute new ones)

On my macOS machine, running the benchmark in Docker would consistently fail with some heap corruption error ([issue](https://github.com/Aider-AI/aider/issues/3718)). A nix environment is provided although you probably want to run this in a safe environment like a VM (the benchmark runs code produced by an LLM so it's important to run it in an isolated environment).

Once you have a cloned repo:

```sh
nix-develop

# set your API keys (alternatively, you can set the keys in .envrc if using direnv (nix env has it set up))
export OPENAI_API_KEY=sk-proj-...
export ANTHROPIC_API_KEY=...
export GEMINI_API_KEY=...

# run the benchmark (try a single exercise first)
./benchmark/benchmark.py o3-mini-run --model o3-mini --edit-format whole --threads 10 --num-tests 1 --exercises-dir polyglot-benchmark --new

./benchmark/benchmark.py o3-mini-full-run --model o3-mini --edit-format whole --threads 10 --exercises-dir polyglot-benchmark --new

# for sonnet thinking
./benchmark/benchmark.py claude-3-7-thinking-full-run-final --model anthropic/claude-3-7-sonnet-20250219 --edit-format whole --threads 5 --exercises-dir polyglot-benchmark --new --read-model-settings .aider.model.settings.yml
```

You need to be mindful of the API limits of the model you are using. For high volume APIs (e.g. OpenAI), I've had success using `20` threads. For Anthropic, I've had success using `5` threads, etc...

Reference for model providers and models: https://aider.chat/docs/llms.html

### Generating Reports

After running benchmarks for one or more models, you can generate comparison reports with:

```sh
# Generate reports for all benchmarks (automatically uses all folders in tmp.benchmarks except polyglot-benchmark)
./benchmark/summarize_benchmark.py

# Generate reports for specific benchmark directories
./benchmark/summarize_benchmark.py path/to/dir1 path/to/dir2

# Specify custom output paths
./benchmark/summarize_benchmark.py --table-output custom_table.csv --plot-output custom_plot.png
```

The report generator will:
- Extract key metrics from all benchmark results
- Format model names for better readability
- Sort models by pass rate
- Generate a formatted table in both CSV and Markdown formats
- Create a visual comparison chart showing pass rates and costs
- Save results in a timestamped directory under benchmark-result/

___ 

### Updating to latest aider version

```sh
git fetch upstream
git merge upstream/main
```

# Haskell LLM Benchmark

This is a test harness to evaluate LLM models on their ability to consistently follow instructions to succesfully edit Haskell code. 

It is a modified version of the [Aider benchmark harness](https://github.com/Aider-AI/aider/blob/main/benchmark/README.md) adapted to include a Haskell environment.

The benchmark is based on [Exercism's Haskell exercises](https://exercism.org/tracks/haskell) ([Github](https://github.com/exercism/haskell)). This benchmark evaluates how effectively a coding assistant and LLMs can translate a natural language coding request into executable code saved into files that pass unit tests. It provides an end-to-end evaluation of not just the LLM's coding ability, but also its capacity to edit existing code and format those code edits so that aider can save the edits to the local source files.

_Last updated: 2025-04-10_

![Haskell LLM Benchmark](/benchmark-result/report-2025-04-10-14-40-40/benchmark_comparison.png)

| Model | Tests | Pass % | Pass 1st Try % | Tests Passed | Passes 1st Try | Well Formed % | Errors | Sec/Test | Total Cost ($) | Cost/Test ($) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| o1-pro | 112 | 82.1 | 72.3 | 92 | 81 | 99.1 | 1 | 301.6 | 275.04 | 2.4558 |
| o1 | 112 | 79.5 | 67.9 | 89 | 76 | 99.1 | 1 | 49.3 | 29.22 | 0.2609 |
| claude-3-7-sonnet-20250219 (thinking) | 112 | 77.7 | 67.9 | 87 | 76 | 99.1 | 2 | 79.5 | 12.55 | 0.1120 |
| gemini-2.5-pro-exp-03-25 | 112 | 75.0 | 65.2 | 84 | 73 | 97.3 | 3 | 40.9 | 0.00 | 0.0000 |
| o3-mini | 112 | 75.0 | 63.4 | 84 | 71 | 100.0 | 0 | 37.5 | 2.13 | 0.0190 |
| optimus-alpha | 112 | 73.2 | 57.1 | 82 | 64 | 100.0 | 0 | 8.4 | 0.00 | 0.0000 |
| quasar-alpha | 112 | 69.6 | 54.5 | 78 | 61 | 98.2 | 2 | 7.4 | 0.00 | 0.0000 |
| claude-3-7-sonnet-20250219 | 112 | 66.1 | 55.4 | 74 | 62 | 99.1 | 1 | 15.9 | 3.80 | 0.0340 |
| gpt-4o | 112 | 57.1 | 48.2 | 64 | 54 | 100.0 | 0 | 12.4 | 1.60 | 0.0143 |
| deepseek-chat-v3-0324 | 112 | 56.2 | 42.0 | 63 | 47 | 100.0 | 0 | 59.2 | 0.41 | 0.0037 |
| llama-4-maverick | 112 | 46.4 | 32.1 | 52 | 36 | 91.1 | 10 | 16.0 | 0.00 | 0.0000 |
| gemini-2.0-flash | 112 | 45.5 | 37.5 | 51 | 42 | 97.3 | 3 | 4.9 | 0.08 | 0.0007 |
| gpt-4o-mini | 112 | 34.8 | 23.2 | 39 | 26 | 100.0 | 0 | 13.4 | 0.11 | 0.0010 |




___

## Instructions

Can generally follow the instructions in the [Aider benchmark harness](https://github.com/Aider-AI/aider/blob/main/benchmark/README.md); with the following exceptions:

- clone this repo
- exercises are included in the `tmp.benchmarks` directory, no need to clone the exercises (although you are welcome to contribute new ones)

On my macOS machine, running the benchmark in Docker would consistently fail with some heap corruption error ([issue](https://github.com/Aider-AI/aider/issues/3718)). A nix environment is provided although you probably want to run this in a safe environment like a VM (the benchmark runs code produced by an LLM so it's important to run it in an isolated environment).

Once you have a cloned repo:

```sh
nix-develop

# set your API keys
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

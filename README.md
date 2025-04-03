## Haskell LLM Benchmark

This is a test harness to evaluate LLM models on their ability to consistently follow instructions to succesfully edit Haskell code. 

It is a modified version of the [Aider benchmark harness](https://github.com/Aider-AI/aider/blob/main/benchmark/README.md) adapted to include a Haskell environment.

The benchmark is based on [Exercism's Haskell exercises](https://exercism.org/tracks/haskell) ([Github](https://github.com/exercism/haskell)). This benchmark evaluates how effectively a coding assistant and LLMs can translate a natural language coding request into executable code saved into files that pass unit tests. It provides an end-to-end evaluation of not just the LLM's coding ability, but also its capacity to edit existing code and format those code edits so that aider can save the edits to the local source files.



### Instructions

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

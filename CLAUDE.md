# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Run Commands
- Use `nix develop` to set up the correct environment before running any commands
- Install dependencies (if not using nix): `pip install -e .[dev]`
- Run benchmarks: `./benchmark/benchmark.py <run-name> --model <model-name> --edit-format whole --threads <num-threads> --exercises-dir polyglot-benchmark --new`
- Generate reports: `./benchmark/summarize_results.py` or `./benchmark/summarize_haskell.py`
- Test script: Run `python3 -m pytest tests/path/to/test.py::TestClass::test_name`

## Environment Requirements
- Python 3.9-3.12
- Required packages: pandas, matplotlib, numpy, PyYAML
- Nix environment is preferred for consistent dependencies and isolated execution
- Set required API keys: `export ANTHROPIC_API_KEY=...` (and others as needed)

## Code Style Guidelines
- Follow PEP 8 conventions (79-80 character line length)
- Add type hints for function parameters and return values
- Import order: standard library, third-party packages, local modules
- Use docstrings for functions, classes, and modules
- Handle errors with specific exceptions and appropriate messages
- Use pathlib.Path for file path handling
- Use context managers (with) for file I/O

Always run any new or modified Python scripts with required packages installed before committing changes. For benchmark scripts, always use `nix develop` to ensure a consistent environment.
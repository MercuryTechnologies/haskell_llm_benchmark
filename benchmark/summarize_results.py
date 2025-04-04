#!/usr/bin/env python3
import os
import sys
import json
import re
import subprocess
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from pathlib import Path
from types import SimpleNamespace
from collections import defaultdict
import numpy as np
import yaml
import textwrap
from typing import List, Optional, Dict, Any

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Define necessary functions from benchmark.py
def load_results(dirname, stats_languages=None):
    dirname = Path(dirname)
    all_results = []
    
    if stats_languages:
        languages = [lang.strip().lower() for lang in stats_languages.split(",")]
        glob_patterns = [f"{lang}/exercises/practice/*/.aider.results.json" for lang in languages]
    else:
        glob_patterns = ["*/exercises/practice/*/.aider.results.json"]
    
    for pattern in glob_patterns:
        for fname in dirname.glob(pattern):
            try:
                results = json.loads(fname.read_text())
                all_results.append(results)
            except json.JSONDecodeError:
                print("json.JSONDecodeError", fname)
                continue
    return all_results

def get_versions(commit_hashes):
    versions = set()
    for hsh in commit_hashes:
        if not hsh:
            continue
        hsh = hsh.split("-")[0]
        try:
            version = subprocess.check_output(
                ["git", "show", f"{hsh}:aider/__init__.py"], universal_newlines=True
            )
            version = re.search(r'__version__ = "(.*)"', version).group(1)
            versions.add(version)
        except subprocess.CalledProcessError:
            pass
    return versions

def summarize_results(dirname, stats_languages=None):
    all_results = load_results(dirname, stats_languages)

    res = SimpleNamespace()
    res.total_tests = len(list(Path(dirname).glob("*/exercises/practice/*")))

    try:
        tries = max(len(results.get("tests_outcomes", [])) for results in all_results if results)
    except ValueError:
        tries = 0

    res.dir_name = str(dirname)

    passed_tests = [0] * tries

    res.completed_tests = 0
    res.duration = 0
    res.cost = 0
    res.error_outputs = 0
    res.user_asks = 0
    res.test_timeouts = 0
    res.exhausted_context_windows = 0
    res.num_malformed_responses = 0
    res.num_with_malformed_responses = 0
    res.syntax_errors = 0
    res.indentation_errors = 0
    res.lazy_comments = 0

    variants = defaultdict(set)

    for results in all_results:
        if not results:
            continue

        res.completed_tests += 1
        tests_outcomes = results.get("tests_outcomes", [])
        passed = tests_outcomes and tests_outcomes[-1]
        if passed:
            for i in range(len(tests_outcomes) - 1, tries):
                passed_tests[i] += 1

        res.cost += results.get("cost", 0)
        res.duration += results.get("duration", 0)
        res.test_timeouts += results.get("test_timeouts", 0)

        res.error_outputs += results.get("num_error_outputs", 0)
        res.user_asks += results.get("num_user_asks", 0)
        res.exhausted_context_windows += results.get("num_exhausted_context_windows", 0)
        res.num_malformed_responses += results.get("num_malformed_responses", 0)
        if results.get("num_malformed_responses"):
            res.num_with_malformed_responses += 1
        res.lazy_comments += results.get("lazy_comments", 0)

        res.syntax_errors += results.get("syntax_errors", 0)
        res.indentation_errors += results.get("indentation_errors", 0)

        for key in "model edit_format commit_hash editor_model editor_edit_format".split():
            val = results.get(key)
            if val:
                variants[key].add(val)

    if not res.completed_tests:
        return None

    # Silent mode - no console output
    percents = dict()
    for i in range(tries):
        pass_rate = 100 * passed_tests[i] / res.completed_tests
        percents[i] = pass_rate
        setattr(res, f"pass_rate_{i + 1}", pass_rate)
        setattr(res, f"pass_num_{i + 1}", passed_tests[i])

    # Set other attributes directly
    pct_well_formed = 0
    if res.completed_tests > 0:
        pct_well_formed = 100 * (1.0 - res.num_with_malformed_responses / res.completed_tests)
    res.percent_cases_well_formed = pct_well_formed

    # Add variants to res object
    for key, val in variants.items():
        val_str = ", ".join(map(str, val))
        setattr(res, key, val_str)

    res.avg_duration = res.duration / res.completed_tests if res.completed_tests else 0
    res.avg_cost = res.cost / res.completed_tests if res.completed_tests else 0

    return res

def generate_summary(dirnames: List[str]) -> pd.DataFrame:
    """
    Generate a summary DataFrame from multiple benchmark directories.
    
    Args:
        dirnames: List of directory paths to process
        
    Returns:
        DataFrame with summarized benchmark results
    """
    raw_rows = []
    
    for dirname in dirnames:
        row = summarize_results(dirname)
        if row:
            # Convert SimpleNamespace to dict and append to rows
            raw_rows.append(vars(row))
    
    # Create DataFrame from collected results
    if not raw_rows:
        print("No valid result data found in the specified directories.")
        return None
        
    df = pd.DataFrame.from_records(raw_rows)
    
    # Select and rename columns for clarity
    if 'model' in df.columns:
        select_cols = [
            'model', 'completed_tests', 'pass_rate_1', 'pass_rate_2', 
            'pass_num_1', 'pass_num_2', 'percent_cases_well_formed', 
            'error_outputs', 'avg_duration', 'cost', 'avg_cost'
        ]
        df = df[[col for col in select_cols if col in df.columns]]
        
        # Rename columns for better readability
        rename_map = {
            'model': 'Model',
            'completed_tests': 'Tests Completed',
            'pass_rate_1': 'Pass Rate 1 (%)',
            'pass_rate_2': 'Pass Rate (%)',
            'pass_num_1': 'Passes 1st Try',
            'pass_num_2': 'Passes Total',
            'percent_cases_well_formed': 'Well Formed (%)',
            'error_outputs': 'Error Outputs',
            'avg_duration': 'Time/Test (s)',
            'cost': 'Total Cost ($)',
            'avg_cost': 'Cost/Test ($)'
        }
        df = df.rename(columns={k: v for k, v in rename_map.items() if k in df.columns})
    
    return df

def generate_table(df: pd.DataFrame, output_path: str = None) -> str:
    """
    Generate a formatted table of results
    
    Args:
        df: DataFrame with benchmark results
        output_path: Optional path to save table as CSV
        
    Returns:
        Table as formatted string
    """
    # Make sure numeric columns are numeric
    numeric_cols = ['Pass Rate 1 (%)', 'Pass Rate (%)', 'Well Formed (%)', 'Time/Test (s)', 
                   'Total Cost ($)', 'Cost/Test ($)']
    for col in numeric_cols:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
    
    # Format the table
    table_df = df.copy()
    float_format = lambda x: f"{x:.2f}" if pd.notnull(x) else "N/A"
    
    for col in table_df.columns:
        if col in ['Pass Rate 1 (%)', 'Pass Rate (%)', 'Well Formed (%)']:
            # Format percentages with 1 decimal point
            table_df[col] = table_df[col].apply(lambda x: f"{float(x):.1f}" if pd.notnull(x) else "N/A")
        elif col in ['Time/Test (s)', 'Total Cost ($)', 'Cost/Test ($)']:
            # Format floats with 2 decimal points
            table_df[col] = table_df[col].apply(float_format)
    
    # Pretty print the table
    table_str = table_df.to_string(index=False)
    print("\nResults Summary Table:")
    print(table_str)
    
    # Save table as CSV if output path is provided
    if output_path:
        table_df.to_csv(output_path, index=False)
        print(f"\nSaved table to {output_path}")
        
        # Create a manual markdown table
        md_path = str(output_path).replace('.csv', '.md')
        
        # Generate header
        headers = list(table_df.columns)
        md_table = "| " + " | ".join(headers) + " |\n"
        md_table += "| " + " | ".join(["---" for _ in headers]) + " |\n"
        
        # Generate rows
        for _, row in table_df.iterrows():
            md_table += "| " + " | ".join([str(row[col]) for col in headers]) + " |\n"
        
        with open(md_path, 'w') as f:
            f.write(md_table)
        print(f"Saved markdown table to {md_path}")
        
    # Also return as YAML
    rows = []
    for _, row in df.iterrows():
        row_dict = row.to_dict()
        # Clean up numeric values for YAML
        for k, v in row_dict.items():
            if isinstance(v, (float, np.float64)):
                row_dict[k] = float(v)
        rows.append(row_dict)
        
    yaml_str = yaml.dump(rows, sort_keys=False, default_flow_style=False)
    
    return yaml_str

def plot_benchmark_comparison(df: pd.DataFrame, output_path: str = None) -> None:
    """
    Generate a beautiful bar chart comparing pass_rate_2 with a secondary axis for cost
    
    Args:
        df: DataFrame with benchmark results
        output_path: Path to save the plot image
    """
    # Make sure required columns exist
    required_cols = ['Model', 'Pass Rate (%)', 'Total Cost ($)']
    if not all(col in df.columns for col in required_cols):
        print(f"Missing required columns for plotting. Need: {required_cols}")
        return
    
    # Sort by pass rate (descending)
    df = df.sort_values(by='Pass Rate (%)', ascending=False)
    
    # Set up the plot with a larger figure and styling
    plt.style.use('seaborn-v0_8-whitegrid')
    plt.rcParams['font.family'] = 'Arial'
    plt.rcParams['font.weight'] = 'bold'
    plt.rcParams['axes.labelweight'] = 'bold'
    plt.rcParams['axes.titleweight'] = 'bold'
    plt.rcParams['figure.facecolor'] = 'white'
    
    # Create a larger figure with better aspect ratio
    fig, ax1 = plt.subplots(figsize=(16, 10))
    
    # Color palette - using a more visually appealing gradient
    bar_color = '#0066cc'  # Rich blue
    bar_edge_color = '#003366'  # Darker blue for edges
    cost_color = '#cc3300'  # Deep red for cost
    
    # Add a gradient background
    ax1.set_facecolor('#f8f9fa')
    fig.patch.set_facecolor('white')
    
    # Bar width and spacing
    bar_width = 0.7
    
    # Plot bars for pass rate with gradient effect
    bars = ax1.bar(
        df['Model'], 
        df['Pass Rate (%)'], 
        width=bar_width, 
        color=bar_color, 
        label='Pass Rate (%)',
        edgecolor=bar_edge_color,
        linewidth=1.5,
        alpha=0.85,
        zorder=3
    )
    
    # Add value labels on top of bars with larger font
    for bar in bars:
        height = bar.get_height()
        ax1.text(
            bar.get_x() + bar.get_width() / 2.,
            height + 0.8,
            f'{height:.1f}%',
            ha='center', 
            va='bottom',
            fontsize=18,  # Increased font size
            fontweight='bold',
            color='#000000',
            bbox=dict(facecolor='white', alpha=0.7, edgecolor='none', boxstyle='round,pad=0.2')
        )
    
    # Set up the primary y-axis with more padding
    max_pass_rate = max(df['Pass Rate (%)'])
    ax1.set_ylim(0, max_pass_rate * 1.25)  # 25% headroom
    ax1.set_ylabel('Pass Rate (%)', fontsize=18, fontweight='bold', labelpad=15)
    ax1.set_xlabel('Model', fontsize=18, fontweight='bold', labelpad=15)
    
    # Style the ticks and grid
    ax1.tick_params(axis='y', labelcolor='black', labelsize=14, width=2)
    ax1.tick_params(axis='x', labelsize=14, width=2, length=6, pad=8)
    ax1.grid(axis='y', linestyle='--', alpha=0.4, color='gray')
    
    # Style the spines
    for spine in ax1.spines.values():
        spine.set_color('#444444')
        spine.set_linewidth(1.5)
    
    # Set up secondary y-axis for cost
    ax2 = ax1.twinx()
    
    # Plot cost as points with a more distinctive marker
    scatter = ax2.scatter(
        df['Model'],
        df['Total Cost ($)'],
        color=cost_color,
        s=200,  # Larger points
        marker='D',  # Diamond marker
        label='Total Cost ($)',
        zorder=5,
        alpha=1.0,
        edgecolor='white',
        linewidth=1.5
    )
    
    # Add value labels for cost points in the top right corner of each point
    for i, cost in enumerate(df['Total Cost ($)']):
        # Position price labels to the top right of each point
        x_offset = 10  # Position to the right
        y_offset = 10  # Position to the top
            
        # Add a background box to make text stand out
        ax2.annotate(
            f'${cost:.2f}',
            (i, cost),
            xytext=(x_offset, y_offset),
            textcoords='offset points',
            ha='left',  # Align text to left (starting at the point)
            va='bottom',  # Align text to bottom (appearing above the point)
            fontsize=14,
            fontweight='bold',
            color=cost_color,
            bbox=dict(facecolor='white', alpha=0.8, edgecolor=cost_color, boxstyle='round,pad=0.3', linewidth=1)
        )
    
    # Style the secondary y-axis
    ax2.set_ylabel('Total Cost ($)', color=cost_color, fontsize=18, fontweight='bold', labelpad=15)
    ax2.tick_params(axis='y', labelcolor=cost_color, labelsize=14, width=2)
    
    # Set the y-limit with extra padding for cost labels
    ax2.set_ylim(0, max(df['Total Cost ($)']) * 1.35)  # 35% headroom
    
    # Add a striking title
    plt.suptitle('Haskell LLM Benchmark', 
              fontsize=26, fontweight='bold', y=0.98)
    
    # Add a subtitle with more detail
    plt.title('Model Performance: Pass Rate vs. Cost', 
              fontsize=18, pad=20, color='#444444')
    
    # Add a footer with date and attribution
    from datetime import date
    today = date.today().strftime("%B %d, %Y")
    plt.figtext(0.5, 0.01, f"Generated on {today} | Haskell LLM Benchmark", 
                ha="center", fontsize=12, fontweight='bold', color='#666666')
    
    # Create a more distinctive legend with better styling
    lines1, labels1 = ax1.get_legend_handles_labels()
    lines2, labels2 = ax2.get_legend_handles_labels()
    legend = ax1.legend(lines1 + lines2, labels1 + labels2, 
                loc='upper right', frameon=True, framealpha=0.95, 
                edgecolor='#444444', fontsize=14)
    legend.get_frame().set_facecolor('white')
    legend.get_frame().set_linewidth(2)
    
    # Rotate and wrap x-axis labels for better readability of long model names
    plt.xticks(rotation=60, ha='right', fontsize=12)
    # Get the current tick labels
    labels = plt.gca().get_xticklabels()
    plt.setp(labels, rotation=60, ha='right')
    
    # Wrap text for long model names with narrower width and adjust position
    for label in labels:
        text = label.get_text()
        # Wrap at a narrower width for better readability
        wrapped_text = '\n'.join(textwrap.wrap(text, width=12))
        label.set_text(wrapped_text)
        # Add more padding to avoid overlapping
        label.set_y(label.get_position()[1] - 0.05)
    
    # No gridlines at all
    ax1.grid(False)
    ax1.xaxis.grid(False)
    ax1.yaxis.grid(False)
    
    # Also remove gridlines from secondary y-axis
    ax2.grid(False)
    ax2.xaxis.grid(False)
    ax2.yaxis.grid(False)
    
    # Ensure only the bottom and left axis lines are visible but subtle
    for position, spine in ax1.spines.items():
        if position in ['bottom', 'left']:
            spine.set_visible(True)
            spine.set_color('#cccccc')
            spine.set_linewidth(0.5)
        else:
            spine.set_visible(False)
            
    # Set the same style for the right spine (cost axis)
    for position, spine in ax2.spines.items():
        if position == 'right':
            spine.set_visible(True)
            spine.set_color('#cccccc')
            spine.set_linewidth(0.5)
        else:
            spine.set_visible(False)
    
    # No watermark
    
    # Adjust layout to fit everything
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    
    # Save the figure if output path is provided
    if output_path:
        plt.savefig(output_path, dpi=400, bbox_inches='tight')
        print(f"Saved plot to {output_path}")
    
    # Display the plot
    plt.show()

def main():
    """Main function to parse arguments and generate reports"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Generate summary reports for benchmark results')
    parser.add_argument('dirnames', nargs='*', help='Benchmark directory paths to analyze (if none provided, uses all dirs in tmp.benchmarks except polyglot-benchmark)')
    parser.add_argument('--table-output', '-t', help='Path to save the table CSV')
    parser.add_argument('--plot-output', '-p', help='Path to save the plot image')
    
    args = parser.parse_args()
    
    # If no dirnames provided, use all directories in tmp.benchmarks except polyglot-benchmark
    if not args.dirnames:
        tmp_benchmarks = Path(__file__).parent.parent / "tmp.benchmarks"
        if tmp_benchmarks.exists() and tmp_benchmarks.is_dir():
            dirnames = [d for d in tmp_benchmarks.iterdir() 
                        if d.is_dir() and d.name != 'polyglot-benchmark']
        else:
            print("tmp.benchmarks directory not found")
            return
    else:
        dirnames = [Path(dirname) for dirname in args.dirnames]
    
    # Create benchmark-result directory if it doesn't exist
    result_dir = Path(__file__).parent.parent / "benchmark-result"
    result_dir.mkdir(exist_ok=True)
    
    # Create dated folder with datetime in a human-readable format
    from datetime import datetime
    now_datetime = datetime.now().strftime("%Y-%m-%d-%H-%M-%S")
    dated_dir = result_dir / f"report-{now_datetime}"
    dated_dir.mkdir(exist_ok=True)
    
    # Set default output paths if not provided
    table_output = args.table_output or (dated_dir / "summary_table.csv")
    plot_output = args.plot_output or (dated_dir / "benchmark_comparison.png")
    
    # Generate DataFrame from results
    df = generate_summary(dirnames)
    
    if df is not None:
        # Clean model names: remove anything before a slash
        df['Model'] = df['Model'].apply(lambda x: x.split('/')[-1] if '/' in x else x)
        
        # Add directory info to model name for models that appear multiple times
        model_counts = df['Model'].value_counts()
        duplicate_models = model_counts[model_counts > 1].index.tolist()
        
        for model in duplicate_models:
            mask = df['Model'] == model
            # Manually inspect each item to help with debugging
            for idx, row in df.loc[mask].iterrows():
                # Check if this is a thinking model by examining the history file
                dir_path = Path(row.get('dir_name', ''))
                is_thinking = False
                
                # Look for the .aider.chat.history.md file to determine if thinking mode was enabled
                # Based on the message from 2025-04-02 23:25:55, we know that model does NOT have thinking
                # enabled - it has whole edit format, prompt cache, infinite output but NO think tokens
                
                # Special case: Only the specific directory for the thinking experiment has thinking enabled
                # We explicitly know that 2025-04-03-10-39-27 is the directory with thinking enabled
                if "2025-04-03-10-39-27" in str(dir_path) or '2025-04-03-10-39-27' in str(row.get('dir_name', '')):
                    is_thinking = True
                
                # Differentiate the models with unique names to ensure different bars
                if is_thinking and model == 'claude-3-7-sonnet-20250219':
                    # Add a unique identifier from the directory name to ensure different bars
                    dir_id = str(idx).split('/')[-1] if '/' in str(idx) else ''
                    dir_id = dir_id[:8] if dir_id else str(idx)[:8]
                    df.loc[idx, 'Model'] = f"{model} (thinking)"
                elif model == 'claude-3-7-sonnet-20250219':
                    # Add a unique identifier to distinguish regular models
                    dir_id = str(idx).split('/')[-1] if '/' in str(idx) else ''
                    dir_id = dir_id[:8] if dir_id else str(idx)[:8]
                    df.loc[idx, 'Model'] = f"{model} (regular)"
                else:
                    df.loc[idx, 'Model'] = f"{model} ({idx.split('--')[-1].split('-full')[0] if '--' in str(idx) else idx})"
        
        # Generate and display table
        generate_table(df, table_output)
        
        # Generate and display plot
        plot_benchmark_comparison(df, plot_output)
    else:
        print("Unable to generate reports. No valid data found.")

if __name__ == "__main__":
    main()
#!/usr/bin/env python3
import os
import sys
import json
import re
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import textwrap
import numpy as np
from collections import defaultdict
from pathlib import Path
import datetime
from typing import List, Dict, Any, Optional, Tuple

# Directories where benchmark results are stored
BENCHMARK_DNAME = Path(os.environ.get("AIDER_BENCHMARK_DIR", "tmp.benchmarks"))

def load_results(dirname: str) -> List[Dict[str, Any]]:
    """Load benchmark results from the specified directory."""
    dirname = Path(dirname)
    all_results = []
    
    # Look specifically for .aider.results.json files
    for fname in dirname.glob("**/exercises/practice/*/.aider.results.json"):
        try:
            results = json.loads(fname.read_text())
            # Add directory path to results for reference
            results['dir_path'] = str(dirname)
            all_results.append(results)
        except json.JSONDecodeError:
            print(f"json.JSONDecodeError: {fname}")
            continue
    
    return all_results

def extract_key_metrics(dirname: str) -> Dict[str, Any]:
    """Extract key metrics from benchmark results."""
    all_results = load_results(dirname)
    
    if not all_results:
        print(f"No results found in {dirname}")
        return {}
    
    # Get the directory name without the full path
    dir_name = Path(dirname).name
    
    metrics = {
        'dirname': dir_name,
        'dir_path': str(dirname),
        'completed_tests': len(all_results),
        'total_tests': len(list(Path(dirname).glob("**/exercises/practice/*"))),
        'model': None,
        'edit_format': None,
        'commit_hash': None,
        'pass_rate_1': 0,
        'pass_rate_2': 0,
        'percent_cases_well_formed': 0,
        'error_outputs': 0,
        'num_malformed_responses': 0,
        'num_with_malformed_responses': 0,
        'seconds_per_case': 0,
        'total_cost': 0,
        'cost_per_case': 0
    }
    
    # Collect variants
    variants = defaultdict(set)
    
    # Counters
    total_duration = 0
    total_cost = 0
    passed_tests_1 = 0
    passed_tests_2 = 0
    error_outputs = 0
    malformed_responses = 0
    with_malformed_responses = 0
    
    for results in all_results:
        # Process model, edit format, etc.
        for key in ["model", "edit_format", "commit_hash"]:
            val = results.get(key)
            if val:
                variants[key].add(val)
        
        # Test outcomes
        tests_outcomes = results.get("tests_outcomes", [])
        if len(tests_outcomes) >= 1 and tests_outcomes[0]:
            passed_tests_1 += 1
        if len(tests_outcomes) >= 1 and tests_outcomes[-1]:  # Use the last test outcome
            passed_tests_2 += 1
        
        # Error metrics
        error_outputs += results.get("num_error_outputs", 0)
        num_malformed = results.get("num_malformed_responses", 0)
        malformed_responses += num_malformed
        if num_malformed:
            with_malformed_responses += 1
        
        # Time and cost
        total_duration += results.get("duration", 0)
        total_cost += results.get("cost", 0)
    
    # Calculate final metrics
    if metrics['completed_tests'] > 0:
        metrics['pass_rate_1'] = round(100 * passed_tests_1 / metrics['completed_tests'], 1)
        metrics['pass_rate_2'] = round(100 * passed_tests_2 / metrics['completed_tests'], 1)
        metrics['percent_cases_well_formed'] = round(100 * (1 - with_malformed_responses / metrics['completed_tests']), 1)
        metrics['seconds_per_case'] = round(total_duration / metrics['completed_tests'], 1)
        metrics['total_cost'] = round(total_cost, 2)
        metrics['cost_per_case'] = round(total_cost / metrics['completed_tests'], 4)
    
    metrics['error_outputs'] = error_outputs
    metrics['num_malformed_responses'] = malformed_responses
    metrics['num_with_malformed_responses'] = with_malformed_responses
    metrics['passes_total'] = passed_tests_2
    metrics['passes_1st_try'] = passed_tests_1
    
    # Handle variants
    for key in ["model", "edit_format", "commit_hash"]:
        values = variants.get(key, set())
        if values:
            metrics[key] = ", ".join(str(v) for v in values)
    
    # Simplify model names to make them more readable
    if metrics.get('model'):
        model_name = metrics['model']
        # Remove provider prefixes
        if 'anthropic/' in model_name:
            model_name = model_name.replace('anthropic/', '')
        if 'openai/' in model_name:
            model_name = model_name.replace('openai/', '')
        if 'gemini/' in model_name:
            model_name = model_name.replace('gemini/', '')
        
        # Simplify Claude model names
        if 'claude-3-7-sonnet-20250219' in model_name:
            model_name = model_name.replace('claude-3-7-sonnet-20250219', 'Sonnet-3.7')
        
        metrics['model'] = model_name
            
    # Check if the directory name indicates thinking mode
    is_thinking = False
    if "thinking" in dir_name.lower():
        is_thinking = True
    
    print(f"is_thinking: {is_thinking}, dir_name: {dir_name}, model: {metrics['model']}")
    
    # Add thinking mode tag to model name if applicable
    if is_thinking and metrics.get('model') and 'claude' in metrics.get('model', '').lower():
        metrics['model'] = f"{metrics['model']} (thinking)"
        # Add a unique identifier to ensure the model doesn't get combined with regular version
        metrics['model_mode'] = 'thinking'
    elif 'claude' in metrics.get('model', '').lower():
        # Add a unique identifier to ensure it's treated separately from thinking mode
        metrics['model_mode'] = 'regular'
    
    return metrics

def generate_table(metrics_list: List[Dict[str, Any]], output_path: Optional[str] = None, 
                  markdown_path: Optional[str] = None) -> Tuple[str, pd.DataFrame]:
    """
    Generate a formatted table from metrics list and optionally save as CSV/markdown.
    
    Args:
        metrics_list: List of metrics dictionaries
        output_path: Optional path to save as CSV
        markdown_path: Optional path to save as markdown
        
    Returns:
        Tuple of (formatted table string, pandas DataFrame)
    """
    if not metrics_list:
        return "No metrics data available.", pd.DataFrame()
    
    # Force a display name that includes (thinking) for thinking models
    for metrics in metrics_list:
        if metrics.get('is_thinking', False):
            metrics['model_display'] = f"{metrics['model_base']} (thinking)"
    
    # Save raw metrics list before conversion to DataFrame
    raw_metrics = []
    for m in metrics_list:
        raw_metrics.append(m.copy())
    
    # Convert to DataFrame for easier manipulation
    df = pd.DataFrame(metrics_list)
    
    # Print the models in the DataFrame for debugging
    print("\nModels in DataFrame:")
    if 'model_display' in df.columns:
        for idx, model in enumerate(df['model_display']):
            print(f"{idx}: {model}")
    
    # Create a new dataframe using model_display instead of model
    # This circumvents any potential automatic combining of models with the same name
    display_data = []
    for metrics in raw_metrics:
        display_row = {
            'Model': metrics.get('model_display'),
            'Tests': metrics.get('completed_tests'),
            'Pass %': metrics.get('pass_rate_2'),
            'Pass 1st Try %': metrics.get('pass_rate_1'),
            'Tests Passed': metrics.get('passes_total'),
            'Passes 1st Try': metrics.get('passes_1st_try'),
            'Well Formed %': metrics.get('percent_cases_well_formed'),
            'Errors': metrics.get('error_outputs'),
            'Sec/Test': metrics.get('seconds_per_case'),
            'Total Cost ($)': metrics.get('total_cost'),
            'Cost/Test ($)': metrics.get('cost_per_case'),
            # Add internal fields for sorting and plotting
            '_model': metrics.get('model'),
            '_dir_id': metrics.get('dir_id'),
            '_is_thinking': metrics.get('is_thinking', False),
            '_pass_rate': metrics.get('pass_rate_2')
        }
        display_data.append(display_row)
    
    # Create display DataFrame directly from our controlled display_data
    display_df = pd.DataFrame(display_data)
    
    # Sort by pass rate (descending)
    display_df = display_df.sort_values(by='_pass_rate', ascending=False)
    
    # Format numbers
    for col in display_df.columns:
        if col in ['Pass %', 'Pass 1st Try %', 'Well Formed %']:
            display_df[col] = display_df[col].apply(lambda x: f"{x:.1f}" if isinstance(x, (int, float)) else x)
        elif col == 'Total Cost ($)':
            display_df[col] = display_df[col].apply(lambda x: f"{float(x):.2f}" if isinstance(x, (int, float)) else x)
        elif col == 'Cost/Test ($)':
            display_df[col] = display_df[col].apply(lambda x: f"{float(x):.4f}" if isinstance(x, (int, float)) else x)
        elif col == 'Sec/Test':
            display_df[col] = display_df[col].apply(lambda x: f"{float(x):.1f}" if isinstance(x, (int, float)) else x)
    
    # Select display columns (excluding internal ones)
    display_cols = [
        'Model', 'Tests', 'Pass %', 'Pass 1st Try %', 
        'Tests Passed', 'Passes 1st Try', 'Well Formed %',
        'Errors', 'Sec/Test', 'Total Cost ($)', 'Cost/Test ($)'
    ]
    display_df_final = display_df[display_cols]
    
    # Print final display DataFrame for debugging
    print("\nFinal display DataFrame models:")
    for idx, model in enumerate(display_df_final['Model']):
        print(f"{idx}: {model}")
    
    # Generate formatted table string
    table_str = display_df_final.to_string(index=False)
    
    # Save as CSV if requested
    if output_path:
        print(f"Saving results to {output_path}")
        # Save both display and internal data
        df_to_save = display_df.copy()
        df_to_save.to_csv(output_path, index=False)
    
    # Save as markdown if requested
    if markdown_path:
        # Create a markdown table
        md_cols = display_cols
        md_table = "| " + " | ".join(md_cols) + " |\n"
        md_table += "| " + " | ".join(["---" for _ in md_cols]) + " |\n"
        
        # Add each row
        for _, row in display_df_final.iterrows():
            md_table += "| " + " | ".join([str(row[col]) for col in md_cols]) + " |\n"
        
        with open(markdown_path, 'w') as f:
            f.write(md_table)
        print(f"Saved markdown table to {markdown_path}")
    
    # Return the full DataFrame with internal fields for the plot function
    return table_str, display_df

def plot_comparison(df: pd.DataFrame, output_path: Optional[str] = None) -> None:
    """
    Generate a bar chart comparing pass rates with cost overlay.
    
    Args:
        df: DataFrame with benchmark results
        output_path: Path to save the plot image
    """
    # Define column names as variables for consistent usage
    pass_rate_column = '_pass_rate'
    cost_column = 'Total Cost ($)'
    
    # Use the Model column for plotting
    model_col = 'Model'
    
    if not all(col in df.columns for col in [model_col, pass_rate_column, cost_column]):
        print(f"DataFrame missing required columns for plotting: {model_col}, {pass_rate_column}, {cost_column}")
        return
    
    # Ensure numeric columns
    df[pass_rate_column] = pd.to_numeric(df[pass_rate_column], errors='coerce')
    df[cost_column] = pd.to_numeric(df[cost_column].str.replace('$', '').astype(float), errors='coerce')
    
    # Sort by pass rate (descending)
    df = df.sort_values(by=pass_rate_column, ascending=False)
    
    # Set up the plot with a larger figure and styling
    plt.style.use('seaborn-v0_8-whitegrid')
    plt.rcParams['font.family'] = 'Berkeley Mono'
    plt.rcParams['font.weight'] = 'bold'
    plt.rcParams['axes.labelweight'] = 'bold'
    plt.rcParams['axes.titleweight'] = 'bold'
    plt.rcParams['figure.facecolor'] = 'white'
    
    # Create a larger figure with better aspect ratio
    fig, ax1 = plt.subplots(figsize=(16, 10))
    
    # Color palette
    bar_color = '#0066cc'  # Rich blue
    bar_edge_color = '#003366'  # Darker blue
    cost_color = '#cc3300'  # Deep red
    
    # Add a gradient background
    ax1.set_facecolor('#f8f9fa')
    fig.patch.set_facecolor('white')
    
    # Bar width
    bar_width = 0.7
    
    # Plot bars for pass rate
    bars = ax1.bar(
        df[model_col], 
        df[pass_rate_column], 
        width=bar_width, 
        color=bar_color, 
        label='Pass Rate (%)',
        edgecolor=bar_edge_color,
        linewidth=1.5,
        alpha=0.85,
        zorder=3
    )
    
    # Add value labels on top of bars
    for bar in bars:
        height = bar.get_height()
        ax1.text(
            bar.get_x() + bar.get_width() / 2.,
            height + 0.8,
            f'{height:.1f}%',
            ha='center', 
            va='bottom',
            fontsize=18,
            fontweight='bold',
            color='#000000',
            bbox=dict(facecolor='white', alpha=0.7, edgecolor='none', boxstyle='round,pad=0.2')
        )
    
    # Set up the primary y-axis with more padding
    max_pass_rate = max(df[pass_rate_column])
    ax1.set_ylim(0, max_pass_rate * 1.25)  # 25% headroom
    ax1.set_ylabel('Pass Rate (%)', fontsize=18, fontweight='bold', labelpad=15)
    ax1.set_xlabel('Model', fontsize=18, fontweight='bold', labelpad=15)
    
    # Style the ticks and grid
    ax1.tick_params(axis='y', labelcolor='black', labelsize=14, width=2)
    ax1.tick_params(axis='x', labelsize=14, width=2, length=6, pad=8)
    
    # Set up secondary y-axis for cost
    ax2 = ax1.twinx()
    
    # Plot cost as points
    scatter = ax2.scatter(
        df[model_col],
        df[cost_column],
        color=cost_color,
        s=200,  # Larger points
        marker='D',  # Diamond marker
        label='Total Cost ($)',
        zorder=5,
        alpha=1.0,
        edgecolor='white',
        linewidth=1.5
    )
    
    # Add value labels for cost points
    for i, cost in enumerate(df[cost_column]):
        # Position price labels to the top right
        x_offset = 10  # Position to the right
        y_offset = 10  # Position to the top
            
        # Add a background box
        ax2.annotate(
            f'${cost:.2f}',
            (i, cost),
            xytext=(x_offset, y_offset),
            textcoords='offset points',
            ha='left',
            va='bottom',
            fontsize=14,
            fontweight='bold',
            color=cost_color,
            bbox=dict(facecolor='white', alpha=0.8, edgecolor=cost_color, boxstyle='round,pad=0.3', linewidth=1)
        )
    
    # Style the secondary y-axis
    ax2.set_ylabel('Total Cost ($)', color=cost_color, fontsize=18, fontweight='bold', labelpad=15)
    ax2.tick_params(axis='y', labelcolor=cost_color, labelsize=14, width=2)
    
    # Set the y-limit with extra padding for cost labels
    ax2.set_ylim(0, max(df[cost_column]) * 1.35)  # 35% headroom
    
    # Add title
    plt.suptitle('Haskell LLM Benchmark', 
              fontsize=26, fontweight='bold', y=0.98)
    
    # Add subtitle
    plt.title('Model Performance: Pass Rate vs. Cost', 
              fontsize=18, pad=20, color='#444444')
    
    # Add footer with date
    today = datetime.datetime.now().strftime("%B %d, %Y")
    plt.figtext(0.5, 0.01, f"Generated on {today} | pass rate based on {pass_rate_column}", 
                ha="center", fontsize=12, fontweight='bold', color='#666666')
    
    # Create legend
    lines1, labels1 = ax1.get_legend_handles_labels()
    lines2, labels2 = ax2.get_legend_handles_labels()
    legend = ax1.legend(lines1 + lines2, labels1 + labels2, 
                loc='upper right', frameon=True, framealpha=0.95, 
                edgecolor='#444444', fontsize=14)
    legend.get_frame().set_facecolor('white')
    legend.get_frame().set_linewidth(2)
    
    # Rotate x-axis labels at 45 degree angle
    plt.xticks(rotation=45, ha='right', fontsize=12)
    
    # Process model names for better readability
    labels = plt.gca().get_xticklabels()
    
    for label in labels:
        text = label.get_text()
        
        # Simplify model names for better readability
        if 'anthropic/' in text:
            text = text.replace('anthropic/', '')
        if 'openai/' in text:
            text = text.replace('openai/', '')
        if 'gemini/' in text:
            text = text.replace('gemini/', '')
        
        # Special case for Claude models
        if 'claude-3-7-sonnet' in text:
            text = text.replace('claude-3-7-sonnet-20250219', 'Sonnet-3.7')
            text = text.replace('claude-3-7-sonnet-3.7', 'Sonnet-3.7')
        
        # Handle special cases with mode indicators
        if '(thinking)' in text or '(regular)' in text:
            # Preserve the mode indicator but limit the width
            parts = text.split(' (')
            if len(parts) > 1:
                base = parts[0]
                mode = '(' + parts[1]
                # Limit base name length
                if len(base) > 12:
                    base = base[:10] + '...'
                text = f"{base}\n{mode}"
        else:
            # For other models, wrap text at a certain width
            if len(text) > 12:
                # Simple line break for long names
                half_length = len(text) // 2
                # Find space near the middle
                space_pos = text.find(' ', half_length - 5)
                if space_pos > 0:
                    text = text[:space_pos] + '\n' + text[space_pos+1:]
                else:
                    # Force break if no space found
                    text = text[:half_length] + '\n' + text[half_length:]
        
        label.set_text(text)
    
    # Add more padding below the plot for the labels
    plt.subplots_adjust(bottom=0.25)
    
    # No gridlines
    ax1.grid(False)
    ax1.xaxis.grid(False)
    ax1.yaxis.grid(False)
    ax2.grid(False)
    ax2.xaxis.grid(False)
    ax2.yaxis.grid(False)
    
    # Simplified spines
    for position, spine in ax1.spines.items():
        if position in ['bottom', 'left']:
            spine.set_visible(True)
            spine.set_color('#cccccc')
            spine.set_linewidth(0.5)
        else:
            spine.set_visible(False)
            
    for position, spine in ax2.spines.items():
        if position == 'right':
            spine.set_visible(True)
            spine.set_color('#cccccc')
            spine.set_linewidth(0.5)
        else:
            spine.set_visible(False)
    
    # Adjust layout
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    
    # Save the figure
    if output_path:
        plt.savefig(output_path, dpi=400, bbox_inches='tight')
        print(f"Saved plot to {output_path}")
    
    # Display the plot
    plt.show()

def main():
    """Parse command line arguments and generate report."""
    import argparse
    parser = argparse.ArgumentParser(description="Generate summary table for Haskell benchmarks")
    parser.add_argument("dirnames", nargs="*", help="Benchmark directory names (if not provided, uses all dirs in tmp.benchmarks except polyglot-benchmark)")
    parser.add_argument("--output", "-o", help="Output path prefix (default: generates a timestamp directory)")
    parser.add_argument("--table-output", "-t", help="Path to save the table CSV")
    parser.add_argument("--plot-output", "-p", help="Path to save the plot image")
    parser.add_argument("--debug", "-d", action="store_true", help="Enable debug output")
    
    args = parser.parse_args()
    
    # If no dirnames provided, use all directories in tmp.benchmarks except polyglot-benchmark
    if not args.dirnames:
        if BENCHMARK_DNAME.exists() and BENCHMARK_DNAME.is_dir():
            dirnames = [d for d in BENCHMARK_DNAME.iterdir() 
                      if d.is_dir() and d.name != 'polyglot-benchmark']
        else:
            print(f"{BENCHMARK_DNAME} directory not found")
            return
    else:
        dirnames = []
        for dirname in args.dirnames:
            dirname_path = Path(dirname)
            if not dirname_path.is_absolute():
                if (BENCHMARK_DNAME / dirname_path).exists():
                    dirname_path = BENCHMARK_DNAME / dirname_path
            dirnames.append(dirname_path)
    
    # Create result directory
    result_dir = Path(__file__).parent.parent / "benchmark-result"
    result_dir.mkdir(exist_ok=True)
    
    # Create dated folder
    now_datetime = datetime.datetime.now().strftime("%Y-%m-%d-%H-%M-%S")
    dated_dir = result_dir / f"report-{now_datetime}"
    dated_dir.mkdir(exist_ok=True)
    
    # Set default output paths
    table_output = args.table_output or (dated_dir / "summary_table.csv")
    plot_output = args.plot_output or (dated_dir / "benchmark_comparison.png")
    markdown_output = str(table_output).replace('.csv', '.md')
    
    # Process each benchmark directory
    metrics_list = []
    for dirname in dirnames:
        print(f"Processing {dirname}...")
        
        # Check if this is a thinking directory before even calling extract_key_metrics
        is_dir_thinking = "thinking" in dirname.name.lower()
        print(f"Directory name contains 'thinking': {is_dir_thinking}")
        
        metrics = extract_key_metrics(dirname)
        if metrics:
            # Add a unique identifier based on directory
            metrics['dir_id'] = dirname.name
            
            # Explicitly mark thinking models
            if is_dir_thinking and 'claude' in metrics.get('model', '').lower():
                # Add a special flag that will be used in display
                metrics['is_thinking'] = True
                # Keep the original name for unambiguous identification
                metrics['model_base'] = metrics['model']
                # Add (thinking) suffix to the model name for display
                metrics['model_display'] = f"{metrics['model']} (thinking)"
                print(f"Marked thinking model: {metrics['model_display']}")
            else:
                metrics['is_thinking'] = False
                metrics['model_base'] = metrics['model']
                metrics['model_display'] = metrics['model']
            
            metrics_list.append(metrics)
    
    if not metrics_list:
        print("No metrics data available from any directory.")
        return
    
    # Generate the table
    table_str, df = generate_table(metrics_list, table_output, markdown_output)
    print("\nResults Summary Table:")
    print(table_str)
    
    # Generate the plot
    plot_comparison(df, plot_output)
    
    print(f"\nReport generated in {dated_dir}")

if __name__ == "__main__":
    main()
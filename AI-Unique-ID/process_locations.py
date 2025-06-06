import pandas as pd
from openai import OpenAI
import os
from typing import Dict, List, Tuple
import json
from IPython.display import display
from tqdm import tqdm
from datetime import datetime

# =========================
# CONFIGURATION VARIABLES
# =========================

# Define the folder for input and output files
DATA_FOLDER = "AI-Unique-ID/"

# Define only the input file name
INPUT_FILE_NAME = "co-data-boulder.csv"

# Construct full input file path
INPUT_FILE = os.path.join(DATA_FOLDER, INPUT_FILE_NAME)

# Automatically derive output file names with suffixes in the same folder
input_base = os.path.splitext(INPUT_FILE_NAME)[0]
PROCESSED_RESULTS_FILE = os.path.join(DATA_FOLDER, f"{input_base}-api-return.csv")
FINAL_MERGED_RESULTS_FILE = os.path.join(DATA_FOLDER, f"{input_base}-entity-ids.csv")

# Set up OpenAI API key
client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'))

def initial_processing(input_file: str) -> Dict[str, pd.DataFrame]:
    """
    Perform initial processing on the input CSV file.
    Returns a dictionary of dataframes grouped by city+state.
    """
    print("\n📊 Loading and processing data...")
    # Read the CSV file
    df = pd.read_csv(input_file)
    
    # Drop specified columns
    columns_to_drop = ['unclear.address', 'full.address', 'lon', 'lat', 'geoAddress', 'status']
    df = df.drop(columns=[col for col in columns_to_drop if col in df.columns])
    
    # Sort by title, city, then year
    df = df.sort_values(['title', 'city', 'year'])
    
    # Group by city+state
    grouped_dfs = {}
    for (city, state), group in df.groupby(['city', 'state']):
        key = f"{city}_{state}"
        grouped_dfs[key] = group
    
    print(f"✅ Data grouped into {len(grouped_dfs)} city-state combinations")
    return grouped_dfs


def process_with_openai(df: pd.DataFrame, group_key: str = 'N/A') -> pd.DataFrame:
    """
    Process a single dataframe using OpenAI API.
    First pass: Only identify which records should be grouped together.
    Optionally takes a group_key (e.g., city_state) for logging.
    """
    # Convert dataframe to JSON for API input
    records = df.to_dict('records')
    
    # Create the prompt
    prompt = f"""Your task is to help with data processing of a dataset coming from two LGBTQ guidebooks published in the late 1970s and 1980s (Gaia's Guide for lesbians and Damron's Guide for gay men). Each record in the dataset corresponds to a single listing in one of the two guidebooks in a single year. We're trying to determine what listings are common across multiple years/guidebooks.

Here is the data to process:
{json.dumps(records, indent=2)}

Please analyze these records and:
1. Identify which records represent the same entity (they should be grouped together)
2. For each record, provide:
   - A GROUP_ID (numeric identifier for the group this record belongs to)
   - REASONING (detailed explanation of why these records were grouped together)
   - CONFIDENCE ("low", "medium", or "high")

IMPORTANT: You must return exactly one result for each input record. The number of results must match the number of input records.

Return the results as a JSON array with these columns: GROUP_ID, REASONING, and CONFIDENCE."""

    # Make API call
    response = client.chat.completions.create(
        model="o4-mini",
        messages=[
            {"role": "system", "content": "You are a data processing assistant helping to identify unique entities in historical LGBTQ guidebook listings."},
            {"role": "user", "content": prompt}
        ]
    )

    # --- Token usage and cost calculation ---
    usage = getattr(response, 'usage', None)
    log_lines = []
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    model_name = "o4-mini"
    num_records = len(df)
    if usage:
        prompt_tokens = getattr(usage, 'prompt_tokens', 0) or 0
        completion_tokens = getattr(usage, 'completion_tokens', 0) or 0
        total_tokens = getattr(usage, 'total_tokens', 0) or 0
        input_cost = (prompt_tokens / 1_000_000) * 1.10
        output_cost = (completion_tokens / 1_000_000) * 4.40
        total_cost = input_cost + output_cost
        print(f"\n🔢 Token usage for this call:")
        print(f"  Prompt tokens: {prompt_tokens}")
        print(f"  Completion tokens: {completion_tokens}")
        print(f"  Total tokens: {total_tokens}")
        print(f"💲 Cost for this call: ${total_cost:.6f} (input: ${input_cost:.6f}, output: ${output_cost:.6f})\n")
        safe_group_key = group_key if group_key is not None else 'N/A'
        log_lines = [
            f"Timestamp: {timestamp}",
            f"Model: {model_name}",
            f"Group key: {safe_group_key}",
            f"Number of records: {num_records}",
            f"Prompt tokens: {prompt_tokens}",
            f"Completion tokens: {completion_tokens}",
            f"Total tokens: {total_tokens}",
            f"Input cost: ${input_cost:.6f}",
            f"Output cost: ${output_cost:.6f}",
            f"Total cost: ${total_cost:.6f}",
            "---"
        ]
    else:
        print("\n⚠️ No token usage information returned by API.\n")
        safe_group_key = group_key if group_key is not None else 'N/A'
        log_lines = [
            f"Timestamp: {timestamp}",
            f"Model: {model_name}",
            f"Group key: {safe_group_key}",
            f"Number of records: {num_records}",
            f"Token usage: N/A",
            f"Cost: N/A",
            "---"
        ]
    # Write log to file
    log_dir = "AI-Unique-ID/logs"
    os.makedirs(log_dir, exist_ok=True)
    log_filename = f"{log_dir}/openai_log_{timestamp}.txt"
    with open(log_filename, "w") as f:
        f.write("\n".join(log_lines))
    # --- End token usage and cost calculation ---
    
    # Parse the response
    try:
        result = json.loads(response.choices[0].message.content)
        result_df = pd.DataFrame(result)
        
        # Validate the response
        if len(result_df) != len(df):
            print(f"⚠️ Warning: API returned {len(result_df)} results for {len(df)} input records")
            print("Input records:", len(df))
            print("API response:", len(result_df))
            print("First few API results:", result_df.head())
            
            # If we have too many results, take only the first len(df) rows
            if len(result_df) > len(df):
                result_df = result_df.head(len(df))
            # If we have too few results, pad with the last result
            else:
                last_row = result_df.iloc[-1] if not result_df.empty else pd.Series({'GROUP_ID': 1, 'REASONING': 'Error in processing', 'CONFIDENCE': 'low'})
                padding = pd.DataFrame([last_row] * (len(df) - len(result_df)))
                result_df = pd.concat([result_df, padding], ignore_index=True)
        
        # Add record_id back to the result dataframe
        result_df['record_id'] = df['record_id'].values
        return result_df
    except Exception as e:
        print(f"❌ Error processing OpenAI response: {e}")
        print("Response content:", response.choices[0].message.content)
        # Return empty dataframe with record_id and default values
        empty_df = pd.DataFrame({
            'GROUP_ID': [1] * len(df),
            'REASONING': ['Error in processing'] * len(df),
            'CONFIDENCE': ['low'] * len(df),
            'record_id': df['record_id'].values
        })
        return empty_df

def assign_global_ids(processed_dfs: Dict[str, pd.DataFrame]) -> pd.DataFrame:
    """
    Second pass: Assign global unique IDs across all cities.
    """
    print("\n🔄 Assigning global unique IDs...")
    
    # Combine all processed dataframes
    combined_df = pd.concat(processed_dfs.values(), ignore_index=True)
    
    # Create a mapping of (city_state, group_id) to global_id
    global_id_map = {}
    current_global_id = 1
    
    # Sort by city_state and group_id to ensure consistent ordering
    for city_state, df in processed_dfs.items():
        for group_id in sorted(df['GROUP_ID'].unique()):
            key = (city_state, group_id)
            if key not in global_id_map:
                global_id_map[key] = current_global_id
                current_global_id += 1
    
    # Apply the mapping to create ENTITY_ID
    def get_global_id(row):
        return global_id_map.get((row['city_state'], row['GROUP_ID']), -1)
    
    combined_df['city_state'] = combined_df['record_id'].map(
        {order: city_state for city_state, df in processed_dfs.items() 
         for order in df['record_id']}
    )
    combined_df['ENTITY_ID'] = combined_df.apply(get_global_id, axis=1)
    
    return combined_df[['record_id', 'ENTITY_ID', 'REASONING', 'CONFIDENCE']]

def main():
    print("\n🚀 Starting data processing pipeline...")
    
    # Initial processing
    grouped_dfs = initial_processing(INPUT_FILE)
    
    # Process each group with OpenAI
    print("\n🤖 Processing city-state groups with OpenAI...")
    processed_dfs = {}
    for key, df in tqdm(grouped_dfs.items(), desc="Processing groups", unit="group"):
        processed_df = process_with_openai(df, str(key))
        processed_dfs[key] = processed_df
    
    # Assign global unique IDs
    final_df = assign_global_ids(processed_dfs)
    
    # Save the processed results
    print("\n💾 Saving processed results...")
    final_df.to_csv(PROCESSED_RESULTS_FILE, index=False)
    
    # Join back to original data using record_id
    print("\n🔄 Merging with original data...")
    original_df = pd.read_csv(INPUT_FILE)
    final_merged_df = pd.merge(
        original_df,
        final_df,
        on='record_id',
        how='left'
    )
    
    # Save the final merged results
    print("\n💾 Saving final results...")
    final_merged_df.to_csv(FINAL_MERGED_RESULTS_FILE, index=False)
    
    print("\n✨ Processing complete! Results saved to:")
    print(f"  - {PROCESSED_RESULTS_FILE}")
    print(f"  - {FINAL_MERGED_RESULTS_FILE}")

if __name__ == "__main__":
    main()
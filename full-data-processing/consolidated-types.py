# Pipeline: Categorize Listing Types with OpenAI (o4-mini)
# This script builds a lookup table mapping specific listing types to larger categories using OpenAI's API.
# Make sure to install the required packages: pandas, openai, tqdm

import pandas as pd
import openai
import os
from tqdm import tqdm
import time
import random
import csv
from io import StringIO

# Step 1: Set your OpenAI API key here (or use an environment variable)
openai.api_key = os.getenv('OPENAI_API_KEY', 'YOUR_API_KEY_HERE')  # Replace with your key or set env var

# Step 2: Load Data
# Reads the CSV file containing unique types and their counts
input_csv = 'full-data-processing/cleaned-types-counts.csv'
df = pd.read_csv(input_csv)
types = df['type_clean'].unique().tolist()
print(f'Loaded {len(types)} unique types.')

# Step 3: Define Larger Categories
# Read categories from the text file, one per line in the format 'Category Name - Description'
categories_file = 'full-data-processing/consolidated-type-categories-descriptions.txt'
categories = []
with open(categories_file, 'r', encoding='utf-8') as f:
    for line in f:
        line = line.strip()
        if line:
            if ' - ' in line:
                name, desc = line.split(' - ', 1)
                categories.append((name.strip(), desc.strip()))
            else:
                # If no description, just use the name
                categories.append((line.strip(), ''))

category_text = '\n'.join([f'- {name}: {desc}' for name, desc in categories])
print(category_text)

# Step 4: Prompt Engineering
# Function to build a prompt for a batch of types
def build_prompt(type_list):
    prompt = (
        "You are an expert in LGBTQ history and archival data.\n"
        "Given the following list of specific types of listings from LGBTQ travel guides, assign each type to the most appropriate larger category from the list below.\n"
        "Use reasoning and context, not just string matching.\n"
        "Types may contain punctuation (commas, slashes, etc.); treat each line as a single type, even if it contains punctuation.\n"
        "If there are several items in a single type, default to the first listed item.\n"
        "Only use 'Other or Unclear' if the type truly does not fit any category.\n"
        "For each type, return a CSV with two columns: type,category. Always enclose the type in double quotes, especially if it contains punctuation or spaces.\n\n"
        f"Larger categories:\n{category_text}\n\n"
        "Types to categorize:\n"
    )
    for t in type_list:
        prompt += f'"{t}"\n'
    return prompt

# Step 5: Batch Processing and API Calls
# Function to send batches of types to the OpenAI API and collect results
def categorize_types(types, batch_size=5, model='o4-mini'):
    results = []
    total_tokens = 0
    start_time = time.time()
    num_batches = 0
    for i in tqdm(range(0, len(types), batch_size)):
        batch = types[i:i+batch_size]
        prompt = build_prompt(batch)
        try:
            response = openai.chat.completions.create(
                model=model,
                messages=[{"role": "user", "content": prompt}],
                max_completion_tokens=800
            )
            num_batches += 1
            text = response.choices[0].message.content
            # Track token usage if available
            if hasattr(response, 'usage') and response.usage is not None:
                total_tokens += getattr(response.usage, 'total_tokens', 0)
            # Parse CSV output
            print("\n--- RAW MODEL RESPONSE ---")
            print(text)
            print("--------------------------\n")
            print("Full API response object:", response)
            if not text:
                print("Warning: Model response is empty or None.")
                print("Full API response object:", response)
                continue
            f = StringIO(text)
            reader = csv.reader(f)
            for row in reader:
                if len(row) != 2 or row[0].lower() == 'type':
                    continue
                t, cat = row
                results.append({'type': t.strip('" '), 'category': cat.strip('" ')})
        except Exception as e:
            print(f'Error in batch {i//batch_size}:', e)
    elapsed = time.time() - start_time
    print(f"\n--- Categorization Summary ---")
    print(f"Processed {len(types)} types in {num_batches} batches.")
    print(f"Elapsed time: {elapsed:.2f} seconds.")
    if total_tokens > 0:
        print(f"Estimated total tokens used: {total_tokens}")
        print(f"(Check your OpenAI account for exact credit usage)")
    else:
        print(f"Token usage not available from API response.")
    print(f"-----------------------------\n")
    return pd.DataFrame(results)

# Step 6: Run Categorization (this will use API credits)
#sample_types = random.sample(types, 10)
print(sample_types[2:6])
#lookup_df = categorize_types(sample_types[2:6], batch_size=1)
lookup_df
print(lookup_df.head())

#lookup_df.to_csv('full-data-processing/type-to-category-lookup.csv', index=False)


def build_single_prompt(type_str):
    prompt = (
        "You are an expert in LGBTQ history and archival data.\n"
        "Given the following type of listing from an LGBTQ travel guide, assign it to the single best larger category from this list:\n"
        "Use reasoning and context, not just string matching.\n"
        "Types may contain punctuation (commas, slashes, etc.); treat the entire string of text as a single type, even if it contains punctuation.\n"
        "If there are several items in a single type, default to the first listed item.\n"
        "Only use 'Other or Unclear' if the type truly does not fit any category.\n"
        f"{category_text}\n"
        f"Type: {type_str}\n"
        "Only return the category name, and nothing else."
    )
    return prompt

def categorize_types_one_by_one(types, model='o4-mini'):
    results = []
    for t in tqdm(types):
        prompt = build_single_prompt(t)
        try:
            response = openai.chat.completions.create(
                model=model,
                messages=[{"role": "user", "content": prompt}],
                max_completion_tokens=800
            )
            category = response.choices[0].message.content.strip()
            print(f"Type: {t} -> Category: {category}")
            results.append({'type': t, 'category': category})
        except Exception as e:
            print(f'Error for type "{t}":', e)
            results.append({'type': t, 'category': 'ERROR'})
    return pd.DataFrame(results)

lookup_df_one_by_one = categorize_types_one_by_one(sample_types[2:6])
lookup_df_one_by_one
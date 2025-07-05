import pandas as pd

# Read the JSON file
json_file = 'student_correlated.json'  # Replace with your file name
data = pd.read_json(json_file)

# Convert JSON to CSV
csv_file = 'students_correlated.csv'  # Output CSV file name
data.to_csv(csv_file, index=False)

print(f"JSON data has been successfully converted to {csv_file}")
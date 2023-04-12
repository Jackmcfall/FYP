import csv

# Read in the distances.csv file
with open("distances.csv") as file:
    reader = csv.DictReader(file)
    distance_data = list(reader)

# Read in the Validated.csv file
with open("Validated.csv") as file:
    reader = csv.DictReader(file)
    validated_data = list(reader)

# Create a dictionary mapping (Place1, Place2) pairs to distances
distance_dict = {(row["Place1"], row["Place2"]): row["Distance (km)"] for row in distance_data}

# Add a "Distance" column to the validated_data list by looking up the distances
# in the distance_dict based on the "Home" and "Away" columns
for row in validated_data:
    home = row["Home"]
    away = row["Away"]
    # Use the (Home, Away) combination to look up the distance in the distance_dict
    if (home, away) in distance_dict:
        row["Distance"] = distance_dict[(home, away)]
    # Use the (Away, Home) combination to look up the distance in the distance_dict
    elif (away, home) in distance_dict:
        row["Distance"] = distance_dict[(away, home)]

# Write out the validated_data list to a new CSV file with the "Distance" column added
with open("../../Desktop/FYP/Validated2.0.csv", "w") as file:
    writer = csv.DictWriter(file, fieldnames=validated_data[0].keys())
    writer.writeheader()
    writer.writerows(validated_data)

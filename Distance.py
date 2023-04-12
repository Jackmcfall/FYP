import csv
from geopy.distance import distance

places = [
    ("Chievo", (45.4353, 10.9686)),
    ("Lazio", (41.9341, 12.4547)),
    ("Torino", (45.0418, 7.6501)),
    ("Sassuolo", (44.7146, 10.6497)),
    ("Empoli", (43.7265, 10.9548)),
    ("Parma", (44.7949, 10.3385)),
    ("Bologna", (44.4922, 11.3099)),
    ("Atalanta", (45.7092, 9.6808)),
    ("Juventus", (45.1096, 7.6412)),
    ("Napoli", (40.8279, 14.1931)),
    ("SPAL", (44.8402, 11.6080)),
    ("Cagliari", (39.1997, 9.1374)),
    ("Genoa", (44.4165, 8.9525)),
    ("Inter Milan", (45.4785, 9.1214)),
    ("Frosinone", (41.6341, 13.3219)),
    ("Udinese", (46.0815, 13.1998)),
    ("Fiorentina", (43.7808, 11.2826)),
    ("AS Roma", (41.9341, 12.4547)),
    ("AC Milan", (45.4785, 9.1214)),
    ("Sampdoria", (44.4165, 8.9525)),
    ("Hellas Verona", (45.4353, 10.9686)),
    ("Lecce", (40.3651, 18.2090)),
    ("Brescia", (45.5707, 10.2371)),
    ("Spezia", (44.1022, 9.8087)),
    ("Crotone", (39.0793, 17.1167)),
    ("Benevento", (41.1165, 14.7811)),
    ("Salernitana", (40.6455, 14.8236)),
    ("Venezia", (45.4278, 12.3640)),
    ("Monza", (45.5831, 9.3081)),
    ("Cremonese", (45.1402, 10.0348)),

]

# Calculate the distances between all pairs of places
distances = []
for i in range(len(places)):
    place1, coords1 = places[i]
    for j in range(i + 1, len(places)):
        place2, coords2 = places[j]
        distances.append((place1, place2, distance(coords1, coords2).km))

# Write the result to a CSV file
with open("distances.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerow(["Place1", "Place2", "Distance (km)"])
    for place1, place2, distance in distances:
        writer.writerow([place1, place2, distance])

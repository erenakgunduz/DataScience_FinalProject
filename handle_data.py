import json
import os
import pandas as pd

# specify the path to your JSON file
json_path = f"{os.getcwd()}/data/sfv.json"

with open(json_path) as f:
    data = json.load(f)

# create an empty list to store the data
all_attacks = []

# loop through each character
for char in data:
    # loop through each normal attack for the character
    for move_name, move_data in data[char]["moves"]["normal"].items():
        # create a dictionary to store the attack data
        attack = {"Character": char, "Move": move_name}
        # append the 16 features to the attack dictionary
        attack.update(move_data)
        # append the character-specific stats to the attack dictionary
        attack.update(
            {
                "health": data[char]["stats"]["health"],
                "stun": data[char]["stats"]["stun"],
                "vgauge1": data[char]["stats"]["vgauge1"],
                "vgauge2": data[char]["stats"]["vgauge2"],
                "fDash": data[char]["stats"]["fDash"],
                "bDash": data[char]["stats"]["bDash"],
                "fWalk": data[char]["stats"]["fWalk"],
                "bWalk": data[char]["stats"]["bWalk"],
                "throwHurt": data[char]["stats"]["throwHurt"],
                "throwRange": data[char]["stats"]["throwRange"],
            }
        )

        # append the damage and stun data to the attack dictionary
        attack.update({"Damage": move_data["damage"], "Stun": move_data["stun"]})
        # append the attack dictionary to the list
        all_attacks.append(attack)

# create a pandas DataFrame from the list of attacks
df_all = pd.DataFrame(all_attacks)

# save the DataFrame to a CSV file
df_all.to_csv(f"{os.getcwd()}/data/all.csv", index=False)

# create a directory for the character-specific CSV files
if not os.path.exists(f"{os.getcwd()}/data/characters"):
    os.makedirs(f"{os.getcwd()}/data/characters")

# loop through each character
for char in data:
    # create an empty list to store the character's attacks
    char_attacks = []
    # loop through each normal attack for the character
    for move_name, move_data in data[char]["moves"]["normal"].items():
        # create a dictionary to store the attack data
        attack = {"Move": move_name}
        # append the 16 features to the attack dictionary
        attack.update(move_data)
        # append the character-specific stats to the attack dictionary
        attack.update(data[char]["stats"])
        # append the damage and stun data to the attack dictionary
        attack.update({"Damage": move_data["damage"], "Stun": move_data["stun"]})
        # append the attack dictionary to the list
        char_attacks.append(attack)

    # create a pandas DataFrame from the list of character's attacks
    df_char = pd.DataFrame(char_attacks)

    # save the DataFrame to a CSV file
    filename = f"{os.getcwd()}/data/characters/{char}.csv"
    df_char.to_csv(filename, index=False)

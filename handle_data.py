import json
import os
import pandas as pd


with open(f"{os.getcwd()}/data/sfv.json") as f:
    data = json.load(f)

# create a directory for the character-specific CSV files
if not os.path.exists(f"{os.getcwd()}/data/characters"):
    os.makedirs(f"{os.getcwd()}/data/characters")

all_attacks = []

# loop through each character
for char in data:
    char_attacks = []
    # loop through each normal attack for the character
    for move_name, move_data in data[char]["moves"]["normal"].items():
        # clean up data by adding attack only if it contains this info
        conditions = (
            "onBlock" in move_data
            and "damage" in move_data
            and "stun" in move_data
            and "airmove" in move_data
        )

        if conditions:
            # create a dictionary to store the attack data
            attack = {"Character": char, "Move": move_name}
            # append the 16 features to the attack dictionary
            # starting with onBlock followed by categorical features
            attack.update(
                {
                    "onBlock": move_data["onBlock"],
                    "plnCmd": move_data["plnCmd"],
                    "airmove": move_data["airmove"],
                    "followUp": move_data["followUp"],
                    "projectile": move_data["projectile"],
                    "moveType": move_data["moveType"],
                }
            )
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
            # append the attack dictionary to the lists
            all_attacks.append(attack)
            char_attacks.append(attack)

    df_char = pd.DataFrame(char_attacks)
    df_char = df_char.drop(columns=["Character"])

    df_char.to_csv(f"{os.getcwd()}/data/characters/{char}.csv", index=False)

df_all = pd.DataFrame(all_attacks)
df_all.to_csv(f"{os.getcwd()}/data/all.csv", index=False)

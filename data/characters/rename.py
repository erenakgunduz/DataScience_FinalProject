import os


directory = os.path.dirname(os.path.realpath(__file__))

for filename in os.listdir(directory):
    if "." in filename:
        if filename == "F.A.N.G.csv":
            new_name = "f_a_n_g.csv"
        elif len(filename.split(".")) == 3:
            name, name2, ext = filename.split(".")
            new_name = f"{name.lower()}_{name2.lower()}.{ext}"
        else:
            name, ext = filename.split(".")
            new_name = f"{name.lower().replace(' ', '_')}.{ext}"
        os.rename(f"{directory}/{filename}", f"{directory}/{new_name}")
        print(f"Renamed {filename} to {new_name}")

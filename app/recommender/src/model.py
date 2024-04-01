from .db_connect import get_db
from psycopg2.extras import RealDictCursor
import rasterio
import os
import pandas as pd
import pickle as pkl

dir = str(os.getcwd())

def get_all_foods():
    db = get_db()
    cur = db.cursor(cursor_factory=RealDictCursor)
    cur.execute("SELECT * FROM foods.veg")
    foods = cur.fetchall()
    cur.close()
    return foods

def predict_veg(latitude, longitude, map):
    av_nutrients = []
    nut_output = dict();
    conv_mg = 122
    conv_k = 390

    latitude = float(latitude)
    longitude = float(longitude)
    
    # open maps
    k_map = rasterio.open(os.path.join(dir, "recommender", "static", "images", "k_map.tif"))
    mg_map = rasterio.open(os.path.join(dir, "recommender", "static", "images", "mg_map.tif"))

    # read values and convert to available nutrients
    mg_row, mg_col = mg_map.index(longitude,latitude)
    k_row, k_col = k_map.index(longitude,latitude)
    soil_mg = (mg_map.read(1)[mg_row, mg_col] * conv_mg)
    soil_k = (k_map.read(1)[k_row, k_col] * conv_k)

    av_nutrients.append({
        'Magnesium': round(soil_mg,2),
        'Potassium': round(soil_k,2)
    })
    soil_nutrients = pd.DataFrame(av_nutrients)

    # load models
    with open((os.path.join(dir, "recommender", "src", "k_dt.pkl")), 'rb') as kf:
        k_mod = pkl.load(kf)
    with open((os.path.join(dir, "recommender", "src", "mg_dt.pkl")), 'rb') as mgf:
        mg_mod = pkl.load(mgf)

    # categorise based on requested content
    if map == "potassium":
        nut_output['soil'] = "Soil Potassium: "+str(round(soil_k,2))
        category = k_mod.predict(pd.DataFrame(soil_nutrients.iloc[:,1]))
    if map == "magnesium":
        nut_output['soil'] = "Soil Magnesium: "+str(round(soil_mg,2))
        category = mg_mod.predict(pd.DataFrame(soil_nutrients.iloc[:,0]))  
    nut_output['category'] = category

    return nut_output

def get_foods(category, map):
    category = category.tolist()
    cat = category.pop(0)
    db = get_db()
    cur = db.cursor(cursor_factory=RealDictCursor)
    if map == "potassium":
        cur.execute("SELECT * FROM foods.veg WHERE category=%s ORDER BY potassium ASC LIMIT 10",(cat,))
    if map == "magnesium":
        cur.execute("SELECT * FROM foods.veg WHERE category=%s ORDER BY magnesium ASC LIMIT 10",(cat,))
    vegs = cur.fetchall()
    cur.close()
    return vegs

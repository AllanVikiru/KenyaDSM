from .db_connect import get_db
from psycopg2.extras import RealDictCursor
import rasterio
import os
import pandas as pd

dir = str(os.getcwd())

def predict_veg(latitude, longitude, nut):
    latitude = float(latitude)
    longitude = float(longitude)
    k_map = rasterio.open(os.path.join(dir, "recommender", "static", "images", "k_map.tif"))
    mg_map = rasterio.open(os.path.join(dir, "recommender", "static", "images", "mg_map.tif"))
    mg_row, mg_col = mg_map.index(longitude,latitude)
    k_row, k_col = k_map.index(longitude,latitude)
    vegs = mg_map.read(1)[mg_row, mg_col]
    ex_k = k_map.read(1)[k_row, k_col]
    return vegs

def db_get_all_images():
    db = get_db()
    cur = db.cursor(cursor_factory=RealDictCursor)
    
    cur.execute("""SELECT * FROM minimaldb.tbl_image 
                   WHERE img_onair=true 
                   ORDER BY img_seqno""")

    images = cur.fetchall()

    cur.close()

    return images


def db_get_hp_images():
    db = get_db()
    cur = db.cursor(cursor_factory=RealDictCursor)
    
    cur.execute("""SELECT * FROM minimaldb.tbl_image 
                   WHERE img_onair=true AND img_is_in_hp=true
                   ORDER BY img_seqno""")

    images = cur.fetchall()

    cur.close()

    return images


def db_get_image_details(img_id):
    db = get_db()
    cur = db.cursor(cursor_factory=RealDictCursor)

    cur.execute("SELECT * FROM minimaldb.tbl_image WHERE img_id=%s",(img_id,))
    record = cur.fetchone()

    cur.close()

    return record
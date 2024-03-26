from .db_connect import get_db
import os
from psycopg2.extras import RealDictCursor
import rasterio

dir = str(os.getcwd())

def read_map():
    k_map = os.path.join(dir, "static", "images", "k_map.tif")
    mg_map = os.path.join(dir, "static", "images", "mg_map.tif")

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



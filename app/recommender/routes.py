import folium

from flask import (
    Blueprint, render_template, request, send_from_directory
)
from recommender.layoutUtils import *
from recommender.auth import *
from .model import *

bp = Blueprint('bl_home', __name__)

@bp.route('/',methods=('GET', 'POST'))

def index():
    return render_template('home/index.html')

def map():
    #create map object
    map = folium.Map(
        crs = "EPSG4326",
        location=[18.906286495910905, 79.40917968750001],
        zoom_start=5, 
        width=800, 
        height=500
    )

    # add a marker to the map object
    folium.Marker(
        [17.4127332, 78.078362],
        popup="<i>This a marker</i>").add_to(map)
    
#MANAGE sitemap and robots calls 
#These files are usually in root, but for Flask projects must
#be in the static folder
@bp.route('/robots.txt')
@bp.route('/sitemap.xml')
def static_from_root():
    return send_from_directory(current_app.static_folder, request.path[1:])


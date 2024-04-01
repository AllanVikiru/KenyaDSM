from flask import (
    Blueprint, render_template, request
)
from .src.layoutUtils import *
from .src.auth import *
from .src.model import *

bp = Blueprint('routes', __name__)

@bp.route('/', methods=('GET', 'POST'))
def index():
    foods = get_all_foods()

    if request.method == 'GET':
        return render_template('home/index.html', foods=foods)
    
    if request.method == 'POST':
        latitude = request.form['latitude']
        longitude = request.form['longitude']
        map = request.form['map']

        nutrients = predict_veg(latitude, longitude, map) # pass latitude longitude and map
        content = nutrients['soil']
        vegs = get_foods(nutrients['category'], map)

        return render_template('home/index.html', vegs=vegs, foods=foods, latitude=latitude, longitude=longitude, content=content, anchor='vegetables')
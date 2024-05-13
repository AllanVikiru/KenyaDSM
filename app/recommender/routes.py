from flask import (
    Blueprint, render_template, request
)
from .src.model import *

bp = Blueprint('routes', __name__)

display_format = dict();
display_format['opener'] = " [ "
display_format['midpoint'] = " ; "
display_format['closer'] = " ] :: "

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

        opener = display_format['opener']
        midpoint = display_format['midpoint']
        closer = display_format['closer']

        return render_template('home/index.html', 
                               vegs=vegs, foods=foods, 
                               latitude=latitude, longitude=longitude, opener = opener, midpoint = midpoint, closer = closer,
                               content=content, anchor='vegetables')
from flask import (
    Blueprint, render_template, request
)
from .src.layoutUtils import *
from .src.auth import *
from .src.model import *

bp = Blueprint('routes', __name__)

@bp.route('/',methods=('GET', 'POST'))

def index():
    if request.method == 'GET':
        return render_template('home/index.html')
    if request.method == 'POST':
        latitude = request.form['latitude']
        longitude = request.form['longitude']
        nut = request.form['map']

        predict_veg(latitude, longitude, nut)
        return render_template('home/index.html')

# @bp.route('/foods',methods=('GET', 'POST'))
# def generate():
#     foods = read_map()
#     return render_template('forms/generator.html', foods=foods)
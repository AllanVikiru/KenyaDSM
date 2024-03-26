import folium

from flask import (
    Blueprint, render_template, render_template_string, request, send_from_directory
)
from recommender.layoutUtils import *
from recommender.auth import *
from .model import *

bp = Blueprint('routes', __name__)

@bp.route('/',methods=('GET', 'POST'))

def index():
    return render_template('home/index.html')

#MANAGE sitemap and robots calls 
#These files are usually in root, but for Flask projects must
#be in the static folder
@bp.route('/robots.txt')
@bp.route('/sitemap.xml')
def static_from_root():
    return send_from_directory(current_app.static_folder, request.path[1:])


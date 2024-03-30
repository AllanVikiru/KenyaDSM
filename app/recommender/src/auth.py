import functools
from flask import current_app
from flask import (
    Blueprint, g, redirect, request, session
)
import os

bp = Blueprint('auth', __name__, url_prefix='/auth')

#IMPORTANT! Called for every request
@bp.before_app_request
def pre_operations(): 

    #ALL STATIC REQUESTS BYPASS!!!
    if request.endpoint == 'static':
        return

    #REDIRECT http -> https in HEROKU
    if 'DYNO' in os.environ:
        current_app.logger.critical("DYNO ENV !!!!")
        if request.url.startswith('http://'):
            url = request.url.replace('http://', 'https://', 1)
            code = 301
            return redirect(url, code=code)


    g.policyCode = -1 #SET DEFAULT INDEPENDENTLY TO WRAPPER
    policyCode = session.get("cookie-policy")
    #possible values Null -> no info, 0 -> Strict, 1 -> Minimal, 
    #                                 2 -> Analisys, 3 -> All
    if policyCode !=None:
        g.policyCode = policyCode
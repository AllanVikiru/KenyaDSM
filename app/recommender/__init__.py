import os

from flask import Flask
from .src.jinjafilters import *
from .src.errorhandlers import *

def create_app():
    # create and configure the app
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY=os.environ['SESSION_SECRET'],
    )

    #ADDS HANDLER TO CLOSE DATABASE AT END OF SESSION!
    from .src import db_connect
    db_connect.init_app(app)

    from . import routes
    app.register_blueprint(routes.bp)

    from .src import modals
    app.register_blueprint(modals.bp)

    #Add other blueprints if needed
    from .src import auth
    app.register_blueprint(auth.bp)

    #ADDS HANDLER FOR ERRORs
    app.register_error_handler(500, error_500)
    app.register_error_handler(404, error_404)

    #JINJA FILTERS
    app.jinja_env.filters['slugify'] = slugify
    app.jinja_env.filters['displayError'] = displayError 
    app.jinja_env.filters['displayMessage'] = displayMessage

    return app
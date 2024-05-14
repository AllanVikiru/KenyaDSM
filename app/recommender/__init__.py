import os

from flask import Flask
from .src import db_connect
from . import routes

def create_app(): # create and configure app
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY=os.environ['SESSION_SECRET'],
    )
    db_connect.init_app(app)
    app.register_blueprint(routes.bp)

    return app
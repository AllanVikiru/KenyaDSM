import os

from flask import Flask

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

    return app
import psycopg2
from flask import g

def get_db():
    db = getattr(g,'_database', None)
    dbname = 'ke_foods'
    user = 'postgres'
    password = 'postgres'
    host = 'localhost'
    port = '5432'
    sslmode = None

    db = g._database = psycopg2.connect(
            dbname=dbname,
            user=user,
            password=password,
            host=host,
            port=port,
            sslmode=sslmode)
    return db

# close database
def close_db(e=None):
    db = g.pop('_database', None)
    if db is not None:
        db.close()

def init_app(app):
    app.teardown_appcontext(close_db)

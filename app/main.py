from recommender import create_app
import os

key = os.urandom(12)

os.environ["SESSION_SECRET"]=str(key)

app = create_app()
print(app.instance_path)
app.run(debug=True)
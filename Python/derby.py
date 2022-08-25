import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import plotly.express as px #graphing
import plotly.graph_objects as go #graphing
from plotly.subplots import make_subplots #graphing
import plotly.figure_factory as ff #graphing
import matplotlib.pyplot as plt #graphing
import seaborn as sns #graphing
import missingno as msno #describe data
import os

colors = ["#FFFFFF", "#6CD4FF", "#F7DF00", "#E60000"]

from plotly.offline import plot, iplot, init_notebook_mode
import plotly.graph_objs as go
init_notebook_mode(connected=True)

plt.rcParams["figure.figsize"] = (12, 8)

for dirname, _, filenames in os.walk('/kaggle/input'):
    for filename in filenames:
        print(os.path.join(dirname, filename))

nyra_tracking = pd.read_csv("/big-data-derby-2022/nyra_tracking_table.csv")
nyra_start = pd.read_csv("/big-data-derby-2022/nyra_start_table.csv")
nyra_race = pd.read_csv("/big-data-derby-2022/nyra_race_table.csv")
nyra_2019 = pd.read_csv("/big-data-derby-2022/nyra_2019_complete.csv")


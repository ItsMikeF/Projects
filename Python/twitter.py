#write a function to call twitter api and return the number of tweets for a given user
import tweepy
import json
import sys
import os
import time
import datetime
import requests
import urllib.request
import urllib.parse
import urllib.error
import base64
import re
import random
import string
import math
import csv
import pandas as pd
import numpy as np

from pandas.io.json import json_normalize

consumer_key = '61108922-fcovElctrgVepoeZURjpYVWjX3bFuGvZVdNkvj2vd'
consumer_secret = 'BJi9x2qjvUDo8hgqqoVySEpyBexaCKUvq1GZHX4y1nA69'

def get_tweets(screen_name):
    auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    api = tweepy.API(auth)
    tweets = api.user_timeline(screen_name = screen_name,count=200)
    return tweets

get_tweets('its_mikef')
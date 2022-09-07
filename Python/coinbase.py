import requests
from tqdm.notebook import tqdm
import datetime

import hmac
import hashlib
import time
import base64
from requests.auth import AuthBase

API_URL = "https://api.pro.coinbase.com/"
API_KEY = "32238cfdd0d5f253df7563c558369239"
API_SECRET = "bfSKL67rAIbm8ya2kdLGb0iBD6tm4ss/CE8G1I6c/zD08edDHQLgYfEyj8nn7CzbXU9Tk4CavjcPmZ6yFJxKzg=="
API_PASS = "jo0geecqg"

class CoinbaseExchangeAuth(AuthBase):
    def __init__(self, api_key, secret_key, passphrase):
        self.api_key = api_key
        self.secret_key = secret_key
        self.passphrase = passphrase

    def __call__(self, request):
        timestamp = str(time.time())
        message = timestamp + request.method + request.path_url + (request.body or '')
        hmac_key = base64.b64decode(self.secret_key)
        signature = hmac.new(hmac_key, message.encode('ascii'), hashlib.sha256)
        signature_b64 = signature.digest().encode('base64').rstrip('\n')
        request.headers.update({
            'CB-ACCESS-SIGN': signature_b64,
            'CB-ACCESS-TIMESTAMP': timestamp,
            'CB-ACCESS-KEY': self.api_key,
            'CB-ACCESS-PASSPHRASE': self.passphrase,
            'Content-Type': 'application/json'
        })
        return request

AUTH = CoinbaseExchangeAuth(API_KEY, API_SECRET, API_PASS)
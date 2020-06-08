from scrapy import Spider, Request
from bonapp.items import BonappItem
import re

class MovadoSpider(Spider):

    name = "movado_spider"
    allowed_domains = ['www.movado.com']
    start_urls = ['https://www.movado.com/us/en/shop-watches/shop-all-watches/?start=0&sz=351']


    def parse(self, response):
from scrapy import Spider, Request
from nordstrom_spider.items import NordstromSpiderItem
import re

class NordstromSpider(Spider):

    name = "nordstrom_spider"
    allowed_domains = ['shop.nordstrom.com/']
    start_urls = ['https://shop.nordstrom.com/sr?origin=keywordsearch&keyword=movado']


    def parse(self, response):

        # create search page list 
        page_count = response.xpath()
        page_urls = [f'https://shop.nordstrom.com/sr?origin=recentsearches1&keyword=movado&page={page}' for page in page_count]
        for url in page_urls:
            yield Request(url=url, callback=self.parse_page_results)

    def parse_page_results(self, response):

        # collect urls for every product on search results page
        product_partial_xpaths = response.xpath()
        product_urls = [f'https://shop.nordstrom.com{lnk}' for lnk in product_partial_xpaths]
        for url in product_urls:
            yield Request(url=url, callback=self.parse_product_page)

    def parse_product_page(self, response):

    	watch_model = response.xpath()
        in_stock = response.xpath()
        price = response.xpath()

        # collect group of various watch details
        dial = ""
        crystal = ""
        movement = ""
        case_diameter = ""
        water_resistance = ""
        gender = ""
        case_material = ""
        strap = ""
        bracelet = ""



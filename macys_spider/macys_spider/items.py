# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class MacysSpiderItem(scrapy.Item):
    watch_model = scrapy.Field()
    sale = scrapy.Field()
    price = scrapy.Field()
    description = scrapy.Field()
    bulleted_details = scrapy.Field()
    rating = scrapy.Field()
    review_count = scrapy.Field()
    review_text = scrapy.Field()
    url = scrapy.Field()
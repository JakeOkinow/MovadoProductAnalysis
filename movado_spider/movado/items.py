# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class MovadoItem(scrapy.Item):
    watch_model = scrapy.Field()
    in_stock = scrapy.Field()
    price = scrapy.Field()
    dial = scrapy.Field()
    crystal = scrapy.Field()
    movement = scrapy.Field()
    case_diameter = scrapy.Field()
    water_resistance = scrapy.Field()
    gender = scrapy.Field()
    case_material = scrapy.Field()
    strap = scrapy.Field()
    bracelet = scrapy.Field()
    online_exclusive = scrapy.Field()

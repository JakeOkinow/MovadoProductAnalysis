from scrapy import Spider, Request
from movado.items import MovadoItem
import re

class MovadoSpider(Spider):

    name = "movado_spider"
    allowed_domains = ['www.movado.com']
    start_urls = ['https://www.movado.com/us/en/shop-watches/shop-all-watches/?start=0&sz=48']


    def parse(self, response):

        # create search page with all products loaded for scraping 
        result_count = response.xpath('//div[@class="result-count"]/span/text()[2]').extract()[0].strip()
        total_products = re.search('\d+', result_count).group(0)
        url = f'https://www.movado.com/us/en/shop-watches/shop-all-watches/?start=0&sz={total_products}'
        yield Request(url=url, callback=self.parse_search_results)

    def parse_search_results(self, response):

        # collect urls for every product on search results page
        product_partial_xpaths = response.xpath('//div[@class="image-container"]/a/@href').extract()
        product_urls = [f'https://www.movado.com{lnk}' for lnk in product_partial_xpaths]
        for url in product_urls:
            yield Request(url=url, callback=self.parse_product_page)

    def parse_product_page(self, response):

        watch_model = response.xpath('//div[@class="col-12 col-sm-6"]//h1/text()').extract_first()

        in_stock = response.xpath('//li[@class="text-uppercase"]/div/text()').extract_first()

        price = response.xpath('//div[@class="col-12 prices-add-to-cart-actions"]\
            //span[@class="sales"]//text()').extract()[0].strip()

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

        details_xpaths = response.xpath('//div[@class="row"]//div[@class="attribute-detail"]') 
        for detail in details_xpaths:
            if detail.xpath('./span[@class="attribute-name"]/text()').extract_first().lower() == "dial":
                dial = detail.xpath('./span[@class="attribute-value"]/text()').extract_first()
            elif detail.xpath('./span[@class="attribute-name"]/text()').extract_first().lower() == "crystal":
                crystal = detail.xpath('./span[@class="attribute-value"]/text()').extract_first()
            elif detail.xpath('./span[@class="attribute-name"]/text()').extract_first().lower() == "movement":
                movement = detail.xpath('./span[@class="attribute-value"]/text()').extract_first()
            elif detail.xpath('./span[@class="attribute-name"]/text()').extract_first().lower() == "case diameter (mm)":
                case_diameter = detail.xpath('./span[@class="attribute-value"]/text()').extract_first()
            elif detail.xpath('./span[@class="attribute-name"]/text()').extract_first().lower() == "water resistance":
                water_resistance = detail.xpath('./span[@class="attribute-value"]/text()').extract_first()
            elif detail.xpath('./span[@class="attribute-name"]/text()').extract_first().lower() == "gender":
                gender = detail.xpath('./span[@class="attribute-value"]/text()').extract_first()
            elif detail.xpath('./span[@class="attribute-name"]/text()').extract_first().lower() == "case material":
                case_material = detail.xpath('./span[@class="attribute-value"]/text()').extract_first()
            elif detail.xpath('./span[@class="attribute-name"]/text()').extract_first().lower() == "strap":
                strap = detail.xpath('./span[@class="attribute-value"]/text()').extract_first()
            elif detail.xpath('./span[@class="attribute-name"]/text()').extract_first().lower() == "bracelet":
                bracelet = detail.xpath('./span[@class="attribute-value"]/text()').extract_first()

        if response.xpath('//div[@class="exclusive-badges"]/span/text()').extract_first():
            online_exclusive = response.xpath('//div[@class="exclusive-badges"]/span/text()').extract_first()
        else:
            online_exclusive = "No"
        lnk = response.request.url
        model_number = lnk[lnk.rfind("-")+1:lnk.rfind(".")]




        item = MovadoItem()
        item["watch_model"] = watch_model
        item["in_stock"] = in_stock
        item["price"] = price
        item["dial"] = dial
        item["crystal"] = crystal
        item["movement"] = movement
        item["case_diameter"] = case_diameter
        item["water_resistance"] = water_resistance
        item["gender"] = gender
        item["case_material"] = case_material
        item["strap"] = strap
        item["bracelet"] = bracelet
        item["online_exclusive"] = online_exclusive
        item["model_number"] = model_number
        item["url"] = lnk

        yield item











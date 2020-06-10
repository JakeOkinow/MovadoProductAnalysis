from scrapy import Spider, Request
from macys_spider.items import MacysSpiderItem
import re

class MacysSpider(Spider):

    name = "macys_spider"
    allowed_domains = ['www.macys.com']
    start_urls = ['https://www.macys.com/shop/featured/movado']


    def parse(self, response):

        # collect number of results and number of pages they're spread across
        results_number = int(response.xpath('//span[@id="productCount"]/text()').extract_first())
        results_showing_perpage = int(response.xpath('//li[@class="selected"]/text()').extract()[-1])
        page_urls = [f'https://www.macys.com/shop/featured/movado/Pageindex/{page_num}' for \
                    page_num in range(1, int(results_number/results_showing_perpage + 2))]

        for url in page_urls:
            yield Request(url=url, callback=self.parse_search_page)
    

    def parse_search_page(self, response):

        product_partial_xpaths = response.xpath('//div[@class="productDescription"]/a/@href').extract()
        product_urls = [f'https://www.macys.com{lnk}' for lnk in product_partial_xpaths]

        for url in product_urls:
            yield Request(url=url, callback=self.parse_product_page)


    def parse_product_page(self, response):

        watch_model = response.xpath('//h1[@class="p-name h3"]//text()').extract_first().strip()
        # in_stock
        price = response.xpath('//div[@data-el="header"]//div[@class="price"]/text()').extract_first().strip()
        description = response.xpath('//div[@class="accordion-body"]\
                                    [@data-el="product-details"]//p/text()').extract_first().strip() 
        bulleted_details = [x for x in response.xpath('//div[@class="accordion-body"]\
                                        [@data-el="product-details"]//ul/li/text()').extract() if x.strip()]
        raw_review_rating = response.xpath('//div[@class="show-for-sr"]/text()').extract_first().strip()
        # not all products are rated/reviewed
        if raw_review_rating == "product don't have any reviews.":
            rating = "None"
            review_count = 0
            review_text = "None"
        else:
            rating = re.search('\d+%', raw_review_rating).group(0) 
            review_count = re.search('\d+$', raw_review_rating).group(0)  
        # review_text






        item = MacysSpiderItem()
        item["watch_model"] = watch_model
        # item["in_stock"] = in_stock
        item["price"] = price
        item["description"] = description
        item["bulleted_details"] = bulleted_details
        item["rating"] = rating
        item["review_count"] = review_count
        # item["review_text"] = review_text

        yield item










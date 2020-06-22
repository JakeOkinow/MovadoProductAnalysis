from scrapy import Spider, Request
from macys_spider.items import MacysSpiderItem
import re
from selenium import webdriver
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import StaleElementReferenceException
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
import time


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

        # MODEL
        watch_model = response.xpath('//h1[@class="p-name h3"]//text()').extract_first().strip()
        
        # DESCRIPTION
        description = response.xpath('//div[@class="accordion-body"]\
                                    [@data-el="product-details"]//p/text()').extract_first().strip() 
        # DETAILS
        bulleted_details = [x for x in response.xpath('//div[@class="accordion-body"]\
                                        [@data-el="product-details"]//ul/li/text()').extract() if x.strip()]
        
        # RATINGS AND REVIEWS
        raw_review_rating = response.xpath('//div[@class="show-for-sr"]/text()').extract_first().strip()
        # not all products are rated/reviewed
        if raw_review_rating == "product don't have any reviews.":
            rating = "None"
            review_count = 0
            review_text_list = ""
        else:
            rating = re.search('\d+%', raw_review_rating).group(0) 
            review_count = re.search('\d+$', raw_review_rating).group(0)  
        
        # PRICE
        try:
            # sale Price
            price = response.xpath('//div[@data-el="header"]//span[@data-auto="sale-price"]/text()').extract_first().strip()
        except:
            # full Price
            price = response.xpath('//div[@data-el="header"]//div[@class="price"]/text()').extract_first().strip()
        
        # WHETHER THERE'S A DISCOUNT CODE
        driver = webdriver.Chrome()
        try:
            driver.get(response.request.url)
            sale = WebDriverWait(driver, 10,ignored_exceptions=StaleElementReferenceException)\
                                .until(EC.presence_of_element_located((By.XPATH, '//div[@data-el="badges"]//div//span'))).get_attribute("textContent")
            # driver.find_element_by_xpath('//div[@data-el="badges"]//div//span').get_attribute("textContent")
        except:
            sale = ""
        # REVIEW TEXT
        if review_count != 0:
            try:
                scroll_height = 0
                review_text_list = []
                search_for_reviews = True
                while search_for_reviews:
                    scroll_height += 350
                    time.sleep(.3)
                    driver.execute_script("window.scrollTo(0, " + str(scroll_height) + ");")
                    new_height = driver.execute_script("return document.body.scrollHeight")
                    if new_height < scroll_height:
                        while True:
                            review_titles = WebDriverWait(driver, 10,ignored_exceptions=StaleElementReferenceException)\
                                .until(EC.presence_of_all_elements_located((By.XPATH, '//span[@class="BVRRValue BVRRReviewTitle"]')))
                            review_texts = WebDriverWait(driver, 10,ignored_exceptions=StaleElementReferenceException)\
                                .until(EC.presence_of_all_elements_located((By.XPATH, '//span[@class="BVRRReviewText"]')))
                            if len(review_titles) == len(review_texts):
                                review_text_list.extend(list(map(lambda t: ", ".join([t[0].text, t[1].text]), zip(review_titles, review_texts))))
                            else:
                                review_text_list.extend(list(map(lambda t: t.text, review_texts)))
                            try:
                                next_review_page = WebDriverWait(driver, 1,ignored_exceptions=StaleElementReferenceException)\
                                        .until(EC.presence_of_element_located((By.XPATH, '//a[@title="next"]')))
                                driver.execute_script("arguments[0].scrollIntoView();", next_review_page)
                                driver.execute_script("arguments[0].click();", next_review_page)
                                scroll_height = 0
                            except:
                                search_for_reviews = False
                                break
            except:
                pass
        driver.close()




        item = MacysSpiderItem()
        item["watch_model"] = watch_model
        item["sale"] = sale
        item["price"] = price
        item["description"] = description
        item["bulleted_details"] = bulleted_details
        item["rating"] = rating
        item["review_count"] = review_count
        item["review_text"] = review_text_list
        item["url"] = response.request.url

        yield item








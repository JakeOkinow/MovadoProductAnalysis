from scrapy import Spider, Request
from amazon.items import AmazonItem

class AmazonSpider(Spider):
    name = 'amazon_spider'
    allowed_domains = ['www.amazon.com']
    start_urls = ['https://www.amazon.com/s?k=movado&rh=p_89%3AMovado&dc&page=1&qid=1591649521&rnid=2528832011&ref=sr_pg_1']


    def parse(self, response):
        # Find the total number of pages in the result so that we can decide how many urls to scrape next
        text = response.xpath('//ul[@class="a-pagination"]/li[6]/text()').get()
        print(text)
#        _, per_page, total = map(lambda x: int(x), re.findall('\d+', text))
#        number_pages = total // per_page

        # List comprehension to construct all the urls
#        result_urls = ['https://www.amazon.com/s?k=movado&rh=p_89%3AMovado&dc&page={x}&qid=1591649521&rnid=2528832011&ref=sr_pg_{x}'.format(x) for x in range(1,number_pages+1)]

        result_urls = ['https://www.amazon.com/s?k=movado&rh=p_89%3AMovado&dc&page={}&qid=1591649521&rnid=2528832011&ref=sr_pg_{}'.format(x, x) for x in range(1, int(text)+1)]
        #result_urls = ['https://www.amazon.com/s?k=movado&rh=p_89%3AMovado&dc&page={}&qid=1591649521&rnid=2528832011&ref=sr_pg_{}'.format(1, 1)]

        # price_seller = 'https://www.amazon.com/gp/offer-listing/{}/ref=dp_olp_new_center?ie=UTF8&condition=new'.format('B07Z8J5C95')
        # yield Request(url = price_seller, callback =self.parse_seller_page)

        # Yield the requests to different search result urls,
        # using parse_result_page function to parse the response.
        for url in result_urls:
            yield Request(url=url, callback=self.parse_result_page)

    def parse_result_page(self, response):
        # This fucntion parses the search result page.

        # We are looking for url of the detail page.

        #print(response.text)
        print()
        print(response.xpath('/html/body/div[1]/div[2]/div[1]/div[1]/div/span[3]/div[2]/div[3]/div/span/div/div/div[2]/h2/a/span/text()').get())

        all_detail_urls = response.xpath('//span[@data-component-type="s-product-image"]/a/@href').getall()

        print('hello')
        print(len(all_detail_urls))
        print(all_detail_urls)
        # Yield the requests to the details pages,
        # using parse_detail_page function to parse the response.
        sponsor_urls = []
        detail_urls = []
        detail_urls = all_detail_urls


        for url in ['https://www.amazon.com{}'.format(x) for x in detail_urls]:
            yield Request(url=url, callback=self.parse_detail_page)

    def parse_detail_page(self, response):
        # Product name
        product = response.xpath('//span[@id="productTitle"]/text()').get()
        # code number
        code = response.xpath('//td[@class="a-span7 a-size-base"]/text()').getall()
        code = code[1]
        #asin number
        asin = response.xpath('//ul[@class="a-unordered-list a-nostyle a-vertical a-spacing-none"]/li[4]/span/span[2]/text()').get()

        #model_number = response.request.url
        #model_number = model_number[model_number.rfind("-")+1:model_number.rfind(".")]
        #stars
        star = response.xpath('//span[@class="a-icon-alt"]/text()').get()
        #number of reviews
        rev_count = response.xpath('//span[@id="acrCustomerReviewText"]/text()').get()
        #number of questions
        q_count = response.xpath('//a[@id="askATFLink"]/span/text()').get()

        url = response.request.url
        print("------------------")
        print(product)
        print(code)
        print(star)
        print(rev_count)
        print(q_count)
        print("------------------")

        price_seller = 'https://www.amazon.com/gp/offer-listing/{}/ref=dp_olp_new_center?ie=UTF8&condition=new'.format(asin)

        yield Request(url=price_seller, callback=self.parse_seller_page, meta=dict(code = code, star = star, rev_count = rev_count, q_count = q_count, url = url))

        # Number of answered question for each product
    #     q_and_a_url = 'https://www.amazon.com/ask/questions/asin/{x}/1/ref=ask_ql_psf_ql_hza'.format(asin)
    #
    #     # Total number of reviews
    #     try:
    #         num_reviews = response.xpath('//*[@id="acrCustomerReviewText"]/text()').extract()[1]
    #         num_reviews = int(num_reviews[:-8])
    #     except IndexError:
    #         # There is no review
    #         return
    #
    #     # The link to the reviews page.
    #     first_part, second_part = response.url.split('site')
    #     first_part = first_part + 'site/reviews'
    #     second_part = second_part.split('.')[0]
    #     first_review_page = first_part + second_part
    #
    #     # If there are some fields of data that you have to piggyback to the next level, you can pass them as a dictionary
    #     # as the meta parameter of the Request method.
    #
    #     review_urls = [first_review_page + '?page={}'.format(i) for i in range(1, num_reviews//20+1)]
    #     for url in review_urls[:1]:
    #         yield Request(url=url, meta={'q_and_a': q_and_a, 'product': product},
    #                 callback=self.parse_review_page)
    #

    def parse_seller_page(self, response):

        price = response.xpath('//span[@class="a-size-large a-color-price olpOfferPrice a-text-bold"]/text()').getall()
        sellergrab = response.xpath('//h3[@class="a-spacing-none olpSellerName"]').getall()
        seller = []
        for s in sellergrab:
            if "</a>" in s:
                temp = s[:s.find("</a>")]
                temp = temp[temp.rfind(">")+1:]
                seller.append(temp)
            else:
                seller.append("Amazon")

        product = response.xpath('//div[@id="olpProductDetails"]/h1/text()').getall()
        product = product[1].strip()
        code = response.meta['code']
        star = response.meta['star']
        rev_count = response.meta['rev_count']
        q_count = response.meta['q_count']
        url = response.meta['url']
        price = list(map(str.strip, price))
        print(product)
        print(code)
        print(price)
        print(seller)

        for i in range(len(price)):
            item = AmazonItem()
            item['product'] = product
            item['code'] = code
            item['star'] = star
            item['rev_count'] = rev_count
            item['q_count'] = q_count
            item['price'] = price[i]
            item['seller'] = seller[i]
            item['url'] = url
            yield item


    # def parse_q_and_a(self, response):
    #     #get all the questions on the page
    #     quest = response.xpath('//*[@id="a-page"]/div[1]/div[6]/div/div').getall()
    #     quest_urls = []
    #     # for each question get the page reference
    #     for ques in quest:
    #         quest_urls.append(ques.xpath('//*[@id="question-Tx117OFQSSKJN01"]/div/div[2]/a/href').get()
    #
    #
    #     try:
    #         response.xpath('//*[@id="a-autoid-48-announce"]')
    #     except:
    #         q_and_a = response.xpath('//*[@id="ask-btf-container"]/div/div/div[2]/span/div').getall()
    #         for que in q_and_a:
    #             vote = que.xpath('//*[@id="ask-btf-container"]/div/div/div[2]/span/div/div[1]/div/div[1]/ul/li[2]/span[1]/text()').get()
    #             question = que.xpath('//*[@id="question-Tx112EHUU7RCBM3"]/div/div[2]/a/span/text()').get()
    #
    #
    # def parse_q_and_answer(self, response):
    #     #get all answers
    #     answers = response.xpath('//*[@id="a-page"]/div[1]/div[3]').getall()
    #
    #     #get question and date asked
    #     question = response.xpath('//*[@id="a-page"]/div[1]/div[1]/div[2]/p[1]/span/text()').get()
    #     askdate = response.xpath('//*[@id="a-page"]/div[1]/div[1]/div[2]/p[2]/text()').get()
    #     #for each answer get answer, user, seller info, and date
    #     for ans in answers:
    #         answer = ans.xpath('//*[@id="answer-MxV8D7ANNS1K7D"]/span[1]/span[2]/text()').get()
    #         user = ans.xpath('//*[@id="answer-MxV8D7ANNS1K7D"]/div[1]/div/div[2]/span/text()').get()
    #         seller = ans.xpath('//*[@id="answer-MxV8D7ANNS1K7D"]/div[1]/a/text()').get()
    #         date = ans.xpath('//*[@id="answer-MxV8D7ANNS1K7D"]/div[1]/span/text()').get()
    #
    # def parse_q_and_a_page(self, response, asin):
    #
    #     text = response.xpath('//*[@id="askPaginationBar"]/ul/li[5]/a/text()').get()
    #     _, per_page, total = map(lambda x: int(x), re.findall('\d+', text))
    #     number_pages = total // per_page
    #
    #     # List comprehension to construct all the urls
    #     result_urls = ['https://www.amazon.com/ask/questions/asin/{asin}/{x}/ref=ask_ql_psf_ql_hza'.format(x, asin) for x in range(1,text)]
    #
    #     # Yield the requests to different search result urls,
    #     # using parse_result_page function to parse the response.
    #     for url in result_urls[:1]:
    #         yield Request(url=url, callback=self.parse_q_and_a)
    #
    # def parse_review(self, response):
    #     #get all the questions on the page
    #     quest = response.xpath('//*[@id="cm_cr-review_list"]').getall()
    #     quest_urls = []
    #     # for each question get the page reference
    #     for ques in quest:
    #         quest_urls.append(ques.xpath('//*[@id="customer_review-R2B4LK9NSKHB4N"]/div[2]/a[2]href').get()
    #
    #
    #     try:
    #         response.xpath('//*[@id="a-autoid-48-announce"]')
    #     except:
    #         q_and_a = response.xpath('//*[@id="ask-btf-container"]/div/div/div[2]/span/div').getall()
    #         for que in q_and_a:
    #             vote = que.xpath('//*[@id="ask-btf-container"]/div/div/div[2]/span/div/div[1]/div/div[1]/ul/li[2]/span[1]/text()').get()
    #             question = que.xpath('//*[@id="question-Tx112EHUU7RCBM3"]/div/div[2]/a/span/text()').get()
    #
    #
    # def parse_review_answer(self, response):
    #     #get all comments
    #     comments = response.xpath('//*[@id="R2B4LK9NSKHB4N"]').getall()
    #
    #     #get question and date asked
    #
    #     user = response.xpath('//*[@id="customer_review-R2B4LK9NSKHB4N"]/div[1]/a/div[2]/span/text()').get()
    #     stars = response.xpath('//*[@id="customer_review-R2B4LK9NSKHB4N"]/div[2]/a[1]/i/span/text()').get()
    #     title = response.xpath('//*[@id="customer_review-R2B4LK9NSKHB4N"]/div[2]/a[2]/span/text()').get()
    #     reviewdate = response.xpath('//*[@id="customer_review-R2B4LK9NSKHB4N"]/span/text()').get()
    #     purchase = response.xpath('//*[@id="customer_review-R2B4LK9NSKHB4N"]/div[3]/span/a/span/text()').get()
    #
    #     comment = response.xpath('//*[@id="customer_review-R2B4LK9NSKHB4N"]/div[4]/span/span/text()').get()
    #     helpful = response.xpath('//*[@id="customer_review-R2B4LK9NSKHB4N"]/div[5]/div/span[1]/div[1]/span/text()').get()
    #     #for each answer get answer, user, seller info, and date
    #     for comm in comments:
    #         answer = ans.xpath('//*[@id="R2B4LK9NSKHB4N"]/div[1]/div[2]/span/text()').get()
    #         user = ans.xpath('//*[@id="R2B4LK9NSKHB4N"]/div[1]/div[1]/div/div[1]/a/text()').get()
    #         #seller = ans.xpath('//*[@id="answer-MxV8D7ANNS1K7D"]/div[1]/a/text()').get()
    #         date = ans.xpath('//*[@id="R2B4LK9NSKHB4N"]/div[1]/div[1]/div/div[1]/span[3]/text()').get()
    #
    # def parse_review_page(self, response, asin):
    #
    #     text = response.xpath('//*[@id="filter-info-section"]/span/text()').get()
    #     _, per_page, total = map(lambda x: int(x), re.findall('\d+', text))
    #     number_pages = total // per_page
    #
    #     # List comprehension to construct all the urls
    #     result_urls = ['https://www.amazon.com/product-reviews/{asin}/ref=cm_cr_arp_d_paging_btm_next_{x}?ie=UTF8&reviewerType=all_reviews&pageNumber={x}'.format(x, asin) for x in range(1,number_pages+1)]
    #
    #     # Yield the requests to different search result urls,
    #     # using parse_result_page function to parse the response.
    #     for url in result_urls[:1]:
    #         yield Request(url=url, callback=self.parse_q_and_a)
    #
    #
    #
    #
    # def parse_review_page(self, response):
    #     # Retrieve the first reviews from meta
    #     q_and_a = response.meta['q_and_a']
    #     product = response.meta['product']
    #
    #     # Find all the review tags
    #     reviews = response.xpath('//li[@class="review-item"]')
    #
    #     for review in reviews:
    #         user = review.xpath('.//div[@class="ugc-author v-fw-medium body-copy-lg"]/strong/text()').extract_first()
    #         rating = review.xpath('.//p[@class="sr-only"]/text()').extract_first()
    #         rating = int(re.findall('\d+', rating)[0])
    #         title = review.xpath('.//h4[@class="ugc-review-title c-section-title heading-5 v-fw-medium  "]/text()').extract_first()
    #         text = review.xpath('.//div[@class="ugc-review-body body-copy-lg"]//p/text()').extract_first()
    #         try:
    #             helpful = review.xpath('.//button[@data-track="Helpful"]/text()').extract()[1]
    #         except IndexError:
    #             helpful = ""
    #         try:
    #             unhelpful = review.xpath('.//button[@data-track="Unhelpful"]/text()').extract()[1]
    #         except IndexError:
    #             unhelpful = ""
    #
    #         item = BestbuyItem()
    #         item['user'] = user
    #         item['rating'] = rating
    #         item['title'] = title
    #         item['text'] = text
    #         item['helpful'] = helpful
    #         item['unhelpful'] = unhelpful
    #         item['product'] = product
    #         item['q_and_a'] = q_and_a
    #
    #         yield item
    #
    #

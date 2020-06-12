from selenium import webdriver
import time
import re
import csv
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys

# Start at Nordstrom's home page and execute search for "movado"
driver = webdriver.Chrome()
driver.get("https://shop.nordstrom.com/")
time.sleep(2)
driver.find_element_by_id("keyword-search-desktop").click()
search_request = driver.find_element_by_id("keyword-search-input")
search_request.send_keys("movado")
search_request.submit()

csv_file = open('nordstrom_website.csv', 'w', encoding='utf-8', newline='')
writer = csv.writer(csv_file)
writer.writerow(['watch_model', 'color', 'in_stock', 'price', 'description', 'bullet_details', 'review_count', 'rating', 'review_text'])

popupad_acknowledged = False

product_urls = []
index = 1
while True:
    index = index + 1
    # if there are no watches/products on the page, end loop
    if not len(driver.find_elements_by_xpath('//article[@class="_1AOd3 QIjwE"]/div/a')):
        break
    # wait for all watches to load
    wait_product = WebDriverWait(driver, 10)
    product_xpath_list = wait_product.until(EC.presence_of_all_elements_located((By.XPATH,
                                '//article[@class="_1AOd3 QIjwE"]/div/a')))
    # Iterate through the xpath list and collect each watch's url
    for partial_xpath in product_xpath_list:
        product_urls.append(partial_xpath.get_attribute("href"))
    # update driver's url to next search results page, allowing while loop to gather more product urls
    try:
        if index == 2:  
            driver.get("".join([driver.current_url, "&page=", str(index)]))
        else:
            driver.get("".join([driver.current_url[:-1], str(index)]))
    except:
        break

# from list of all pruduct urls, scrape each product's page
for url in product_urls:
    driver.get(url)
    # collect all possible colors/variations of watch on page
    elements = driver.find_elements_by_xpath('//ul[@id="product-page-swatches"]//button')
    if not elements:
        elements = driver.find_elements_by_xpath('//div[@class="_3nIoM _11U2i"]//button')
    # try select color, or close pop up ad then select
    for button in elements:
        try:
            button.click()
        except:
            driver.find_element_by_xpath('//a[@aria-label="No thanks"]').click()
            popupad_acknowledged = True
            button.click()

        product_dict = {}

        # WATCH MODEL
        watch_model = driver.find_element_by_xpath('//h1[@itemprop="name"]').text
        # COLOR
        color = re.sub("selected | color", "", button.find_element_by_xpath('./img').get_attribute("alt"))
        # IN STOCK
        try:
            in_stock = driver.find_element_by_xpath('//div[@id="selling-essentials"]//div//h3').get_attribute("textContent")
        except:
            in_stock = "Yes"
        # PRICE
        if in_stock == "Yes":
            price = driver.find_element_by_xpath('//span[@id="current-price-string"]').text
        else:
            price = "NA"
        # DESCRIPTION AND BULLETED DETAILS
        description = driver.find_element_by_xpath('//div[@class="_3LvFj"]//p').text
        bullet_details = driver.find_element_by_xpath('//ul[@class="_1D4Qk"]').text
        # REVIEW COUNT
        try:
            review_count = driver.find_element_by_class_name("_2cm3y").get_attribute("textContent")
        except:
            review_count = 0
        # RATING
        try:
            rating = driver.find_element_by_xpath('//span[@itemprop="ratingValue"]').get_attribute("textContent")
        except:
            rating = "NA"
        # REVIEW TEXT
        scroll_height = 0
        review_text = []
        while True:
            scroll_height += 500
            time.sleep(.5)
            driver.execute_script("window.scrollTo(0, " + str(scroll_height) + ");")
            new_height = driver.execute_script("return document.body.scrollHeight")
            if new_height < scroll_height:
                break
        time.sleep(2)
        reviews = driver.find_elements_by_xpath('//div[@class="_13VE3"]')
        for review in reviews:
            review_text.append(review.text)
        html = driver.find_element_by_tag_name('html')
        html.send_keys(Keys.UP)


        product_dict['watch_model'] = watch_model
        product_dict['color'] = color
        product_dict['in_stock'] = in_stock
        product_dict['price'] = price
        product_dict['description'] = description
        product_dict['bullet_details'] = bullet_details
        product_dict['review_count'] = review_count
        product_dict['rating'] = rating
        product_dict['review_text'] = review_text

        writer.writerow([product_dict['watch_model'], product_dict['color'], product_dict['in_stock'], product_dict['price'], \
            product_dict['description'], product_dict['bullet_details'], product_dict['review_count'], product_dict['rating'], \
            product_dict['review_text']])


csv_file.close()
driver.quit()








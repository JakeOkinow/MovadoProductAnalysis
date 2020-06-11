from selenium import webdriver
import time
import re
import csv
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

# Start at Nordstrom's home page and execute search for "movado"
driver = webdriver.Chrome()
driver.get("https://shop.nordstrom.com/")
time.sleep(2)
driver.find_element_by_id("keyword-search-desktop").click()
search_request = driver.find_element_by_id("keyword-search-input")
search_request.send_keys("movado")
search_request.submit()

# starting directly with a search url is not reliable
# driver.get("https://shop.nordstrom.com/sr?origin=recentsearches1&keyword=movado")

csv_file = open('nordstrom_website.csv', 'w', encoding='utf-8', newline='')
writer = csv.writer(csv_file)

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
        elements = "1"
    
    # select watch color, or close pop up ad
    for button in elements:
        # time.sleep(1)
        try:
            if elements != "1":
                button.click()
        except:
            driver.switch_to.alert.dismiss()
#           driver.switch_to.alert.find_element_by_xpath('//a[@aria-label="No thanks"]').click()
        finally:
            if elements != "1":
                button.click()

        product_dict = {}

        watch_model = driver.find_element_by_xpath('//h1[@itemprop="name"]').text
        try:
            in_stock = driver.find_element_by_xpath('//div[@id="selling-essentials"]//div//h3').get_attribute("textContent")
        except:
            in_stock = "Yes"
        if in_stock == "Yes":
            price = driver.find_element_by_xpath('//span[@id="current-price-string"]').text
        else:
            price = "NA"
        description = driver.find_element_by_xpath('//div[@class="_3LvFj"]//p').text
        bullet_details = driver.find_element_by_xpath('//ul[@class="_1D4Qk"]').text
        try:
            review_count = driver.find_element_by_class_name("_2cm3y").get_attribute("textContent")
        except:
            review_count = 0
        try:
            rating = driver.find_element_by_xpath('//span[@itemprop="ratingValue"]').get_attribute("textContent")
        except:
            rating = "NA"
        try:
            driver.switch_to.alert.dismiss()
        except:
            pass


        product_dict['watch_model'] = watch_model
        product_dict['in_stock'] = in_stock
        product_dict['price'] = price
        product_dict['description'] = description
        product_dict['bullet_details'] = bullet_details
        product_dict['review_count'] = review_count
        product_dict['rating'] = rating

        writer.writerow(product_dict.values())


csv_file.close()
driver.quit()








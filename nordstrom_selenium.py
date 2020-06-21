from selenium import webdriver
import time
import re
import csv
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import StaleElementReferenceException
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
writer.writerow(['watch_model', 'model_number', 'color', 'in_stock', 'price', 'description', 'bullet_details', 'review_count', 'rating', 'review_text', 'url'])

popupad_acknowledged = False

product_urls = []
index = 1
while True:
    index = index + 1
    # if there are no watches/products on the page, end loop
    if not len(driver.find_elements_by_xpath('//article[@class="_1AOd3 QIjwE"]/div/a')):
        break
    # wait for all watches to load
    product_xpath_list = WebDriverWait(driver, 10).until(EC.presence_of_all_elements_located((By.XPATH,
                                '//article[@class="_1AOd3 QIjwE"]/div/a')))
    # Iterate through the xpath list and collect each watch's url
    for partial_xpath in product_xpath_list:
        product_urls.append(partial_xpath.get_attribute("href"))
    # update driver's url to next search result's page, allowing while loop to gather more product urls
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
    product_dict = {}

    # WATCH MODEL
    watch_model = driver.find_element_by_xpath('//h1[@itemprop="name"]').text

    # MODEL NUMBER
    model_number = re.findall("\d{4}\d*", url)[0]
    
    # DESCRIPTION AND BULLETED DETAILS
    description = driver.find_element_by_xpath('//div[@class="_3LvFj"]//p').text
    bullet_details = driver.find_element_by_xpath('//ul[@class="_1D4Qk"]').text
    if bullet_details == False:
        driver.execute_script("window.scrollTo(0, " + '300' + ");")
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
    search_for_reviews = True
    while search_for_reviews and review_count != 0:
        # scroll slowly through page for all elements to load
        scroll_height += 300
        time.sleep(.5)
        driver.execute_script("window.scrollTo(0, " + str(scroll_height) + ");")
        new_height = driver.execute_script("return document.body.scrollHeight")
        # when scrolling height exceeps page height, scrape reviews
        if new_height < scroll_height:
            while True:
                reviews = WebDriverWait(driver, 2,ignored_exceptions=StaleElementReferenceException)\
                                .until(EC.presence_of_all_elements_located((By.XPATH, '//div[@class="_13VE3"]')))
                for review in reviews:
                    review_text.append(review.text)
                # if a "next review page" button exists, click and rescrape reviews
                try:
                    next_review_page = WebDriverWait(driver, 2,ignored_exceptions=StaleElementReferenceException)\
                            .until(EC.presence_of_element_located((By.XPATH, '//li[@class="_27MWr _9EQqw"]/a')))
                    driver.execute_script("arguments[0].scrollIntoView();", next_review_page)
                    nextbutton=WebDriverWait(driver,10).until(EC.element_to_be_clickable((By.XPATH,"//li[@class='_27MWr _9EQqw']/a/span[text()='Next']")))
                    # the popup ad can interrupt clicking, so first check in case it hasn't been acknowledged
                    if popupad_acknowledged == False:
                        try:
                            driver.execute_script("arguments[0].click();", nextbutton)
                        except:
                            driver.find_element_by_xpath('//a[@aria-label="No thanks"]').click()
                            popupad_acknowledged = True
                            driver.execute_script("arguments[0].click();", nextbutton)
                    else:
                        driver.execute_script("arguments[0].click();", nextbutton)
                    scroll_height = 0
                    break
                except:
                    search_for_reviews = False
                    break
    
    # collect all possible colors/variations of watch on page
    elements = driver.find_elements_by_xpath('//ul[@id="product-page-swatches"]//button')
    if not elements:
        elements = driver.find_elements_by_xpath('//div[@class="_3nIoM _11U2i"]//button')
    # try selecting color, or address pop up ad and then select
    for button in elements:
        html = driver.find_element_by_tag_name('html')
        html.send_keys(Keys.UP)
        try:
            button.click()
        except:
            driver.find_element_by_xpath('//a[@aria-label="No thanks"]').click()
            popupad_acknowledged = True
            button.click()
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



        product_dict['watch_model'] = watch_model
        product_dict['model_number'] = model_number
        product_dict['color'] = color
        product_dict['in_stock'] = in_stock
        product_dict['price'] = price
        product_dict['description'] = description
        product_dict['bullet_details'] = bullet_details
        product_dict['review_count'] = review_count
        product_dict['rating'] = rating
        product_dict['review_text'] = review_text
        product_dict['url'] = url

        writer.writerow([product_dict['watch_model'], product_dict['model_number'], product_dict['color'], product_dict['in_stock'], product_dict['price'], \
            product_dict['description'], product_dict['bullet_details'], product_dict['review_count'], product_dict['rating'], \
            product_dict['review_text'], product_dict['url']])


csv_file.close()
driver.quit()








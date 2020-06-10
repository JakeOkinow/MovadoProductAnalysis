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

# driver.get("https://shop.nordstrom.com/sr?origin=recentsearches1&keyword=movado")

csv_file = open('nordstrom.csv', 'w', encoding='utf-8', newline='')
writer = csv.writer(csv_file)

# list of products to scrape
product_url = []

index = 1
while True:
	print("Scraping Page number " + str(index))
	index = index + 1

	if not len(driver.find_elements_by_xpath('//article[@class="_1AOd3 QIjwE"]/div/a')):
		break

	# wait for all watches to load
	wait_product = WebDriverWait(driver, 10)
	product_xpath_list = wait_product.until(EC.presence_of_all_elements_located((By.XPATH,
								'//article[@class="_1AOd3 QIjwE"]/div/a')))
	print(len(product_xpath_list))

	# Iterate through the list and collect each url
	for partial_xpath in product_xpath_list:
		product_url.append(partial_xpath.get_attribute("href"))

	next_page = driver.find_element_by_xpath('//a[@class="_2WIqd"]').get_attribute("href")
	try:
		if index == 2:	
			driver.get("".join([driver.current_url, "&page=", str(index)]))
		else:
			driver.get("".join([driver.current_url[:-1], str(index)]))
	except:
		break

# scrape each product's url
for url in product_url:
	driver.get(url)

	watch_model = driver.find_element_by_xpath('//h1[@itemprop="name"]').text
	in_stock = ""
	price = ""
	dial = ""
	crystal = ""
	movement = ""
	case_diameter = ""
	water_resistance = ""
	gender = ""
	case_material = ""
	strap = ""
	bracelet = ""
	online_exclusive = ""

	print(watch_model)




driver.quit()






		# 	# Once you locate the element, you can use 'element.text' to return its string.
		# 	# To get the attribute instead of the text of each element, use 'element.get_attribute()'
		# 	try:
		# 		title = review.find_element_by_xpath('.//div[@class="NHaasDS75Bd fontSize_12 wrapText"]').text
		# 	except:
		# 		continue


		# 	print('Title = {}'.format(title))

		# 	# OPTIONAL: How can we deal with the "read more" button?
			
		# 	# Use relative xpath to locate text, username, date_published, rating.
		# 	text = review.find_element_by_xpath('.//span[@class=""').text
		# 	score = review.find_element_by_xpath('.//span[@class="a11y-hidden]/span"').text

		# 	# Uncomment the following lines once you verified the xpath of different fields
			
		# 	# review_dict['title'] = title
		# 	# review_dict['text'] = text
		# 	# review_dict['username'] = username
		# 	# review_dict['date_published'] = date_published
		# 	# review_dict['rating'] = rating

		# # We need to scroll to the bottom of the page because the button is not in the current view yet.
		# driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")

		# # Locate the next button element on the page and then call `button.click()` to click it.
		# button = driver.find_element_by_xpath('//li[@class="nextClick displayInlineBlock padLeft5 "]')
		# button.click()
		# time.sleep(2)





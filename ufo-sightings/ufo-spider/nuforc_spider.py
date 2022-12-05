# Import scrapy
import scrapy
# Import the CrawlerProcess to run the spider.
from scrapy.crawler import CrawlerProcess
# Import Pandas to save the data as csv
import pandas as pd
 
  


# Need pyopenssl==22.0.0   
# pip install pyopenssl==22.0.0
# mamba install pyopenssl==22.0.0
# conda install pyopenssl==22.0.0

# Create the Spider class
class Nuforc_Spider(scrapy.Spider):
  name = "nuforc_spider"
  
  # Custom setting value to better performance of the spider class.
  # For more information, see: https://docs.scrapy.org/en/latest/topics/settings.html
  custom_settings = {
        'TWISTED_REACTOR': 'twisted.internet.asyncioreactor.AsyncioSelectorReactor',
        'SCHEDULER_PRIORITY_QUEUE': 'scrapy.pqueues.ScrapyPriorityQueue',
        'CONCURRENT_REQUESTS': 600,
        'CONCURRENT_REQUESTS_PER_DOMAIN': 40,
        'REACTOR_THREADPOOL_MAXSIZE': 40,
        'LOG_LEVEL': 'INFO',
        'COOKIES_ENABLED': False
    }
  
  # Start_requests method #
  def start_requests(self):
    
    url_short = 'https://nuforc.org/webreports/ndxevent.html'
    yield scrapy.Request(url = url_short,
                         callback = self.parse_links)
    
  # First parsing method #
  async def parse_links(self, response):
   
    # Selector to get the links with the data of the Ufo sights.
    ufo_blocks = response.xpath('//tr[contains(@valign, "TOP")]/td')
    ufo_links = ufo_blocks.xpath('./a/@href')
    ufo_dates = ufo_blocks.xpath('./a/text()')
    
    links_to_follow = ufo_links.getall()
    ufo_Date_Table = ufo_dates.getall()
    
    # Parse the links with the Ufo sights to the next Method.    
    for url, date in zip(links_to_follow, ufo_Date_Table):
        yield response.follow(url = url,
                              callback = self.parse_tables,
                              meta = {
                                'Date_Table': date
                              }
                              )
        
        
  # Second parsing method #
  async def parse_tables(self, response):
    
    # store the ufo data into a variable
    Date_Table = response.meta['Date_Table']
    
     
    # Acess the table in the page
    table_body = response.css('tbody tr')
    
    # Default value for the get method.
    get_default_value = 'Unknown'
    
    # Loop the table to extract the data and pass it
    # to the next parse method as a meta dictionary
    for row in table_body:
      # If column is empty returns crawler-not-found
      link = row.css('td a::attr(href)').get(default = get_default_value)
      date = row.css('td a::text').get(default = get_default_value)
      city = row.css('td:nth-child(2)::text').get(default = get_default_value)
      state = row.css('td:nth-child(3)::text').get(default = get_default_value)
      country = row.css('td:nth-child(4)::text').get(default = get_default_value)
      shape = row.css('td:nth-child(5)::text').get(default = get_default_value)
      duration = row.css('td:nth-child(6)::text').get(default = get_default_value)
      summary = row.css('td:nth-child(7)::text').get(default = get_default_value)
      posted = row.css('td:nth-child(8)::text').get(default = get_default_value)
      image = row.css('td:nth-child(9)::text').get(default = "No")
      
      # Yead the link to the next method and use the data as a dictionary
      yield response.follow(url = link,
                            callback = self.parse_text,
                            meta= {
                                  'Date': date,
                                  'Posted': posted,
                                  'City': city,
                                  'State': state,
                                  'Country': country,
                                  'Shape': shape,
                                  'Duration': duration,
                                  'Image': image,
                                  'Link': link,
                                  'Summary': summary,
                                  'Date_Table': Date_Table
                                  }
                            )
      
  # Third parsing methd #   
  async def parse_text(self, response):
    
    # Extract the ufo sights text and save.
    ufo_blocks_text = response.css('tbody tr:nth-child(2) td font ::text')
    ufo_text = ufo_blocks_text.getall()
    ufo_text = [element for element in ufo_text if element != '\n']
    # Join all text into a single string
    ufo_text = ' '.join(ufo_text)

    # store the ufo data into a variable
    ufo_dict = response.meta
    # Add the ufo_text to the dictionary
    ufo_dict['Text'] = ufo_text
    # append the data to the ufo list
    ufo_list.append(ufo_dict)   
    
    
 
### Initialize the ufo report list **outside** of the Spider class ###
ufo_list = []

### Inicializing the Spider class ###
def main():
  # Run the Spider
  process = CrawlerProcess()
  process.crawl(Nuforc_Spider)
  process.start()



if __name__ == "__main__":
  ### Run the Spider ###
  main()
  
  
  # Create a Dataframe with the ufo data and
  ufo_sights = pd.DataFrame(ufo_list)
  # Only save the useful column of the meta dictionary
  ufo_sights = ufo_sights[['Date_Table', 'Date', 'Posted', 'City', 'State', 'Country', 'Shape', 'Duration', 'Image', 'Link', 'Summary', 'Text']]

  # Complement of the link to the sights reports
  link_start = 'https://nuforc.org/webreports/'
  # create a full link to the reports.
  ufo_sights['Link'] = ufo_sights['Link'].apply(lambda link: link_start + link)


  ## Save the Dataframe to a parquet file.
  #print('### Saving the Data as a parquet file ###')
  #ufo_sights.to_parquet("../data/nuforc_reports.parquet", index = False)
  ## Save the Dataframe to a feather file.
  #print('### Saving the Data as a feather file ###')
  #ufo_sights.to_feather('../data/nuforc_reports.feather', index = False)



# Save the Dataframe to a csv file.
  print('### Saving the Data as a csv file ###')
  ufo_sights.to_csv("../data/nuforc_reports.csv", index = False)
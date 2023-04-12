# Define here the models for your scraped items
#
# See documentation in:
# https://docs.scrapy.org/en/latest/topics/items.html

import scrapy


class SerieaItem(scrapy.Item):
    Date = scrapy.Field()
    Home = scrapy.Field()
    Score = scrapy.Field()
    Away = scrapy.Field()
    Half_time = scrapy.Field()
    pass

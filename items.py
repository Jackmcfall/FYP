# Define here the models for your scraped items
#
# See documentation in:
# https://docs.scrapy.org/en/latest/topics/items.html

import scrapy


class SerieaItem(scrapy.Item):
    Day = scrapy.Field()
    Date = scrapy.Field()
    Time = scrapy.Field()
    Home = scrapy.Field()
    Score = scrapy.Field()
    Away = scrapy.Field()
    Attendance = scrapy.Field()
    Venue = scrapy.Field()
    pass





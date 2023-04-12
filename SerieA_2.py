import scrapy
from ..items import SerieaItem


class SerieaSpider(scrapy.Spider):
    name = 'matches'
    start_urls = {'https://www.soccerstats.com/results.asp?league=italy&pmtype=bydate'
                  }

    def start_requests(self):
        urls = ['https://www.soccerstats.com/results.asp?league=italy&pmtype=bydate'
                ]

        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse)

    def parse(self, response, **kwargs):
        rows = response.xpath('//*[@class="odd"]')

        for row in rows:
            items = SerieaItem()

            items['Date'] = row.xpath('.//td[1]//text()').extract()
            items['Home'] = row.xpath('.//td[2]//text()').extract()
            items['Score'] = row.xpath('.//td[3]//text()').extract()
            items['Away'] = row.xpath('.//td[4]//text()').extract()
            items['Half_time'] = row.xpath('.//td[6]//text()').extract()

            yield items

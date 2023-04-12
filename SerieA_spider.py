import scrapy
from ..items import SerieaItem

class SerieaSpider(scrapy.Spider):
    name = 'matches'
    start_urls = {'https://fbref.com/en/comps/11/2018-2019/schedule/2018-2019-Serie-A-Scores-and-Fixtures'
                  }

    def start_requests(self):
        urls = ['https://fbref.com/en/comps/11/2018-2019/schedule/2018-2019-Serie-A-Scores-and-Fixtures']

        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse)

    def parse(self, response, **kwargs):
        rows = response.xpath('//*[@id="all_sched"]//tbody//tr')

        for row in rows:
            items = SerieaItem()

            items['Day'] = row.xpath('.//td[1]//text()').extract_first()
            items['Date'] = row.xpath('.//td[2]//text()').extract_first()
            items['Time'] = row.xpath('.//td[3]//text()').extract_first()
            items['Home'] = row.xpath('.//td[4]//text()').extract_first()
            items['Score'] = row.xpath('.//td[6]//text()').extract_first()
            items['Away'] = row.xpath('.//td[8]//text()').extract_first()
            items['Attendance'] = row.xpath('.//td[9]//text()').extract_first()
            items['Venue'] = row.xpath('.//td[10]//text()').extract_first()

            yield items

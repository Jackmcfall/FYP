# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: https://docs.scrapy.org/en/latest/topics/item-pipeline.html


# useful for handling different item types with a single interface
from itemadapter import ItemAdapter

import csv
import itertools

from SerieA.SerieA_2.SerieA_2 import items


class Seriea2Pipeline(object):
    def process_item(self, item, spider):
        return item

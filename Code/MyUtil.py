# -*- coding: utf-8 -*-
"""
Created on Sat Aug 29 15:31:28 2015

@author: HAN
"""

from datetime import datetime

'''
path
'''
data_path = '/Users/HAN/AllCode/Projects/Coupon/Data/'
input_path = data_path + 'input_uk/'
res_path = data_path + 'result/'



def writeAnswer(result):
#    magic_key = '2fcca928b8b3e9ead0f2cecffeea50c1';
    magic_key = '';
    f = open('./input/sample_submission.csv', 'r')
    line = f.readlines()
    f.close()
    datetime_now = datetime.now().strftime("%m-%d_%H-%M-%S")
    f = open('./result/' +datetime_now  + '_submission_visit.csv', 'w')
    f.write(line[0].strip()+"\n")
    del line[0]
    for l in line:
        l = l.strip()
        user = l.split(',')[0]
        coupons =  result[user]
        if len(coupons) == 0:
            coupon_ids = ","
            coupon_ids += magic_key
        else:
            coupon_ids = "," + coupons[0]
            del coupons[0] 
            for coupon in coupons:
                coupon_ids += " " + coupon
            if len(coupons) < 9:
                coupon_ids += " " + magic_key
        coupon_ids += "\n"
        f.writelines(user + coupon_ids)
    f.close()
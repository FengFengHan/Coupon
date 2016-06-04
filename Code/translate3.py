# -*- coding: utf-8 -*-
"""
Created on Thu Sep  3 20:40:54 2015

@author: Administrator
"""
import pandas as pd

i_path = './input/'
o_path_i = './input_uk/jp2uk/'
o_path = './input_uk/'

'''
file list
'''
files_jp = {'coupon_list_train.csv': ['CAPSULE_TEXT', 'GENRE_NAME', 'large_area_name', 'ken_name', 'small_area_name'],
'coupon_list_test.csv':['CAPSULE_TEXT', 'GENRE_NAME', 'large_area_name', 'ken_name', 'small_area_name'],
'coupon_area_train.csv':['SMALL_AREA_NAME', 'PREF_NAME'],
'coupon_area_test.csv':['SMALL_AREA_NAME', 'PREF_NAME'],
'coupon_detail_train.csv':['SMALL_AREA_NAME'],
'user_list.csv':['PREF_NAME'],
'prefecture_locations.csv':['PREF_NAME', 'PREFECTUAL_OFFICE']
#, 'coupon_visit_train.csv':[]
}


def correct_firCol(df):
    if(not df.columns[0][0].isalpha()):
        col_name = []
        for name in df.columns:
            col_name.append(name)
        col_name[0] = col_name[0][1:]
        df.columns = col_name

'''
output japanese
'''
japanese = set()
for f in files_jp.keys():
   infile = pd.read_csv(i_path + f)
   correct_firCol(infile)
   for col in files_jp[f]:
       japanese = japanese.union(set(infile[col]))
japanese_series = pd.Series(list(japanese))
jp = japanese_series.dropna()
jp.to_csv(o_path_i + 'japanese.csv', index = False, encoding = 'utf-8')

'''
input translations
'''
jp2uk = pd.read_excel(o_path_i + 'jp2uk.xlsx')
translations = {k : v for (k,v) in zip(jp2uk.Japanese, jp2uk.English)}
jp2uk_offical = pd.read_excel(o_path_i + 'jp2uk.xlsx', sheetname = 'jp2uk_2')
translation_offical = {k : v for (k,v) in zip(jp2uk_offical.Japanese_2, jp2uk_offical.English_2)}
for key in translation_offical.keys():
    translations[key] = translation_offical[key]
## check the different japannese be translated to same chinese
### result:beside Beauty, if different japannese be translated to same chinese, 
###    then the different japannese has a include relation in  geography
#translate_op = {}
#for key in translations.keys():
#    value = translations[key]
#    translate_op.setdefault(value, [])
#    translate_op[value].append(key)

    
'''
translate
'''
for f in files_jp.keys():
   infile = pd.read_csv(i_path + f)
   correct_firCol(infile)
   for col in files_jp[f]:
       infile[col] = infile[col].map(translations)
   infile.to_csv(o_path + f.replace('.csv', '_en.csv'), index =False)
        
        



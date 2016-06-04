#from Coupon import util
from util import *
import pandas as pd
import datetime
from time import gmtime, strftime
import matplotlib.pyplot as plt
'''
read file
'''
i_path = './input_uk/'
train_list = pd.read_csv(i_path + 'coupon_list_train_en.csv')
test_list = pd.read_csv(i_path + 'coupon_list_test_en.csv')
user_list = pd.read_csv(i_path + 'user_list_en.csv')
user_list.fillna({'REG_DATE':'2015-01-01 00:00:00', 'WITHDRAW_DATE':'2015-01-01 00:00:00'}, inplace = True)
dateparse = lambda x: pd.datetime.strptime(x, '%Y-%m-%d %H:%M:%S')
user_list[['REG_DATE', 'WITHDRAW_DATE']] = user_list[['REG_DATE', 'WITHDRAW_DATE']].applymap(dateparse)
train_purchase = pd.read_csv(i_path + 'coupon_detail_train_en.csv')
train_purchase['I_DATE'] = train_purchase['I_DATE'].map(dateparse)
train_list_area = pd.read_csv(i_path + 'coupon_area_train_en.csv')
test_list_area = pd.read_csv(i_path + 'coupon_area_test_en.csv')
# validate the same coupon between the train and test
#con_list = pd.merge(train_list, test_list) # None

#validate user
test_date_s = datetime.datetime(2012,6,24,0,0,0)
#around 900(that is 3%) user has Unregistered
#user_list _v = user_list[user_list.WITHDRAW_DATE > test_date_s] 

#visit test coupon
## result trarin_visit has 2833K lines , con_tel_trv only have 741 lines
#train_visit = pd.read_csv(i_path + 'coupon_visit_train_en.csv')  
#con_tel_trv = pd.merge(test_list, train_visit, left_on = 'COUPON_ID_hash', right_on = 'VIEW_COUPON_ID_hash')
#con_tel_trv.to_csv(i_path + 'coupon_visit_train_test.csv')

#if a user visit something, the he will buy it
## result: has 514 user, 625 coupon
con_tel_trv = pd.read_csv(i_path + 'coupon_visit_train_test.csv')
user_view = zip(con_tel_trv.USER_ID_hash, con_tel_trv.VIEW_COUPON_ID_hash)
view_result = {}
for (user_id, view_coupon_id)  in user_view:
   view_result.setdefault(user_id, set()) ### set() sort element auto
   view_result[user_id].add(view_coupon_id)
#cnt_view_t_c = 0
#for s in view_result.values():
#    cnt_view_t_c += len(s)
    
#
result = {}
for user_id in user_list['USER_ID_hash']:
   result.setdefault(user_id, [])
   if user_id in view_result:
      result[user_id] = list(view_result[user_id])

writeAnswer(result)

#relation between  CAPSULE_TEXT and GENRE_NAME
#capsule2genre_train = train_list[['CAPSULE_TEXT', 'GENRE_NAME']].drop_duplicates()
#capsule2genre_test = test_list[['CAPSULE_TEXT', 'GENRE_NAME']].drop_duplicates()
#capsule2genre = pd.merge(capsule2genre_train,capsule2genre_test, how = 'outer')
#
#capsule2genre_train.to_csv('./med/capsule2genre_train.csv', index = False)
#capsule2genre_test.to_csv('./med/capsule2genre_test.csv', index = False)
#capsule2genre.to_csv('./med/capsule2genre.csv', index = False)

#every week of user purchase coupon
#result: coupon in the test period is around 3000
## creat a column that denote week number in the period
#start = datetime.date(2011,7,1)
#def getWeekN(x):
#    day = (x.date() - start).days
#    if day == 0 or day == 1:
#        return 0
#    else:
#        return ((day - 2)//7 + 1)
#train_purchase['WEEK_N'] = train_purchase.I_DATE.map(getWeekN)
#groupped = train_purchase.groupby(by = train_purchase['WEEK_N'])
###count of every week user purchase coupon
#cnt_u_buy = groupped.count()
#cnt_u_buy = cnt_u_buy.ix[:,0]
###count of every week user purchase different coupon
#def foo(df):
#    return (df.drop_duplicates(['COUPON_ID_hash', 'USER_ID_hash']).shape)[0]
#cnt_u1c = groupped.apply(foo)
#plt.plot(cnt_u_buy)

# coupon relation between train_list, train_list_area, train_purchase
#result: list_area and purchase belong but not equal to list_;
#    list_area and purchase has no include relation;
#    Union(list_area, purchase) = list_
#list_ = set(train_list.COUPON_ID_hash)
#list_area = set(train_list_area.COUPON_ID_hash)
#purchase = set(train_purchase.COUPON_ID_hash)
#cnt_list = len(list_)  # 19413
#cnt_list_area = len(list_area) # 19368
#cnt_purchase = len(purchase) # 19368
#purchase_area = purchase.intersection(list_area) # 19323

# same_rate between small_area of user buy and small_area of coupon list area
## the small_rate is 100% that illustrate small_area is should the coupon 
## rather than the user live
cnt_in_same = 0        #number of coupon_id of purchase in train_list_area, and
cnt_in_notsame = 0
cnt_notin = 0
ids = set() #id of not in train_list but in purchase
def getArea_s(df):
    return list(df.SMALL_AREA_NAME)
#list_area = pd.merge()
grouped = train_list_area.groupby(by = train_list_area.COUPON_ID_hash)
train_id2area_s = grouped.apply(getArea_s)
id_Area_s = zip(train_purchase.COUPON_ID_hash, train_purchase.SMALL_AREA_NAME)
for (c_id, area_s) in id_Area_s:
    if c_id in train_id2area_s.keys():
        if area_s in train_id2area_s[c_id]:
            cnt_in_same += 1
        else:
            cnt_in_notsame += 1
    else:
        cnt_notin += 1
        ids.add(c_id)
same_rate = float(cnt_in_same/(cnt_in_same + cnt_in_notsame))

# same_rate between small_area of user buy and small_area of coupon shop
train_id2area = {for k:v in zip()}

import sys
sys.path.append(r'/Users/HAN/AllCode/Projects/Coupon/Code')
from MyUtil import *
import pandas as pd
import numpy as np
import math
from datetime import datetime
# read
tr_cp = pd.read_csv(input_path + 'coupon_list_train_en.csv')
te_cp = pd.read_csv(input_path + 'coupon_list_test_en.csv')
user = pd.read_csv(input_path + 'user_list_en.csv')
user.fillna({'WITHDRAW_DATE': '2015-01-01 00:00:00'}, inplace = True)
dateparse = lambda x: pd.datetime.strptime(x, '%Y-%m-%d %H:%M:%S')
user[['REG_DATE', 'WITHDRAW_DATE']] = user[['REG_DATE', 'WITHDRAW_DATE']].applymap(dateparse)
u_pur = pd.read_csv(input_path + 'coupon_detail_train_en.csv')
u_pur['I_DATE'] = u_pur['I_DATE'].map(dateparse)
u_pur = pd.merge(u_pur,user,on='USER_ID_hash')
tr_cp_area = pd.read_csv(input_path + 'coupon_area_train_en.csv')
te_cp_area = pd.read_csv(input_path + 'coupon_area_test_en.csv')
u_visit = pd.read_csv(input_path + 'coupon_visit_train_en.csv')

# Weight Matrix: GENRE_NAME, DISCOUNT_PRICE, DISPPERIOD, large_area_name,
# small_area_name, VALIDPERIOD, USABLE_DATE_sum, SMALL_AREA_NAME

# 741,32
view_te = pd.merge(u_visit,te_cp,left_on = 'VIEW_COUPON_ID_hash',
                   right_on = 'COUPON_ID_hash')

#NA in tr_cp and te_cp
# VALIDFROM
# VALIDEND
# VALIDPERIOD
# USABLE_DATE_MON
# USABLE_DATE_TUE
# USABLE_DATE_WED
# USABLE_DATE_THU
# USABLE_DATE_FRI
# USABLE_DATE_SAT
# USABLE_DATE_SUN
# USABLE_DATE_HOLIDAY
# USABLE_DATE_BEFORE_HOLIDAY
# for column in tr_cp.columns:
#     if(np.any(tr_cp[column].isnull())):
#         print(column)
# print('test:')
# for column in te_cp.columns:
#     if(np.any(te_cp[column].isnull())):
#         print(column)

start_day = '2011-07-03'
tr_cp['day_order'] = 

raw_feature_char = set(['CAPSULE_TEXT','GENRE_NAME',
"large_area_name", "ken_name", "small_area_name"])


def feat1(ori_train,ori_test,file_dir = None):
    feat1_features = ["COUPON_ID_hash",
                      "GENRE_NAME", "DISCOUNT_PRICE", "PRICE_RATE",
                      #"USABLE_DATE_MON", "USABLE_DATE_TUE", "USABLE_DATE_WED", "USABLE_DATE_THU",
                      #"USABLE_DATE_FRI", "USABLE_DATE_SAT", "USABLE_DATE_SUN",
                      #"USABLE_DATE_HOLIDAY","USABLE_DATE_BEFORE_HOLIDAY",
                      "large_area_name", "ken_name", "small_area_name"]
    ori_train = ori_train[feat1_features]
    ori_test = ori_test[feat1_features]
    feat1_features_char = []
    for feature in feat1_features:
        if feature in raw_feature_char:
            feat1_features_char.append(feature)
    ori_train.loc[:,'MyKind'] = 'train'
    ori_test.loc[:,'MyKind'] = 'test'
    tr_te = pd.concat([ori_train,ori_test])
    tr_te.fillna(1,inplace=True)
    tr_te.loc[:,'DISCOUNT_PRICE'] = 1.0 / np.log10(tr_te.loc[:,'DISCOUNT_PRICE'])
    tr_te.loc[:,'PRICE_RATE'] = (tr_te.loc[:,'PRICE_RATE'] * tr_te.loc[:,'PRICE_RATE'])/ (100.0 * 100)
    tr_te = pd.get_dummies(tr_te,columns=feat1_features_char)
    train = tr_te[tr_te['MyKind'] == 'train']
    train.drop(labels=['MyKind'],axis=1,inplace=True)
    test = tr_te[tr_te['MyKind'] == 'test']
    test.drop(labels=['MyKind'],axis=1,inplace=True)
    return (train,test)
tr_cp, te_cp = feat1(tr_cp, te_cp)



def cosine_simlar(u_pur_fea, te_cp, fea_weight):
    user_ids = u_pur_fea["USER_ID_hash"]
    u_pur_fea = u_pur_fea.drop(labels=['USER_ID_hash'], axis=1)
    te_cp_ids = te_cp['COUPON_ID_hash']
    te_cp = te_cp.drop(labels=['COUPON_ID_hash'], axis = 1)
    assert (np.all(u_pur_fea.columns == te_cp.columns) )
    u_pur_fea = u_pur_fea.as_matrix()
    te_cp = te_cp.as_matrix()
    assert (len(fea_weight) == te_cp.shape[1])
    weight_matrix = np.asmatrix(np.diag(fea_weight))
    u_test_score = u_pur_fea * weight_matrix * (te_cp.T)
    cp_id_sels = []
    te_cp_indexs = [i for i in range(te_cp.shape[0])]
    for i in range(len(user_ids)):
        cp_id_sort = sorted(te_cp_indexs, key = lambda x: u_test_score[i,x], reverse = True)
        cp_id_sel = " ".join(te_cp_ids[cp_id_sort[:10]])
        cp_id_sels.append(cp_id_sel)
    result = pd.DataFrame(columns = ['USER_ID_hash', 'PURCHASED_COUPONS'])
    result.USER_ID_hash = user_ids
    result.PURCHASED_COUPONS = cp_id_sels
    return result

u_pur_cp = pd.merge(u_pur.drop(labels=['ITEM_COUNT', 'I_DATE',
                                       'SMALL_AREA_NAME', 'PURCHASEID_hash',
                                       'REG_DATE', 'SEX_ID', 'AGE', 'WITHDRAW_DATE', 'PREF_NAME'], axis=1),
                    tr_cp, on='COUPON_ID_hash')
u_pur_cp_group = u_pur_cp.groupby(by=['USER_ID_hash'], as_index=False)
u_pur_fea = u_pur_cp_group.mean()
# W <- as.matrix(Diagonal(x=c(rep(2.05,13), rep(2,1), rep(-0.13,1), rep(0,9), rep(0.5,9), rep(1.01,47), rep(4.75,55))))
fea_weight_map = {"GENRE_NAME": 2.05, "DISCOUNT_PRICE": 2, "PRICE_RATE": -0.13,
                  "large_area_name": 0.5, "ken_name": 1.01, "small_area_name": 4.75}
fea_weight = []
for feature in te_cp.columns:
    if feature != "COUPON_ID_hash":
        if feature in fea_weight_map:
            weight = fea_weight_map[feature]
        else:
            pos = feature.rfind("_")
            weight = fea_weight_map[feature[:pos]]
        fea_weight.append(weight)
res = cosine_simlar(u_pur_fea, te_cp, fea_weight)
submit_user = pd.read_csv(input_path + 'sample_submission.csv')
submit_res = pd.merge(pd.DataFrame(submit_user.loc[:,"USER_ID_hash"]),
                      res, how='left', on='USER_ID_hash')
file_name = datetime.now().strftime("%y-%m-%d_%H-%M") + '_submission.csv'
submit_res.to_csv(res_path + file_name,index=False)
#u_test = u_pur_fea * fea_weight * te_cp
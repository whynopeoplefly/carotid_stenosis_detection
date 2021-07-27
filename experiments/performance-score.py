import numpy as np
import pandas as pd
import pickle
import os
from sklearn.metrics import f1_score,  precision_score, recall_score, average_precision_score, roc_auc_score, accuracy_score

cols = ['RIICA', 'RACA', 'RMCA', 'RPCA', 'RIVA', 'BA', 'LIICA', 'LACA', 'LMCA', 'LPCA', 'LIVA']

print('xlnet')
for col in cols:
    aps, accs, prs, res, aucs, f1s = [], [], [], [], [], []
    for n in range(10):
        # read_path = os.path.join('hyperparameter', 'results_20_1e-5', 'internal', 'round_' + str(n), 'predict_result.pickle')
        read_path = os.path.join('hyperparameter', 'results_20_1e-5', 'external', 'predict_result_'+ str(n) +'.pickle')
        with open(read_path, 'rb') as file:
            test_data =pickle.load(file)
            predict_prob = test_data[col + '_pred']
            predict_label = np.where(predict_prob > 0.5, 1, 0)
            true_label = test_data[col]
            f1 = f1_score(true_label, predict_label, average='binary')
            f1s.append(f1)
            ap = average_precision_score(true_label, predict_prob, average='micro')
            aps.append(ap)
            precision = precision_score(true_label, predict_label)
            prs.append(precision)
            recall = recall_score(true_label, predict_label)
            res.append(recall)
            roc_auc = roc_auc_score(true_label, predict_prob)
            aucs.append(roc_auc)
            acc = accuracy_score(true_label, predict_label)
            accs.append(acc)

    print(col+'_AP', round(np.mean(aps), 2), round(np.std(aps), 2))
    print(col+'_PR', round(np.mean(prs), 2), round(np.std(prs), 2))
    print(col+'_RE', round(np.mean(res), 2), round(np.std(res), 2))
    print(col+'_F1', round(np.mean(f1s), 2), round(np.std(f1s), 2))
    print(col+'_AUC', round(np.mean(aucs), 2), round(np.std(aucs), 2))
    print(col+'_ACC', round(np.mean(accs), 2), round(np.std(accs), 2))

print('lstm')
for col in cols:
    aps, accs, prs, res, aucs, f1s = [], [], [], [], [], []
    for n in range(10):
        read_path = os.path.join('lstm', 'results_accuracy', 'internal', 'round_' + str(n), 'predict_result_' + str(n) + '.csv')
        # read_path = os.path.join('lstm', 'results_accuracy', 'external', 'predict_result_' + str(n) + '.csv')
        test_data = pd.read_csv(read_path)
        predict_prob = test_data[col + '_pred']
        predict_label = np.where(predict_prob > 0.5, 1, 0)
        true_label = test_data[col]
        f1 = f1_score(true_label, predict_label, average='binary')
        f1s.append(f1)
        ap = average_precision_score(true_label, predict_prob, average='micro')
        aps.append(ap)
        precision = precision_score(true_label, predict_label)
        prs.append(precision)
        recall = recall_score(true_label, predict_label)
        res.append(recall)
        roc_auc = roc_auc_score(true_label, predict_prob)
        aucs.append(roc_auc)
        acc = accuracy_score(true_label, predict_label)
        accs.append(acc)

    print(col + '_AP', round(np.mean(aps), 2), round(np.std(aps), 2))
    print(col + '_PR', round(np.mean(prs), 2), round(np.std(prs), 2))
    print(col + '_RE', round(np.mean(res), 2), round(np.std(res), 2))
    print(col + '_F1', round(np.mean(f1s), 2), round(np.std(f1s), 2))
    print(col + '_AUC', round(np.mean(aucs), 2), round(np.std(aucs), 2))
    print(col + '_ACC', round(np.mean(accs), 2), round(np.std(accs), 2))

print('RB model')
for col in cols:
    # Internal
    aps, accs, prs, res, aucs, f1s = [], [], [], [], [], []
    for n in range(10):
        result = pd.read_csv(os.path.join('rb','internal', 'data_compare_' + str(n) + '.csv'))
        true_label = result[col]
        predict_label = result[col + '1']
        f1 = f1_score(true_label, predict_label, average='binary')
        f1s.append(f1)
        precision = precision_score(true_label, predict_label)
        prs.append(precision)
        recall = recall_score(true_label, predict_label)
        res.append(recall)
        acc = accuracy_score(true_label, predict_label)
        accs.append(acc)
        roc_auc = roc_auc_score(true_label, predict_label)
        aucs.append(roc_auc)
        ap = average_precision_score(true_label, predict_label, average='micro')
        aps.append(ap)
    print(col + '_PR', round(np.mean(prs), 2), round(np.std(prs), 2))
    print(col + '_RE', round(np.mean(res), 2), round(np.std(res), 2))
    print(col + '_F1', round(np.mean(f1s), 2), round(np.std(f1s), 2))
    print(col + '_ACC', round(np.mean(accs), 2), round(np.std(accs), 2))
    print(col + '_AP', round(np.mean(aps), 2), round(np.std(aps), 2))
    print(col + '_AUC', round(np.mean(aucs), 2), round(np.std(aucs), 2))
    # External
    # result = pd.read_csv(os.path.join('rb', 'external.csv'))
    # true_label = result[col]
    # predict_label = result[col + '1']
    # precision = precision_score(true_label, predict_label)
    # recall = recall_score(true_label, predict_label)
    # f1 = f1_score(true_label, predict_label, average='binary')
    # acc = accuracy_score(true_label, predict_label)
    # ap = average_precision_score(true_label, predict_label, average='micro')
    # roc_auc = roc_auc_score(true_label, predict_label)
    # print(col + '_PR', precision)
    # print(col + '_RE', recall)
    # print(col + '_F1', f1)
    # print(col + '_ACC', acc)
    # print(col + '_AP', ap)
    # print(col + '_AUC', roc_auc)
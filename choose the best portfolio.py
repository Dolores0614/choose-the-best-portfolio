import numpy as np
import pandas as pd
import scipy.stats as scs
import matplotlib
import matplotlib.pyplot as plt
import json

#station 1: ETL
#ASX
df_ASX = pd.read_excel(r"/Users/zhangdonglei/Desktop/3645projA/ASX200top10.xlsx", sheet_name=1, header=0, index_col=0)
#find the equities' name
equityname = list(filter(lambda col_name: not col_name.startswith("Unnamed"), df_ASX.columns))[1:]
print(equityname) #['BHP AT Equity', 'CSL AT Equity', 'RIO AT Equity', 'CBA AT Equity', 'WOW AT Equity', 'WES AT Equity', 'TLS AT Equity', 'AMC AT Equity', 'BXB AT Equity', 'FPH AT Equity']

#find the prices of each equity as a dataframe
cols = [3 + x * 5 for x in range(10)]
df_ASX = pd.read_excel("/Users/zhangdonglei/Desktop/3645projA/ASX200top10.xlsx", sheet_name = 1, header=1, index_col=0).iloc[:, cols]
print(df_ASX)

#the final dataframe
df_ASX.columns = equityname
df_ASX = df_ASX.dropna()
print(df_ASX)

#news
news = open("/Users/zhangdonglei/Desktop/3645projA/news_dump.json", "r")
data = json.load(news)
data = pd.DataFrame(data)

#client: do not need ETL
df_client = pd.read_excel("/Users/zhangdonglei/Desktop/3645projA/Client_Details.xlsx", sheet_name=1, header=0, index_col=0)

#Economics indicator
df_econ = pd.read_excel("/Users/zhangdonglei/Desktop/3645projA/Economic_Indicators.xlsx", header=1, index_col=0)
df_econ_m = df_econ.iloc[0: 28]
df_econ_m.replace('-', np.nan, inplace=True)
print(df_econ_m)
df_econ_q = df_econ.iloc[28:]
df_econ_q.replace('-', np.nan, inplace=True)#dellet empty row

#Feature Engineering
#equities' price's normality check
np.random.seed(100)
num_e = len(equityname) #number of equities
ret = np.log(df_ASX / df_ASX.shift(1))
weights = np.random.random(10)
weights /= np.sum(weights)
def normality_tests(arr):
    print(f"Skew of dataset: {scs.skew(arr):10.4f}")
    print(f"Skew test p-value: {scs.skewtest(arr)[1]:10.4f}")
    print(f"Kurt of dataset: {scs.kurtosis(arr):10.4f}")
    print(f"Kurt test p-value: {scs.kurtosistest(arr)[1]:10.4f}")
    print(f"Norm test p-value: {scs.normaltest(arr)[1]:10.4f}")

ret_trend = (ret * weights).sum(axis=1)
normality_tests(ret_trend)
# Skew of dataset:    -0.4689
# Skew test p-value:     0.0000
# Kurt of dataset:     8.9206
# Kurt test p-value:     0.0000
# Norm test p-value:     0.0000000

plt.hist(ret_trend, bins=100)
plt.show()

#clients data
df_client_positions_means = df_client.mean()
df_client_positions_means = df_client_positions_means[2:]
print(df_client_positions_means)

# Plot the bar chart
# Create a figure and axes for the plot
plt.figure(figsize=(10, 6))
plt.title('Mean Positions')
plt.xlabel('Equities')
plt.ylabel('Mean Position')
plt.bar(df_client_positions_means.index, df_client_positions_means.values, color='b')
plt.tight_layout()
plt.show()

#Econ
ow_avg = df_econ_m.mean(axis=1)
ow_avg = df_econ_q.mean(axis=1)

########################################################################################
##station3 :Model Design
import scipy.optimize as sco
cpath = r"/Users/zhangdonglei/Desktop/3645projA/Client_Details.xlsx"
client_info = pd.read_excel(cpath, sheet_name = "Data", header = 0, index_col = 0)

risk_p = client_info["risk_profile"].values
ret = ret.iloc[1:] #return of the company

p_ret = []  #product returns
p_std = []  #product validilaty
for p in range(3000): #monte-carlo simulation
    weights = np.random.random(num_e) #  num_e is the number of quities = 10 here
    weights /= np.sum(weights) #make sure the sum of the random generated weights is 1
    p_ret.append(np.sum(ret.mean()*weights)*252) #daily prof return * work day in a year
    p_std.append(np.sqrt(np.dot(weights.T, np.dot(ret.cov()*252, weights))))

#trans the list to np: easier to calculate laetr
p_ret = np.array(p_ret)
p_std = np.array(p_std)

con = ({"type": "eq", "fun": lambda x: np.sum(x) - 1}) #conditon: sum(x) =1
bound = tuple((0, 1) for x in range(num_e)) #x is between (0,1)


# creat a fuction to return :portfolio ret, volatility and sharp ratio
def p_ret_vol_sp(weights):
    weights = np.array(weights)
    p_ret = np.sum(ret.mean()*weights) * 252
    p_std = np.sqrt(np.dot(weights.T, np.dot(ret.cov()*252, weights)))
    return np.array([p_ret, p_std, p_ret / p_std])

#find the maximize sharp ratio
def max_sp(weights):
    return -p_ret_vol_sp(weights)[2]

optsp = sco.minimize(max_sp, np.ones(num_e) / num_e, method = "SLSQP", bounds = bound, constraints = con) #use spciy.optmize to solve a  Optimization problem
#[1./num_e for _ in range(num_e)]
print("The situation of maximization of sharp ratio")
print("The weights are:" + str(optsp["x"].round(4)))
print("Return   Validity   Sharp ratio :" + str(p_ret_vol_sp(optstd["x"]).round(4))) #the ret, vol, sp of product


#risk min
def min_std(weights):
    return p_ret_vol_sp(weights)[1]

optstd = sco.minimize(min_std, num_e * [1. /num_e], method = "SLSQP", bounds = bound, constraints = con) #use spciy.optmize to solve a  Optimization problem
print("The situation of min of var")
print("The weights are:" + str(optstd["x"].round(4)))
print("Return   Validity   Sharp ratio :" + str(p_ret_vol_sp(optstd["x"]).round(4))) #the ret, vol, sp of product

#risk preference adjust
risk = risk_p[30]
def person_risk(weights):
    if risk > 5:
        return (risk/9 + 1) * p_ret_vol_sp(weights)[1]
    else:
        return p_ret_vol_sp(weights)[1]
opt_riskadjust = sco.minimize(person_risk, num_e * [1. /num_e], method = "SLSQP", bounds = bound, constraints = con) #use spciy.optmize to solve a  Optimization problem
print("The situation of min of var")
print("The weights are:" + str(opt_riskadjust["x"].round(4)))
print("Return   Validity   Sharp ratio :" + str(p_ret_vol_sp(opt_riskadjust["x"]).round(4))) #the ret, vol, sp of product

#the final optimal weights
weight_label = equityname
opy_weights = dict(zip(weight_label, opt_riskadjust['x'].round(4)))
opt_df = pd.DataFrame({'weight' : opt_riskadjust['x'].round(4)}, weight_label)
print(opt_df)

bound = tuple((0, 1) for x in weights)
t_ret =np.linspace(0.0, 0.2, 50)
t_vol = []
for test in t_ret:
    con = (
        {"type": "eq", "fun": lambda x: p_ret_vol_sp(x)[0] - test},
        {"type": "eq", "fun": lambda x: np.sum(x) - 1}
    ) #con for expected return
    res = sco.minimize(min_std, np.ones(num_e) / num_e, method = "SLSQP", bounds = bound, constraints = con)
    t_vol.append(res["fun"])
t_vol = np.array(t_vol)

# Setting up the figure size
plt.figure(figsize=(8,5))

# Convert p_std and p_ret to numpy arrays
p_std = np.array(p_std)
p_ret = np.array(p_ret)

# Scatter plot for random portfolio composition
plt.scatter(p_std, p_ret, c=p_ret/p_std, cmap='viridis', edgecolor='none', alpha=0.6)

# Plot the efficient frontier
plt.plot(t_vol, t_ret, "k", lw=3, label="Efficient Frontier")

# Calculate portfolio values
sharpe_portfolio = p_ret_vol_sp(optsp['x'])
min_std_portfolio = p_ret_vol_sp(optstd['x'])
personal_risk_portfolio = p_ret_vol_sp(opt_riskadjust['x'])

# Plot portfolios
plt.scatter(sharpe_portfolio[1], sharpe_portfolio[0], c='r', marker='*', s=150, label='SP-Max')
plt.scatter(min_std_portfolio[1], min_std_portfolio[0], c='b', marker='*', s=150, label='Risk-Min')
plt.scatter(personal_risk_portfolio[1], personal_risk_portfolio[0], c='y', marker='*', s=150, label='Personal Risk')

# Annotations
plt.annotate('SP-Max', (sharpe_portfolio[1], sharpe_portfolio[0]), xytext=(10,0), textcoords='offset points')
plt.annotate('Personal Risk', (personal_risk_portfolio[1], personal_risk_portfolio[0]), xytext=(10,0), textcoords='offset points')
plt.annotate('Risk-Min', (min_std_portfolio[1], min_std_portfolio[0]), xytext=(10,0), textcoords='offset points')

# Labels and title
plt.title('Efficient Frontier of Portfolio')
plt.xlabel('Expected Volatility')
plt.ylabel('Expected Return')
plt.colorbar(label='Sharpe Ratio')

# Display
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()



############################################
####adjust accoding to the news
jpath = r"/Users/zhangdonglei/Desktop/3645projA/news_dump.json"
df_news = pd.read_json(jpath)

#preparation
import nltk
from nltk.sentiment.vader import SentimentIntensityAnalyzer
nltk.download('vader_lexicon')
analyzer = SentimentIntensityAnalyzer()



#set equityname
new_equityname = []
for i in equityname:
    new_equityname.append(i.split()[0])
equityname = new_equityname
print(equityname)
#['BHP', 'CSL', 'RIO', 'CBA', 'WOW', 'WES', 'TLS', 'AMC', 'BXB', 'FPH']#

#Iterate through the headlines and get the polarity scores using vader scores
df_news['Headline'].apply(vader.polarity_scores).tolist()
#Convert the 'scores'list of dicts into a DataFrame
df_scores = pd.DataFrame(scores)
print(scores_df.head(20))
#Join the DataFrames of the news and the list of dicts
df_news = df_news.join(df_scores,rsuffix='_right')
df_news.to_csv(r'/Users/zhangdonglei/Desktop/3645projA/vaderscores1.csv')
#Group by ticker columns from scored_news and calculate the mean
mean_scores = df_news.groupby(['Equity']).mean()
#explore the trend of each quity to adjust our strategy(the weights of each equity)
group = df.groupby('Equity')
weights_adjust ={}
for e in equityname:
    part = groups.get_group(e)
    print(part)
    recet = part.iloc[0:20].mean()["compound"]
    history = part[20:].mean()["compound"]
    adjust = np.round((recent - history)/10, 4)
    weights_adjust[e] = adjust
#show the plot
plt.bar(weights_adjust.keys(), weights_adjust.values())
plt.grid()
plt.show()

#the final optimal weights after adjust to news

opt_df['sentiment adjust'] = weights_adjust.values()


################################################
######use econnomics indicator to adjust
import statsmodels.api as sm
df_econ.dropna(axis=0, inplace= True)
df_econ_m = df_econ.iloc[:28].T
df_econ_m["Monthly Indicators"] = pd.to_datetime(df_econ_m["Monthly Indicators"])
df_econ_m = df_econ_m.set_index("Monthly Indicators")

#df_ASX is the dataframe from ASX data,merge the 2 dataframe
# Merge the dataframes based on their indices
merged_df = pd.merge(left=df_ASX, right=df_econ_m, left_on=df_ASX.index, right_on=df_econ_m.index, how='left')
# Forward fill the NaN values
merged_df.fillna(method='ffill', axis=1, inplace=True)
# Rename the first column to 'Date'
merged_df.rename(columns={merged_df.columns[0]: 'Date'}, inplace=True)
# Convert the 'Date' column to datetime format and set it as the index
merged_df['Date'] = pd.to_datetime(merged_df['Date'], format="%Y-%m-%d")
merged_df.set_index('Date', inplace=True)


#find the factor we will use and make a new dataframe
econ_m = mer_data.resample("M").last()
econ_m = econ_m.loc['2019-1-31':]
print(econ_m)
print(econ_m.columns)

econ_m_ret = econ_m.pct_change(periods= 1, fill_method='pad')
econ_m_ret = econ_m_ret.dropna()
econ_m_ret = econ_m_ret.iloc[:-1, :]

factor = ['Consumer Sentiment Index', 'CPI (%q/q)', 'PPI (%q/q)', 'Money Supply, M3 (%y/y)']
factor_df = econ_m_ret[factor].sort_index(ascending=0)

#calculate recent data and make prediction
recent_data = factor_df.iloc[:4].mean()

# Print the first few rows of factor_df to inspect its content
print(factor_df.head())

# Print the index after the reset
print(factor_df.index)
predic_factor = factor_df.iloc[0]*(1+recent_data)  #predicted return for next month
print(recent_data)
print(predic_factor)

predic_ret = []
for equity in list(df_ASX.columns)[1:]:
    y = econ_m_ret[equity]
    x = econ_m_ret[factor]
    model = sm.OLS(y,x)
    y_predic = results.predict(recent_data.values)
    predic_ret.append(y_predic)

predic_ret = pd.DataFrame(predic_ret, index=equityname)
predic_mean = predic_ret.mean()
weights_adjust2 = predic_ret - predic_mean

#the final optimal weights after adjust to news
weights_adjust2.columns = ['economics weighst change']
opt_df['economic adjust'] = weights_adjust2['economics weighst change']
print(opt_df)

final_w = opt_df.sum(axis=1)
final_w = (final_w / final_w.sum()).round(4)
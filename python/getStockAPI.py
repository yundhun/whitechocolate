import pandas as pd 
code_df = pd.read_html('http://kind.krx.co.kr/corpgeneral/corpList.do?method=download&searchType=13', header=0)[0] # 종목코드가 6자리이기 때문에 6자리를 맞춰주기 위해 설정해줌 
code_df.종목코드 = code_df.종목코드.map('{:06d}'.format) # 우리가 필요한 것은 회사명과 종목코드이기 때문에 필요없는 column들은 제외해준다. 
code_df = code_df[['회사명', '종목코드']] # 한글로된 컬럼명을 영어로 바꿔준다. 
code_df = code_df.rename(columns={'회사명': 'name', '종목코드': 'code'}) 
code_df.head()

# 종목 이름을 입력하면 종목에 해당하는 코드를 불러와 # 네이버 금융(http://finance.naver.com)에 넣어줌 

def get_url(item_name, code_df): 
	code = code_df.query("name=='{}'".format(item_name))['code'].to_string(index=False) 
	url = 'https://finance.naver.com/item/sise_day.nhn?code={code}'.format(code=code) 
	print("요청 URL = {}".format(url)) 
	return url.replace(' ','') # 신라젠의 일자데이터 url 가져오기 


df = pd.DataFrame() # 1페이지에서 20페이지의 데이터만 가져오기 

for item_name in code_df['name'][1936:]:
    url = get_url(item_name, code_df) # 일자 데이터를 담을 df라는 DataFrame 정의 
    for page in range(1, 24):
        pg_url = '{url}&page={page}'.format(url=url, page=page)
        t = pd.read_html(pg_url, header=0)[0]
        t['종목명'] = item_name
        df = df.append( t , ignore_index=True)
    df = df.dropna() # 상위 5개 데이터 확인하기 df.head()
    
df.to_csv('./data.csv')
    
#오성첨단소재 다시 수행

code_df['name'][1936:]

#
runMode = 'SIM'
#runMode = 'OPERATION'


df[df['종목명']=='DSR']

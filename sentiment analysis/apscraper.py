# import & download dependencies w/ requests to pull url, bs4 to process HTML, 
# nltk to check sentiment, and spacy to break sentences
import requests
from bs4 import BeautifulSoup
import nltk
nltk.downloader.download('vader_lexicon')
nltk.download('punkt')
from nltk.sentiment.vader import SentimentIntensityAnalyzer
from nltk.tokenize import sent_tokenize
import spacy
from spacy.lang.en.examples import sentences 
nlp = spacy.load('en_core_web_md')
sid = SentimentIntensityAnalyzer()
# initializing test characteristics
url = "https://apnews.com/article/president-joe-biden-ap-poll-prescription-drugs-medicare-b82928109ae8564e750a6a196c98cfe9"
demWords = ["biden","democrats", "democrat", "joe biden"]
demScores = []
repScores = []
repWords = ["trump", "donald trump", "republican", "republicans"]
# process test
page = requests.get(url)
soup = BeautifulSoup(page.content, "html.parser")
soup.find("div",class_ = "Enhancement-item").clear() # removing ads and promos from AP News body
for x in soup.find_all(): # removing empty tags
      if len(x.get_text(strip=True)) == 0: 
        x.extract()
content = soup.find("div",class_ = "RichTextStoryBody RichTextBody")
for block in content:
    nugget = block.text.strip()
    for sentence in sent_tokenize(nugget):
        # print(sid.polarity_scores(sentence)["compound"])
        result_list = list(map(str.lower, [tok.text for tok in nlp(sentence) if (tok.dep_ == "nsubj")]))
        # print(result_list)

        for value in demWords:
            if value in result_list:
                demScores.append(sid.polarity_scores(sentence)["compound"])
                # print("----------------- LOOK HERE ---------------")
                break
        for value in repWords:
            if value in result_list:
                repScores.append(sid.polarity_scores(sentence)["compound"])
                # print("----------------- LOOK HERE ---------------")
                break            
        print(sentence)
    
print(demScores)
print(sum(demScores) / len(demScores))
print(repScores)
print(sum(repScores) / len(repScores))
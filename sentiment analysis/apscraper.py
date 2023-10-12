import requests
from bs4 import BeautifulSoup
import nltk
nltk.downloader.download('vader_lexicon')
from nltk.sentiment.vader import SentimentIntensityAnalyzer
from nltk.tokenize import sent_tokenize
import spacy
nlp = spacy.load("en_core_web_sm")
import en_core_web_sm
nlp = en_core_web_sm.load()
sid = SentimentIntensityAnalyzer()
page = requests.get("https://apnews.com/article/president-joe-biden-ap-poll-prescription-drugs-medicare-b82928109ae8564e750a6a196c98cfe9")
soup = BeautifulSoup(page.content, "html.parser")
soup.find("div",class_ = "Enhancement-item").clear()
for x in soup.find_all(): 
      if len(x.get_text(strip=True)) == 0: 
          
        # Remove empty tag 
        x.extract()
content = soup.find("div",class_ = "RichTextStoryBody RichTextBody")
for block in content:
    nugget = block.text.strip()
    for sentence in sent_tokenize(nugget):
        print(sid.polarity_scores(nugget)["compound"])
        print([tok for tok in nlp(nugget) if (tok.dep_ == "nsubj") ])
        print(nugget)
    

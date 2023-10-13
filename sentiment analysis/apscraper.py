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
url = "https://apnews.com/article/president-joe-biden-ap-poll-prescription-drugs-medicare-b82928109ae8564e750a6a196c98cfe9"
page = requests.get(url)
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
        print(sid.polarity_scores(sentence)["compound"])
        print([tok for tok in nlp(sentence) if (tok.dep_ == "nsubj") ])
        print(sentence)
    

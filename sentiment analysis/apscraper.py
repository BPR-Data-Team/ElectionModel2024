# import & download dependencies w/ requests to pull url, bs4 to process HTML, 
# nltk to check sentiment, and spacy to break sentences
import csv
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
urls = [
    "https://apnews.com/article/president-joe-biden-ap-poll-prescription-drugs-medicare-b82928109ae8564e750a6a196c98cfe9",
    "https://apnews.com/article/biden-pacific-islands-summit-climate-change-funding-727aa6b908ef5cf1ea28438b965ba9b5",
    "https://apnews.com/article/biden-guns-gun-violence-mass-shootings-5a72e3730b6d24b2143f034a9ee6ed21"
    "https://apnews.com/article/donald-trump-letitia-james-fraud-new-york-c1865da048782b708de74c050f647df2",
    "https://apnews.com/article/donald-trump-benjamin-netanyahu-israel-hamas-republicans-63295565c0abe5b30da5898a6b8eb01a",
    "https://apnews.com/article/trump-gaetz-speaker-ouster-republican-8380c43e9cfcb96b3457bf7e1002b6f1"
    ]
demWords = ["biden","democrats", "democrat", "joe biden"]
demScores = []
repScores = []
repWords = ["trump", "donald trump", "republican", "republicans"]
# process test
with open("sentiment analysis/article_scores.csv", 'w', newline='') as csvfile:
  writer = csv.writer(csvfile)
  writer.writerow(["articleTitle","democratScore","republicanScore"])
  for link in urls:
    page = requests.get(link)
    soup = BeautifulSoup(page.content, "html.parser")
    soup.find("div",class_ = "Enhancement-item").clear() # removing ads and promos from AP News body
    for x in soup.find_all(): # removing empty tags
          if len(x.get_text(strip=True)) == 0: 
            x.extract()
    content = soup.find("div",class_ = "RichTextStoryBody RichTextBody") # selecting article text
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
            # print(sentence)
        
    print(soup.title.string)
    # print(demScores)
    print((sum(demScores) / len(demScores)))
    # print(repScores)
    print((sum(repScores) / len(repScores)))
    writer.writerow([soup.title.string, (sum(demScores) / len(demScores)),(sum(repScores) / len(repScores))])


  

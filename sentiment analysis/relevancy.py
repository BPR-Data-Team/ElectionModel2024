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
from datetime import datetime

nlp = spacy.load('en_core_web_sm')
sid = SentimentIntensityAnalyzer()
# initializing test characteristics
electionDays = dict(
    {'2004-11-2':{
        'repWords':['bush','cheney','republican'],
        'demWords':['kerry','edwards','democrat'],
        'indepWords':['independent']},
            '2000-11-7':{
                'repWords':['bush','cheney','republican'],
                'demWords':['gore','lieberman','democrat'],
                'indepWords':['independent']},
            '2008-11-4':{
                'repWords':['mccain','palin','republican'],
                'demWords':['obama','biden','democrat'],
                'indepWords':['independent']},
            '2012-11-6':{
                'repWords':['romney','ryan','republican'],
                'demWords':['obama','biden','democrat'],
                'indepWords':['independent']},
            '2016-11-1':{
                'repWords':['trump','pence','republican'],
                'demWords':['clinton','kaine','democrat'],
                'indepWords':['independent']},
            '2020-11-3':{
                'repWords':['trump','pence','republican'],
                'demWords':['biden','harris','democrat'],
                'indepWords':['independent']}
        }
        )
def processURL(URL,startDate):
    page = requests.get(URL)
    date = page.url[-10:]
    soup = BeautifulSoup(page.content, "html.parser")
    content = soup.find("a",class_ = "topic-title")
    nextLink = soup.find("a")[2]['href']
    electionYear = date.url[4:]
    daysToElection = abs(datetime.strptime((startDate, "%Y-%m-%d")-datetime.strptime(date, "%Y-%m-%d")).days)
    demMentions = 0
    repMentions = 0
    indepMentions = 0
    demAveSentiment = 0
    repAveSentiment = 0 
    indepAveSentiment = 0
    if daysToElection <= 50:
        for block in content:
            sentence = block.text.strip()
            
            # print(sid.polarity_scores(sentence)["compound"])
            result_list = list(map(str.lower, [tok.text for tok in nlp(sentence) if (tok.dep_ == "nsubj")]))
            # print(result_list)

            for value in electionDays[date]["demWords"]:
                if value in result_list:
                    demMentions += 1
                    demAveSentiment += sid.polarity_scores(sentence)["compound"]

            for value in electionDays[date]["repWords"]:
                if value in result_list:
                    repMentions += 1
                    repAveSentiment += sid.polarity_scores(sentence)["compound"]

            for value in electionDays[date]["indepWords"]:
                if value in result_list:
                    indepMentions += 1
                    indepAveSentiment += sid.polarity_scores(sentence)["compound"]
            writer.writerow([electionYear,daysToElection,demMentions,repMentions,indepMentions,demAveSentiment/demMentions,repAveSentiment/repMentions,indepAveSentiment/indepMentions])
        processURL(nextLink,startDate)
    else:
        checkURL(nextLink)

def checkURL(link):
    page = requests.get(link)
    date = page.url[-10:]
    if date in electionDays:
        processURL(link,date)
    else:
        soup = BeautifulSoup(page.content, "html.parser")
        nextLink = soup.find("a")[2]['href']
        checkURL(nextLink)


with open("article_scores.csv", 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["electionYear","daysToElection","demMentions","repMentions","indepMentions","demAveSentiment","repAveSentiment","indepAveSentiment"])
    startLink = "https://text.npr.org/1206601773/2023-10-17"
    checkURL(startLink)
        # recursive passing continue unless days until = 50
# process test
""" with open("article_scores.csv", 'w', newline='') as csvfile:
  writer = csv.writer(csvfile)
  writer.writerow(["articleTitle","democratScore","republicanScore"])
  for sheet in range(1,60):
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
 """
# Text-Mining-Functions
This repositiory contains customized functions to do NLP tasks. 

As of now it contains two major functions which helps to search for pattern in text document.
It will be helpful for Classification & Sentiment analysis problems.

## Function 1: Random Pattern Search

This function will help to search for pattern(single word or more than one word) in text irrespective of the words order in the pattern. 
If all the words in the pattern present in original text, it will match those texts. 

<b>Example</b>

Text = "I am proud to be a Data Scientist"
pattern= "Scientist proud"

Since both the words (Scientist & proud) presents in my text, i will mark it as pattern exist.

## Function 2: Forward Pattern Search

This function will help to search for pattern(single word or more than one word) in text respective of the words order in the pattern. 
If all the words in the pattern present in original text & in the same order, it will match those texts. 

<b>Example</b>

Text = "I am proud to be a Data Scientist"
pattern= "Scientist proud"

Even though both the words (Scientist & proud) presents in my text, but not in the same order like pattern, i will mark it as pattern not exist.

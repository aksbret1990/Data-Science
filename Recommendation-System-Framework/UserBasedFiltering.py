# -*- coding: utf-8 -*-
"""
Mining Assignment 1
"""

import math

#################################################
# recommender class does user-based filtering and recommends items 
class UserBasedFilteringRecommender:
    
    # class variables:    
    # none
    
    ##################################
    # class instantiation method - initializes instance variables
    #
    # usersItemRatings:
    # users item ratings data is in the form of a nested dictionary:
    # at the top level, we have User Names as keys, and their Item Ratings as values;
    # and Item Ratings are themselves dictionaries with Item Names as keys, and Ratings as values
    # Example: 
    #     {"Angelica":{"Blues Traveler": 3.5, "Broken Bells": 2.0, "Norah Jones": 4.5, "Phoenix": 5.0, "Slightly Stoopid": 1.5, "The Strokes": 2.5, "Vampire Weekend": 2.0},
    #      "Bill":{"Blues Traveler": 2.0, "Broken Bells": 3.5, "Deadmau5": 4.0, "Phoenix": 2.0, "Slightly Stoopid": 3.5, "Vampire Weekend": 3.0}}
    #
    # metric:
    # metric is in the form of a string. it can be any of the following:
    # "minkowski", "cosine", "pearson"
    #     recall that manhattan = minkowski with r=1, and euclidean = minkowski with r=2
    # defaults to "pearson"
    #
    # r:
    # minkowski parameter
    # set to r for minkowski, and ignored for cosine and pearson
    #
    # k:
    # the number of nearest neighbors
    # defaults to 1
    #
    def __init__(self, usersItemRatings, metric='pearson', r=1, k=1):
        
        # set self.usersItemRatings
        self.usersItemRatings = usersItemRatings

        # set self.metric and self.similarityFn
        if metric.lower() == 'minkowski':
            self.metric = metric
            self.similarityFn = self.minkowskiFn
        elif metric.lower() == 'cosine':
            self.metric = metric
            self.similarityFn = self.cosineFn
        elif metric.lower() == 'pearson':
            self.metric = metric
            self.similarityFn = self.pearsonFn
        else:
            print ("    (DEBUG - metric not in (minkowski, cosine, pearson) - defaulting to pearson)")
            self.metric = 'pearson'
            self.similarityFn = self.pearsonFn
        
        # set self.r
        if (self.metric == 'minkowski'and r > 0):
            self.r = r
        elif (self.metric == 'minkowski'and r <= 0):
            print ("    (DEBUG - invalid value of r for minkowski (must be > 0) - defaulting to 1)")
            self.r = 1
            
        # set self.k
        if k > 0:   
            self.k = k
        else:
            print ("    (DEBUG - invalid value of k (must be > 0) - defaulting to 1)")
            self.k = 1
          
        #print(self.metric)
    
    #################################################
    # minkowski distance (dis)similarity - most general distance-based (dis)simialrity measure
    # notation: if UserX is Angelica and UserY is Bill, then:
    # userXItemRatings = {"Blues Traveler": 3.5, "Broken Bells": 2.0, "Norah Jones": 4.5, "Phoenix": 5.0, "Slightly Stoopid": 1.5, "The Strokes": 2.5, "Vampire Weekend": 2.0}
    # userYItemRatings = {"Blues Traveler": 2.0, "Broken Bells": 3.5, "Deadmau5": 4.0, "Phoenix": 2.0, "Slightly Stoopid": 3.5, "Vampire Weekend": 3.0}
    def minkowskiFn(self, userXItemRatings, userYItemRatings):
        
        distance = 0
        commonRatings = False 
        
        for item in userXItemRatings:
            # inlcude item rating in distance only if it exists for both users
            if item in userYItemRatings:
                distance += pow(abs(userXItemRatings[item] - userYItemRatings[item]), self.r)
                commonRatings = True
                
        if commonRatings:
            return round(pow(distance,1/self.r), 2)
        else:
            # no ratings in common
            return -2

    #################################################
    # cosince similarity
    # notation: if UserX is Angelica and UserY is Bill, then:
    # userXItemRatings = {"Blues Traveler": 3.5, "Broken Bells": 2.0, "Norah Jones": 4.5, "Phoenix": 5.0, "Slightly Stoopid": 1.5, "The Strokes": 2.5, "Vampire Weekend": 2.0}
    # userYItemRatings = {"Blues Traveler": 2.0, "Broken Bells": 3.5, "Deadmau5": 4.0, "Phoenix": 2.0, "Slightly Stoopid": 3.5, "Vampire Weekend": 3.0}
    def cosineFn(self, userXItemRatings, userYItemRatings):
        
        sum_xy = 0
        sum_x2 = 0
        sum_y2 = 0
        
        for item in userXItemRatings:
            if item in userYItemRatings:
                x = userXItemRatings[item]
                y = userYItemRatings[item]
                sum_xy += x * y
                sum_x2 += pow(x, 2)
                sum_y2 += pow(y, 2)
        
        denominator = math.sqrt(sum_x2) * math.sqrt(sum_y2)
        if denominator == 0:
            return -2
        else:
            return round(sum_xy / denominator, 3)

    #################################################
    # pearson correlation similarity
    # notation: if UserX is Angelica and UserY is Bill, then:
    # userXItemRatings = {"Blues Traveler": 3.5, "Broken Bells": 2.0, "Norah Jones": 4.5, "Phoenix": 5.0, "Slightly Stoopid": 1.5, "The Strokes": 2.5, "Vampire Weekend": 2.0}
    # userYItemRatings = {"Blues Traveler": 2.0, "Broken Bells": 3.5, "Deadmau5": 4.0, "Phoenix": 2.0, "Slightly Stoopid": 3.5, "Vampire Weekend": 3.0}
    def pearsonFn(self, userXItemRatings, userYItemRatings):
        
        sum_xy = 0
        sum_x = 0
        sum_y = 0
        sum_x2 = 0
        sum_y2 = 0
        n = 0
        
        for item in userXItemRatings:
            if item in userYItemRatings:
                n += 1
                x = userXItemRatings[item]
                y = userYItemRatings[item]
                sum_xy += x * y
                sum_x += x
                sum_y += y
                sum_x2 += pow(x, 2)
                sum_y2 += pow(y, 2)
       
        if n == 0:
            return -2
        
        denominator = math.sqrt(sum_x2 - pow(sum_x, 2) / n) * math.sqrt(sum_y2 - pow(sum_y, 2) / n)
        if denominator == 0:
            return -2
        else:
            return round((sum_xy - (sum_x * sum_y) / n) / denominator, 2)
            

    def GetMinDist(self,distance):
        MinNumList = []
        MinNameList = []          
        for i in range(7):  
            MinNum = 999999999999999999999
            MinName = ""
            for j in distance.keys():
                if(MinNum > distance[j]):
                    MinNum = distance[j]
                    MinName = j 
         
            MinNumList.append(MinNum)
            MinNameList.append(MinName)
            del distance[MinName]
        return MinNameList


    def GetMaxDist(self,distance):
        MaxNumList = []
        MaxNameList = []   
        for i in range(7):  
            MaxNum = -999999999999999999999
            MaxName = ""
            for j in distance.keys():
                if(MaxNum < distance[j]):
                    MaxNum = distance[j]
                    MaxName = j 
             
            MaxNumList.append(MaxNum)
            MaxNameList.append(MaxName)
            del distance[MaxName]
        return MaxNameList


    def GetMultipleMaxDist(self,distance,k):
        MaxNumList = []
        MaxNameList = []   
        for b in range(k):  
            MaxNum = -999999999999999999999
            MaxName = ""
            for j in distance.keys():
                if(MaxNum < distance[j]):
                    MaxNum = distance[j]
                    MaxName = j 
             
            MaxNumList.append(MaxNum)
            MaxNameList.append(MaxName)
            del distance[MaxName]
        return MaxNameList


    def GetMaxNum(self,distance,k):
        MaxNumList = []
        MaxNameList = []   
        for b in range(k):  
            MaxNum = -999999999999999999999
            MaxName = ""
            for j in distance.keys():
                if(MaxNum < distance[j]):
                    MaxNum = distance[j]
                    MaxName = j 
             
            MaxNumList.append(MaxNum)
            MaxNameList.append(MaxName)
            del distance[MaxName]
        return MaxNumList
    
       
    def GetMultipleMinDist(self,distance,k):
        MinNumList = []
        MinNameList = []   
        for b in range(k):  
            MinNum = 999999999999999999999
            MinName = ""
            for j in distance.keys():
                if(MinNum > distance[j]):
                    MinNum = distance[j]
                    MinName = j 
             
            MinNumList.append(MinNum)
            MinNameList.append(MinName)
            del distance[MinName]
        return MinNameList


    def GetMinNum(self,distance,k):
        MinNumList = []
        MinNameList = []   
        for b in range(k):  
            MinNum = 999999999999999999999
            MinName = ""
            for j in distance.keys():
                if(MinNum > distance[j]):
                    MinNum = distance[j]
                    MinName = j 
             
            MinNumList.append(MinNum)
            MinNameList.append(MinName)
            del distance[MinName]
        return MinNumList
    
    



    #################################################
    # make recommendations for userX from the most similar k nearest neigibors (NNs)
    def recommendKNN(self, userX):
        
        # YOUR CODE HERE
        
        # for given userX, get the sorted list of users - by most similar to least similar        
       
                
        distance={}
        for key in self.usersItemRatings.keys():
            distance[key] = 0
            
        del distance[userX]
          
        for j in self.usersItemRatings:
            if j in distance:
                distance[j] = self.similarityFn(self.usersItemRatings[userX],self.usersItemRatings[j])
 
        #creating copies of distance dictionary           
        distancecopy=dict(distance)
        distancecopy1=dict(distance)            
        distancecopy2=dict(distance)
        distancecopy3=dict(distance)
     
                  
         
        if(self.metric=="minkowski" and self.k==1):
            SimilarList = self.GetMinDist(distance)
            MostSimilar = SimilarList[0]
            newlist=[]
            MostSimilarDict = self.usersItemRatings[MostSimilar]    
            MostSimilarL = list(MostSimilarDict.items())
            MostSimilarL.sort(key=lambda tup:tup[1], reverse=True) 
            for item in MostSimilarL:
                k,v = item
                if(k not in self.usersItemRatings[userX].keys()):
                    newlist.append(item)
            # return sorted list of recommendations (sorted highest to lowest ratings)
            return newlist   
            
        if(self.metric!="minkowski" and self.k==1):
            SimilarList = self.GetMaxDist(distance)
            MostSimilar = SimilarList[0]
            newlist=[]
            MostSimilarDict = self.usersItemRatings[MostSimilar]    
            MostSimilarL = list(MostSimilarDict.items())
            MostSimilarL.sort(key=lambda tup:tup[1], reverse=True) 
            for item in MostSimilarL:
                k,v = item
                if(k not in self.usersItemRatings[userX].keys()):
                    newlist.append(item)
            # return sorted list of recommendations (sorted highest to lowest ratings)
            return newlist   


        if((self.metric == "pearson" or self.metric == "cosine") and self.k > 1):    
            if(self.metric == 'pearson'):            
                for j in distancecopy.keys():
                    distancecopy[j] = (distancecopy[j] + 1)/2
            
                    
            MaxNumList = self.GetMaxNum(distancecopy,self.k)
            SimilarList = self.GetMultipleMaxDist(distancecopy1,self.k)
                
          
            # calcualte the weighted average item recommendations for userX from userX's k NNs            
            total_weight = 0;
            for i in range(self.k):
                total_weight += MaxNumList[i]
                
            
            for i in range(self.k):
                MaxNumList[i] = MaxNumList[i]/total_weight
                
            MostSimilarL = []
            for a in range(self.k):
                MostSimilar = SimilarList[a]
                MostSimilarDict = self.usersItemRatings[MostSimilar]
                
                
                for x in MostSimilarDict.keys():
                    MostSimilarDict[x] = MostSimilarDict[x] * MaxNumList[a]
    
                if(a==0):
                     CombinedDict = MostSimilarDict
                     
                else:
                    for x in MostSimilarDict.keys():
                        if(x in CombinedDict):
                            CombinedDict[x] = round(CombinedDict[x] + MostSimilarDict[x],2)
                        else:
                            CombinedDict[x] = round(MostSimilarDict[x],2)
                            
              
            newlist=[]
            CombinedDictL = list(CombinedDict.items())
            CombinedDictL.sort(key=lambda tup:tup[1], reverse=True) 
            for item in CombinedDictL:
                k,v = item
                if(k not in self.usersItemRatings[userX].keys()):
                    newlist.append(item)
            # return sorted list of recommendations (sorted highest to lowest ratings)
            return newlist       



        if(self.metric=='minkowski' and self.k > 1):
            MinNumList = self.GetMinNum(distancecopy2,self.k)
            SimilarList = self.GetMultipleMinDist(distancecopy3,self.k)

            for i in SimilarList:
                print(i)
    
            # calcualte the weighted average item recommendations for userX from userX's k NNs            
            total_weight = 0;
            for i in range(self.k):
                total_weight += MinNumList[i]
                
            for i in range(self.k):
                MinNumList[i] = round(MinNumList[i]/total_weight,2)
                
            MostSimilarL = []
            for a in range(self.k):
                MostSimilar = SimilarList[a]
                MostSimilarDict = self.usersItemRatings[MostSimilar]
                
                
                for x in MostSimilarDict.keys():
                    MostSimilarDict[x] = round(MostSimilarDict[x] * MinNumList[a],2)
    
                if(a==0):
                     CombinedDict = MostSimilarDict
                     
                else:
                    for x in MostSimilarDict.keys():
                        if(x in CombinedDict):
                            CombinedDict[x] = round(CombinedDict[x] + MostSimilarDict[x],2)
                        else:
                            CombinedDict[x] = round(MostSimilarDict[x],2)
                            
              
            newlist=[]
            CombinedDictL = list(CombinedDict.items())
            CombinedDictL.sort(key=lambda tup:tup[1], reverse=True) 
            for item in CombinedDictL:
                k,v = item
                if(k not in self.usersItemRatings[userX].keys()):
                    newlist.append(item)
            # return sorted list of recommendations (sorted highest to lowest ratings)
            return newlist       

























        
    
        
            
        
        


        
       
        
        
      


        

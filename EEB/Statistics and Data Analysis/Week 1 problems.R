#1. If we roll two dice (one white, one black)
    #a) how many different combinations are possible?
        6*6 #36
    #b) What is the probability of getting two sixes?
        1/6 * 1/6 #0.02777778 = 1/36
    #c) What is the probability of getting a two and a three
        (1/36) + (1/36) #0.05555556 (any specific outcome P = 1/36)
    #d) If we are interested in the sum of the numbers we get on the two dice, 
        #what sum has the highest probability?
      
      
      
#2. Your genome is composed of 4 nucleotides: A, C, G, T. Let’s assume that 
    #each of them is equally frequent. What is the probability that, at one
    #particular position of the genome, you find 3 bases in a row that are:
    #a) Exactly A T A?
        1/4 * 1/4 * 1/4 #Each nucleotide has a P of 1/4; 0.015625
    #b) Any combination of A and T (without any C or G)?
        2/4 * 2/4 * 2/4 #Each of the three positions we can have 2 different 
        #nucleotides; 0.125
    #c) What assumptions did you need to make? How realistic do you think they are?
        #That they all have equal frequencies in the genome + they are independent
        #of one another

#3. In a certain city there are 3 males for every 2 females. Only 1/3 of the males 
    #go to University, while for females, the proportion is 1/2
    #a) What proportion of the population go to university?
        #P(M total) = 3/5
        #P(F total) = 2/5
        
        #P(U total) = P(U|F)P(F) + P(U|M)P(M)
        (2/5)*(1/2) + (3/5)*(1/3) #0.4
    #b) Of those that go to University, what is the proportion of females?
        #P(F|U) =(P(F|U)P(F))/P(U) 
        (1/2)*(2/5)/(2/5) #0.5
        
#4. In cattle, the allele A1 is dominant and gives black coat colour. The A2 allele
    #is recessive and gives a red coat. This means that calves with genotype A1A1 or
    #A2A2 are black, and those with A2A2 are red. 
    #a) When crossing two heterozygous animals, what is the probability that one 
        #of their offspring will be red?
        1/2 * 1/2 #0.25
    #b) If two heterozygous animals have three offspring, what is the most probable 
        #number of red calves?
        (1/4)^3 #probability of three red calves; 0.015625
        (3/4)^3 #probability of three black calves; 0.421875
        (1/4) * (3/4)^2 * 3 #P of 1 red, 2 black; 0.421875
        (1/4)^2 * (3/4) * 3 #P of 2 red, 1 black; 0.140625
        #P of 0 and 1 red calf is the same, so 1 red calf is the answer
        
#5. A bag contains 3 red and 2 blue marbles. A random samples of two marbles is
    #taken from the bag, without replacement. What is the probability that:
    #a) Both are red?
        3/5 * 2/4 #0.3
    #b) There is one of each colour?
        #P(BR) = 2/5 * 3/4 (assuming we take blue first)
          #2 blue balls in total out of 5; and then 3 red remaining in 4 total
        #P(RB) = 3/5 * 2/4
        #Mutually exclusive, so we can sum them up
        2/5 * 3/4 + 3/5 * 2/4 #0.6
    #c) What are the answers is the balls are sampled with replacement?
        3/5 * 3/5 #0.36 (for a)
        2/5 * 3/5 + 3/5 * 2/5 #0.48 (for b)
        
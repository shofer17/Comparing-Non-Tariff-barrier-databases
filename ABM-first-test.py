number = 10
number_list = [10, 2, 20]

number * number_list

gruppe1 = []
gruppe2 = []


gruppe1.append(2)
gruppe1.append(4)
gruppe1.append(6)

gruppe2.extend([1,3,8])


gruppe1
gruppe2

avg1 = sum(gruppe1)/len(gruppe1)
avg2 = sum(gruppe2)/len(gruppe2)

gruppe1[0]
gruppe2[1] = gruppe2[1]*0.6 + gruppe2[0]*0.4
gruppe2

gruppe1.insert(0, gruppe2.pop(2))
gruppe1
gruppe2

gruppe1.extend(gruppe2)
population = gruppe1
population
first_five_agents = population[0:5]
first_five_agents
del(population)


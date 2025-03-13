from Date import Date

date1 = Date(1933, 6, 22)
print(date1)  

date2 = Date(1900, 1, 1)
date3 = Date(2000, 1, 1)
date4 = Date(1933, 6, 22)

print(date1.__gt__(date2))
print(date1.__le__(date3))
print(date1.__eq__(date4))

# print(date1 > date2)   
# print(date1 <= date3)  
# print(date1 == date4)  

dates = [date3, date1, date2, date4]
dates.sort()
print([str(d) for d in dates])  
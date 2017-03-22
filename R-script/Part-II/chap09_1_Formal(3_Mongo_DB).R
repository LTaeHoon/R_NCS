# Formal(3_Mongo_DB)

# Formal(3_Mongo_DB)
# - MongoDB(NoSQL) 데이터 처리


# 패키지 설치 
install.packages('RMongo')

# 패키지 로딩
library(rJava)
library(RMongo) # Loading required package: rJava
# MongoDB에 접속하기 위한 패키지 로더


####################################
# 1. MongoDB에서 특정 DB 연결 
####################################

# mongoDbConnect(dbName, host="127.0.0.1", port=27017)
conn <- mongoDbConnect('local',host="127.0.0.1", port=27017) # local DB 연결  


# 전체문서 조회 - dbGetQuery(conn, collection, query, skip=0, limit=1000) 
result <-dbGetQuery(conn, "startup_log", "") # startup_log 컬렉션 전체 검색 
result # starup_log 컬렉션에서 12개의 Document 검색

# DB 연결 해제 
dbDisconnect(conn)


##########################################
# 2. MongoDB에서 db/collection/document   
##########################################

# 1) DB 연결 객체 생성 
conn2 <- mongoDbConnect('user_db', '127.0.0.1') # user_db DB 생성 


# 2) Collection 객체 생성 
dbInsertDocument(conn2, 'user_coll', "{'name' : 'hong', 'age':35, 'gender':'man'}")
# [1] "ok"


# 3) document 조회 
result <-dbGetQuery(conn2, "user_coll", "")
result


# 4) Document 추가 
dbInsertDocument(conn2, 'user_coll', "{'name' : 'lee', 'age':45, 'gender':'man'}")
dbInsertDocument(conn2, 'user_coll', "{'name' : 'yoo', 'age':25, 'gender':'woman'}")

result <-dbGetQuery(conn2, "user_coll", "")
result

# 5) Document 삭제 
dbRemoveQuery(conn2, "user_coll", "{ 'name' : 'yoo'}")
dbGetQuery(conn2, "user_coll", "")

dbDisconnect(conn2)
